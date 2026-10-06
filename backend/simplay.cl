; Copyright (C) 2026  Bartosz "Lew" Pastudzki <lew@wiedz.net.pl>
 
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published
; by the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; MODULE: simplay.cl
;; aims to provide more accurate way of simulating playing deals than
;; current 'deal-play object in bridge.cl. At the same time it aims to
;; provide more informative output than standard minimax approach.
;; Instead, it starts from naive greedy trick taking and looks for possible
;; improvements for both sides.

;; (load "bridge.cl") <- assume it was loaded before

;; TOC:
;;  1. Basic helper functions
;;  2. Trickseq: generating one-suit trick sequences. Intentionally, missing
;;     cards are replaced with nulls so that they can be handled separately.
;;  3. Greedy tricks sequence – will create base for further optimization.


;; ====================================================
;; 1. Helpers:
;; ====================================================
(defun str2ranks* (str)
    (if (string= str "-") nil (str2ranks str)))

(defun str2split (rankstr)
    (mapcar #'str2ranks*
            (splitstr " " rankstr)))

(defvar ranksyms '(2 3 4 5 6 7 8 9 10 J Q K A))

(defun str2trick (str)
    (mapcar (f* #'read-from-string (curry* #'position (x) (x ranksyms)))
            (splitstr "-" str)))

(defun str2tricks (str)
    (mapcar #'str2trick (splitstr " " str)))
 
(defun joinstr (sep strs)
    (cond ((not strs) "")
          ((not (cdr strs)) (car strs))
          (t (format nil "~A~A~A" (car strs) sep (joinstr sep (cdr strs))))))

(defun rankstr (rank)
    (if rank (nth rank ranksyms) "x"))

(defun trickstr (trick)
    (joinstr "-" (mapcar #'rankstr trick)))

(defun suitsplitstr (split)
    (joinstr " " (mapcar (lambda (hand) 
                            (format nil "~{~A~}" (if hand (reverse (mapcar #'rankstr hand)) '("-"))))
                         split)))

(defun best (compare lst)
    (if lst
        (fold (lambda (acc val) (if (funcall compare val acc) val acc))
              (first lst)
              (rest lst))))

(test (best #'> '(1 -5 10 14 8 -100)) 14 eq)

(defun mod+ (denominator &rest operands)
    (mod (apply #'+ operands) denominator))

(defmacro stack-job (init feed &optional clean-up)
    `(let ((stack ,init)
           (results '()))
        (loop for x = (pop stack)
              while x 
              do (let ((more ,feed))
                    (if more (setf stack (append more stack))
                             (setf results (cons ,(if clean-up clean-up 'x) results)))))
        results))

(defun pass-if (test value)
    (if (funcall test value) value))

;; Class representing and alternative value
;; Basic building block for non-linear programming
;; (ie. we define many options and we decide aterwards)
(defclass alt ()
    ((opts :initarg := :reader opts)))

(defun alt (&rest options)
    (make-instance 'alt := options))

(defmethod permutation ((this alt) size)
    (make-instance 'alt := (apply #'list-prod (repeat size (opts this)))))

(defmethod combination ((this alt) &optional size)
    (with-slots (opts) this
        (if (not size) (setf size (length opts)))
        (make-instance 'alt :=
            (reverse (stack-job (loop for next in opts
                                      collect (list (list next) (remove next opts)))
                                (let-from* x (acc rest)
                                    (if (< (length acc) size)
                                        (loop for next in rest
                                              collect (list (cons next acc) (remove next rest)))))
                                (car x))))))
    
(defun alt-prod (&rest alts)
    (make-instance 'alt := (apply #'list-prod (mapcar #'opts alts))))

(defmethod map-alt (f (this alt))
    (make-instance 'alt := (mapcar f (opts this))))

(defmethod decide ((this alt) transform grade)
    (with-slots (opts) this
        (fold (lambda (acc val)
                 (if (eq (car acc) 1) acc
                     (let* ((new-val (funcall transform val))
                            (grade (funcall grade new-val)))
                        (if (or (not acc) (> grade (first acc)))
                            (list grade new-val)
                            acc))))
              nil
              opts)))
                        
(defun error-grade (tolerance ref val)
    (let ((deviation (abs (- ref val))))
        (if (> deviation tolerance) 0
            (- 1 (/ deviation tolerance)))))

(test (let ((coins (alt 5.0 2.0 1.0 0.5 0.2 0.1 0.05 0.02 0.01)))
         (decide (permutation coins 3)
                 #'id
                 (f* (curry #'apply #'+)
                     (curry #'error-grade 1 1.27))))
      '(0.98 (1.0 0.2 0.05))
      equal)

(test (decide (combination (alt 13 11 7 5 3) 2)
              (curry #'apply #'+)
              (curry #'error-grade 10 22))
      '(4/5 24) ;; 24, because we can't take 11 twice (combination)
      equal) 

(test (decide (alt-prod (alt 10 9 5) (alt 101 202 303) (alt 505 606 909))
              (curry #'apply #'+)
              (curry #'error-grade 10 1018))
      '(9/10 1019)
      equal)

;; ====================================================
;; 2. Trickseq
;; ====================================================
;; Structure made to store trick sequences in one suit,
;; missing cards possible, additional metadata stored:
;; leader: which player started first trick in seq
;; transfers: list of (trick-no . whom) pairs listing
;;            in which tricks, leader changes to whom
(defclass trickseq ()
    ((suit :initform nil :initarg :suit :reader suit)
     (tricks :initform nil :initarg :tricks :reader tricks)
     (leader :initform 0 :initarg :leader :reader leader)
     (transfers :initform nil :initarg :transfers :reader transfers)
     (remaining :initarg :remaining :reader remaining)))

(defun ranktrick-transfers (tricks &optional (leader 0))
    (loop for trick in tricks
          for trickno from 0
          append (let ((winner (car (car (sort (zip-id trick)
                                               (lambda (a b)
                                                  (> (or (second a) -1) (or (second b) -1))))))))
                    (if (> winner 0)
                        (let ((new-leader (mod+ 4 leader winner)))
                            (setf leader new-leader)
                            `((,trickno ,new-leader)))))))

(test (ranktrick-transfers (str2tricks "2-10-J-5 3-5-A-7 4-Q-K-x 9-x-8-x"))
      '((0 2) (1 0) (2 2))
      equal)

(test (ranktrick-transfers (str2tricks "A-x-5-2 K-x-4-3 4-x-Q-6 10-J-x-x") 3)
      '((2 1) (3 2))
      equal)
    
(defun trickseq (suit remaining &key tricks (leader 0))
    (make-instance 'trickseq
                   :suit suit
                   :tricks tricks
                   :leader leader
                   :transfers (reverse (ranktrick-transfers tricks leader))
                   :remaining remaining))

(defmethod print-object ((this trickseq) o)
    (format o "SEQ<~A~A ~A:~A>" (leader this) (suit-uc (slot-value this 'suit))
                             (joinstr " " (mapcar #'trickstr (tricks this)))
                             (suitsplitstr (remaining this))))

(defmethod len ((this trickseq))
    (length (tricks this)))

(defmethod eq? ((a trickseq) (b trickseq))
    (and (eq (slot-value a 'suit) (slot-value b 'suit))
         (equal (tricks a) (tricks b))
         (= (leader a) (leader b))
         (equal (transfers a) (transfers b))
         (equal (remaining a) (remaining b))))

(defmethod eq? ((a list) (b list))
    (not (filter #'not (mapcar #'eq? a b))))

(defmethod last-leader ((this trickseq))
    (or (second (car (transfers this)))
        (leader this)))

(defmethod continuation-by ((this trickseq) new-leader)
    (let ((shift (mod (- new-leader (last-leader this)) 4)))
        (trickseq (suit this) (roll (- shift) (remaining this))
                  :leader new-leader)))

(defmethod top-trick ((this trickseq) winner)
    ;; creates a sequence after winner took trick (others laid smallest)
    (with-slots (suit tricks leader transfers remaining) this
        (let-from! (apply (curry #'mapcar #'list)
                          (loop for player from 0 to 3
                                for cards in remaining
                                collect (if (= player winner)
                                            (let ((lastno (- (length cards) 1)))
                                                (list (nth lastno cards) (subseq cards 0 lastno)))
                                            (list (first cards) (rest cards)))))
                   (ranks rest)
            (make-instance 'trickseq 
                :suit suit
                :tricks (append tricks (list ranks))
                :leader leader
                :transfers (if (> winner 0) (cons (list (length tricks)
                                                        (mod+ 4 (or (second (car transfers))
                                                                    leader)
                                                                 winner)) 
                                                  transfers)
                                transfers)
                :remaining (roll (- winner) rest)))))

(test (top-trick (trickseq 'c (str2split "AJ106 Q4 K532 987")) 2)
      (trickseq 'c (str2split "532 98 AJ10 Q") :tricks (str2tricks "6-4-K-7"))
      eq?)

(defmethod highest-holders ((this trickseq))
    ;; Lists who holds highest cards as list of player ids starting from holder of highest
    (with-slots (remaining) this
        (mapcar #'car (sort (filter #'second 
                                    (zip-id (mapcar (f* #'last #'car) remaining)))
                            (lambda (a b) (> (second a) (second b)))))))

(defmethod top-trick-opts ((this trickseq))
    ;; Lists all possible top-tricks for current partnership.
    ;; Whoever sits at position 0 is the one leading this trick - if they
    ;; are void in the suit they cannot lead it, no matter who holds what.
    (with-slots (remaining) this
        (if (first remaining)
            (with-results ((highest-holders this))
                (if (and highest-holders (= (mod (first highest-holders) 2) 0)) ; we can take with highest
                    (cons (top-trick this (first highest-holders))
                          (if (and (second highest-holders) ; there may be no second holder at all
                                   (= (mod (second highest-holders) 2) 0) ; we hold also second highest
                                   ;; ... but only if holding back the first holder is actually
                                   ;; safe: their forced-lowest card must still lose to the
                                   ;; second holder's card, or this "duck" would win for real
                                   (< (first (nth (first highest-holders) remaining))
                                      (car (last (nth (second highest-holders) remaining)))))
                              (list (top-trick this (second highest-holders))))))))))
 
(test (top-trick-opts (trickseq 'c (str2split "AJ106 Q4 K532 987")))
      (list (trickseq 'c (str2split "J106 Q K53 98") :tricks (str2tricks "A-4-2-7"))
            (trickseq 'c (str2split "532 98 AJ10 Q") :tricks (str2tricks "6-4-K-7")))
      eq?)


(test (top-trick-opts (trickseq 'd (str2split "J106 - K532 Q98")))
      (list (trickseq 'd (str2split "532 Q9 J10 ") :tricks (str2tricks "6-x-K-8")))
      eq?)

(test (top-trick-opts (trickseq 'd (str2split "8 - 3 -")))
      (list (trickseq 'd '(nil nil nil nil) :tricks (str2tricks "8-x-3-x") ))
      eq?)

(test (top-trick-opts (trickseq 's '(nil nil nil nil)))
      nil
      equal)

;; leader is void, partner holds the outright highest card (A) - leader
;; still can't lead this suit, so there is no top-trick to take here
(test (top-trick-opts (trickseq 'd (str2split "- Q4 AK987 J106532")))
      nil
      equal)

(defmethod suited-tricks ((this trickseq))
    (with-slots (suit tricks) this
        (mapcar (curry #'mapcar (lambda (rank) (if rank (list suit rank))))
                tricks)))

(test (suited-tricks (first (top-trick-opts (trickseq 'd (str2split "J106 - K532 Q98")))))
      '(((D 4) NIL (D 11) (D 6)))
      equal)

(defmethod split ((this trickseq) at)
    (with-slots (suit tricks leader transfers remaining) this
        (flet ((reconstruct-remaining ()
                   (if (not transfers) remaining
                       (let ((result remaining)
                             (current-leader (second (car transfers))))
                          (loop for trick in (reverse tricks)
                                for trickno from (- (length tricks) 1) downto 0
                                while (>= trickno at)
                                do (let ((transfer-to (second (find-if (f* #'first (curry #'eq trickno))
                                                                       transfers))))
                                       (when transfer-to
                                            (setf result (roll (- transfer-to current-leader) result))
                                            (setf current-leader transfer-to))
                                       (setf result (loop for rank in trick
                                                          for hand in result
                                                          collect (if rank (cons rank hand) hand)))))
                          (mapcar (curry* #'sort (x) (x #'<))
                                  result)))))
            (list (make-instance 'trickseq
                                 :suit suit
                                 :tricks (subseq tricks 0 at)
                                 :leader leader
                                 :transfers (filter (f* #'first (curry #'> at))
                                                    transfers)
                                 :remaining (reconstruct-remaining))
                  (make-instance 'trickseq
                                 :suit suit
                                 :tricks (subseq tricks at)
                                 :leader (or (second (first (filter (f* #'first (curry #'> at))
                                                                    transfers)))
                                             leader)
                                 :transfers (mapcar (lambda* (pos to) (list (- pos at) to))
                                                    (filter (f* #'first (curry #'<= at))
                                                            transfers))
                                 :remaining remaining)))))
              
(test (split (trickseq 's (str2split "- - - -") :tricks (str2tricks "2-5-K-9 3-8-A-4 7-6-Q-x"))
             1)
      (list (trickseq 's (str2split "Q3 8 A7 64") :tricks (str2tricks "2-5-K-9"))
            (trickseq 's (str2split "- - - -") :tricks (str2tricks "3-8-A-4 7-6-Q-x") :leader 2))
      eq?)

;; ======================================================================
;; 3. Greedy tricks: lets figure out greedy play sequence for both sides
;;    and for further optimizations.
;; ======================================================================

(defmethod suit-tops ((this trickseq))
    ;; do top-tricks as long as you can to find all quick tricks in a suit
    (let ((stack (top-trick-opts this))
          (results '()))
       (loop for head = (pop stack)
             while head
             do (let ((more (top-trick-opts head)))
                   (if more (setf stack (append more stack))
                            (setf results (cons head results)))))
      results))
    
(test (suit-tops (trickseq 's (str2split "AJ106 94 K532 Q87")))
      (list (trickseq 's (str2split "J10 - 53 Q") :tricks (str2tricks "6-4-K-7 2-8-A-9"))
            (trickseq 's (str2split "53 Q J10 -") :tricks (str2tricks "A-4-2-7 6-9-K-8")))
      eq?)
      
;; previously produced this pair of candidates twice over (4 total) - one
;; path in each duplicate pair relied on the illegal-duck bug fixed above,
;; and happened to converge on the same terminal state as the legal path
(test (suit-tops (trickseq 'd (str2split "AJ106 974 K532 Q8")))
      (list (trickseq 'd '(nil nil nil nil) :tricks (str2tricks "6-4-K-8 2-Q-A-7 J-9-3-x 10-x-5-x"))
            (trickseq 'd '(nil nil nil nil) :tricks (str2tricks "A-4-2-8 6-7-K-Q 3-x-J-9 10-x-5-x")))
      eq?)

;; partner void from the start, so there is never a second holder to
;; check for - continuation must not crash trying to mod nil by 2
(test (suit-tops (trickseq 'd (str2split "AK - - -")))
      (list (trickseq 'd '(nil nil nil nil) :tricks (str2tricks "A-x-x-x K-x-x-x")))
      eq?)

;; when joining trickseqs in scenarios, one of central actions is to make a trickseq
;; be legal continuation of another one.
(defmethod adapt-leader ((this trickseq) (predecessor trickseq))
    (let ((trick-gap (mod (- (or (second (car (transfers predecessor)))
                                 (leader predecessor))
                             (leader this))
                          4)))
        (cond ((= trick-gap 0)
                  this)
              ((nth trick-gap (first (tricks this)))
                  (make-instance 'trickseq
                                 :suit (suit this)
                                 :tricks (cons (roll (- trick-gap) (car (tricks this)))
                                               (cdr (tricks this)))
                                 :leader (mod+ 4 (leader this) trick-gap)
                                 :remaining (remaining this)
                                 :transfers (let ((ltrans (car (last (transfers this)))))
                                                (if (eq (car ltrans) 0) ; changed trick was a transfer
                                                    (let ((base (subseq (transfers this) 0
                                                                        (- (length (transfers this)) 1))))
                                                        (if (= (second ltrans) (mod+ 4 (leader this) trick-gap))
                                                            base
                                                            (transfers this)))
                                                    (append (transfers this) `((0 ,(leader this)))))))))))

(test (adapt-leader (trickseq 's (str2split "Q 2 1097 -")
                                 :leader 0
                                 :tricks (str2tricks "5-8-J-4 6-3-K-A"))
                    (trickseq 'c (str2split "- - - -")
                                 :leader 0
                                 :tricks (str2tricks "2-10-x-A")))
      ;; We have to adapt the trick so that 3 is a leader instead of 0...
      (trickseq 's (str2split "Q 2 1097 -")
                   :leader 3
                   :tricks (str2tricks "4-5-8-J 6-3-K-A"))
      eq?)

(test (adapt-leader (trickseq 's (str2split "Q 2 1097 -")
                                 :leader 3
                                 :tricks (str2tricks "5-8-J-4 6-3-K-A"))
                    (trickseq 'c (str2split "- - - -")
                                 :leader 0
                                 :tricks (str2tricks "2-10-x-A")))
      ;; No need to adapt, because leader is equal to player who ended previous sequence
      (trickseq 's (str2split "Q 2 1097 -")
                   :leader 3
                   :tricks (str2tricks "5-8-J-4 6-3-K-A"))
      eq?)


(test (adapt-leader (trickseq 's (str2split "Q 2 1097 -")
                                 :leader 3
                                 :tricks (str2tricks "A-8-J-4 5-K-6-x"))
                    (trickseq 'c (str2split "- - - -")
                                 :leader 0
                                 :tricks (str2tricks "2-10-x-A")))
      (trickseq 's (str2split "Q 2 1097 -")
                   :leader 3
                   :tricks (str2tricks "A-8-J-4 5-K-6-x"))
      eq?)

(test (adapt-leader (trickseq 's (str2split "Q 2 1097 -")
                                 :leader 0
                                 :tricks (str2tricks "A-8-J-4 5-K-6-x"))
                    (trickseq 'c (str2split "- - - -")
                                 :leader 0
                                 :tricks (str2tricks "2-10-x-A")))
      (trickseq 's (str2split "Q 2 1097 -")
                   :leader 3
                   :tricks (str2tricks "4-A-8-J 5-K-6-x"))
      eq?)


(test (adapt-leader (trickseq 's (str2split "Q 2 1097 -")
                                 :leader 0
                                 :tricks (str2tricks "A-8-J-x 5-K-6-x"))
                    (trickseq 'c (str2split "- - - -")
                                 :leader 0
                                 :tricks (str2tricks "2-10-x-A")))
      ;; This can't be done, because it would create illegal trick starting from missing card
      nil
      eq)

;; Then we combine trickseqs into play-scenarios. First a naive greedy play from
;; both sides, then to apply optimizations. Note that the scenario can contain
;; more than 13 tricks. This is because then optimizations can be based on reordering
;; tricks and only in the end the whole scenario would be trimmed to 13-tricks limit
(defclass play-scenario ()
    ((phases :initarg :phases :reader phases)
     (remaining :initarg :remaining :initform '(nil nil nil nil) :reader remaining)))

(defmethod print-object ((this play-scenario) o)
    (format o "SCENARIO<~A:~A>" (phases this) (joinstr "/" (mapcar #'suitsplitstr (remaining this)))))

(defmethod len ((this play-scenario))
    (apply #'+ (mapcar #'len (phases this))))

(defmethod eq? ((a play-scenario) (b play-scenario))
    (and (eq? (phases a) (phases b))
         (equal (remaining a) (remaining b))))

;; To make sure that remaining slot contains correct values, sync is called when
;; new elements are added. Optional parameter is used to check only new parameters.
;; Without, whole phases slot is processed. 
(defmethod sync ((this play-scenario) &optional new-phases)
    (with-slots (phases remaining) this
        (loop for p in (or new-phases phases)
              do (setf (nth (suitno (suit p)) remaining)
                       (roll (leader p) (remaining p))))
        this))

(defmethod last-leader ((this play-scenario))
    (last-leader (car (last (phases this)))))

;; Sometimes we can't simply concatenate tricks into scenario, but
;; we can split a trick into two and put another one in the middle
(defmethod inject ((base trickseq) (injectee trickseq))
    (let ((inject-point (first (first (filter (f* #'second (curry #'eq (leader injectee)))
                                              (transfers base))))))
        (if inject-point
            (let-from! (split base (+ inject-point 1))
                       (before after)
               (let ((after* (adapt-leader after injectee)))
                  (if after* (sync (make-instance 'play-scenario
                                                  :phases (list before injectee after*)))))))))


;; The most basic way to create play-scenario is to combine 2 trickseqs.
;; Sometimes we combine with no-trick seq to insert remaining splits in
;; suits not included in scenario. In such case we only call sync.
(defmethod combine ((a trickseq) (b trickseq) &optional _)
    (declare (ignore _)) ; parameter for compability with one in rest.cl
    (if (tricks b)
        (let ((b* (adapt-leader b a)))
            (if b* (sync (make-instance 'play-scenario :phases (list a b*)))
                   (inject a b)))
        (sync (make-instance 'play-scenario :phases (list a))
              (list a b))))

    
(defmethod combine ((a play-scenario) (b trickseq) &optional _)
    ;so much code because this method supports not only adding to the end of scenario,
    ;but wherever possible. This needs finding where last trick in given suit was
    ;and checking for place where a sequence can be adapted with neighbours.

    (declare (ignore _)) ; parameter for compability with one in rest.cl
    (labels ((fit (before &optional after)
                (if before (let* ((link1 (combine (car (last before)) b))
                                  (before* (subseq before 0 (- (length before) 1))))
                               (cond ((not link1) (fit before* (cons (car (last before)) after)))
                                     ((or (not after) (= (len link1) 3) )
                                         (append before* (phases link1) after))
                                     (t (let ((link2 (combine link1 (car after))))
                                         (if link2 (append before* (phases link2) (cdr after))
                                                   (fit before* (cons (car (last before)) after)))))))))
             (earliest-point (seqs)
                (let ((revpos (position-if (f* #'suit (curry #'eq (suit b)))
                                           (reverse seqs))))
                    (if (not revpos) (list nil seqs)
                                     (let ((pos (- (length seqs) revpos)))
                                        (list (subseq seqs 0 pos) (subseq seqs pos)))))))
        (if (not (tricks b))
            (sync a (list b))
            (with-slots (phases remaining) a
                (let-from! (earliest-point phases)
                           (unavailable insertable)
                    (if insertable
                        (let ((inserted (fit insertable)))
                            (if inserted (sync (make-instance 'play-scenario 
                                                        :phases (append unavailable inserted)
                                                        :remaining remaining)
                                         (list b))))))))))

(defmethod combine ((a play-scenario) (b play-scenario) &optional _)
    (declare (ignore _))
    (labels ((trial-loop (phases &optional (left (length phases)))
                (if (> left 0)
                    (let ((link (combine a (car phases))))
                       (if link (sync (make-instance 'play-scenario
                                                     :phases (append (phases link) (cdr phases)))
                                      (cdr phases))
                                (let ((b* (make-instance 'play-scenario :phases (cdr phases))))
                                    (trial-loop (or (combine b (car phases)) b*)
                                                (- left 1))))))))
       (trial-loop (phases b))))
       
(test (phases (combine (trickseq 's (str2split "- - - -")
                            :tricks (str2tricks "2-6-K-10 3-9-A-8"))
                       (trickseq 'c (str2split "- - - -")
                            :leader 2
                            :tricks (str2tricks "A-x-x-x K-x-x-x Q-x-x-x"))))
      ;; Can't combine by concatenation. Source sequence must be split to fit other one
      (list (trickseq 's (str2split "3 9 A 8") :tricks (str2tricks "2-6-K-10"))
            (trickseq 'c (str2split "- - - -") :leader 2 :tricks (str2tricks "A-x-x-x K-x-x-x Q-x-x-x"))
            (trickseq 's (str2split "- - - -") :leader 2 :tricks (str2tricks "3-9-A-8")))
      eq?)

(test (phases (reduce #'combine (list (trickseq 'h (str2split "- - - -")
                                            :tricks (str2tricks "5-10-A-4"))
                                      (trickseq 's (str2split "- - - -")
                                            :leader 2
                                            :tricks (str2tricks "2-6-A-10 K-9-5-8"))
                                      (trickseq 'c (str2split "- - - -")
                                            :leader 2
                                            :tricks (str2tricks "A-x-x-x K-x-x-x Q-x-x-x")))))
       ;; Here testing play-scenario variant where a new trick must be inserted in the middle of scenario
       (list (trickseq 'h (str2split "- - - -")
                       :tricks (str2tricks "5-10-A-4"))
             (trickseq 'c (str2split "- - - -") :leader 2 :tricks (str2tricks "A-x-x-x K-x-x-x Q-x-x-x"))
             (trickseq 's (str2split "- - - -") :leader 2 :tricks (str2tricks "2-6-A-10 K-9-5-8")))

       eq?)


;; Now we want to compose suit-tops into greedy play for current partnership
;; and current leader. Deal can be list of hands or play-scenario.
(defun take-tops (deal)
    (labels ((append-partner-continuation (seqs)
                 (let ((partner-id (mod+ 4 (leader (car seqs)) 2)))
                   (append seqs
                           (apply #'append (mapcar (f* (curry* #'continuation-by (x) (x partner-id))
                                                   #'suit-tops)
                                           (filter (f** #'remaining #'car #'length (curry #'= 0))
                                                   seqs))))))
             (prioritize-exhausted (seqs)
                 ;; We want to start from suits that we exhaust on leader hand to
                 ;; make sure that we don't miss taking continuation from partners hand
                 (sort seqs (lambda (a b)
                                (declare (ignore b))
                                (and (not (transfers a)) (not (first (remaining a)))))))
             (combine-or-same (a b)
                (let ((ans (combine a b)))
                   (cond (ans ans)
                         ((typep a 'play-scenario) a)
                         (t (make-instance 'play-scenario :phases (list a))))))
             (splits-for-scenario (deal leader)
                (mapcar (curry* #'trickseq (suit cards) 
                                (suit cards :leader (mod+ 4 (last-leader deal) leader)))
                        '(c d h s) 
                        (mapcar (curry #'roll leader) (remaining deal))))
             (tops (suit-splits)
                (let* ((seq-per-suit (mapcar (orf #'suit-tops #'list) suit-splits))
                       (seqalt (map-alt (f* #'prioritize-exhausted #'append-partner-continuation)
                                        (apply #'alt-prod 
                                               (mapcar (curry #'apply #'alt) 
                                                        seq-per-suit))))
                       (top-len (apply #'max (mapcar (f* (curry #'mapcar #'len)
                                                     (curry #'apply #'+))
                                                     (opts seqalt)))))
                    (second (decide seqalt
                            (curry #'reduce #'combine-or-same)
                            (f* #'len (curry #'error-grade 13 top-len)))))))
        (if (typep deal 'play-scenario)
            (or (tops (peek (splits-for-scenario deal 1)))
                (tops (peek (splits-for-scenario deal 3))))
            (tops (mapcar (lambda (suit) (trickseq suit (mapcar (curry* #'hand-suit (h) (h suit))
                                                                deal)))
                          '(c d h s))))))
                
(test (take-tops (str2deal "N: ♠ K ♥ AQ8765 ♦ K9 ♣ A1093
                            E: ♠ J2 ♥ 102 ♦ 107654 ♣ 8652
                            S: ♠ AQ1094 ♥ 93 ♦ Q32 ♣ KJ4
                            W: ♠ 87653 ♥ KJ4 ♦ AJ8 ♣ Q7"))
      (sync (make-instance 'play-scenario :phases 
                (list (trickseq 's (str2split "- J AQ109 8765") :tricks (str2tricks "K-2-4-3"))
                      (trickseq 'c (str2split "- - 10 8") :tricks (str2tricks "3-2-K-7 4-Q-A-5 9-6-J-x"))
                      (trickseq 's (str2split "- - - -") :leader 2
                                   :tricks (str2tricks "A-5-x-J Q-6-x-x 10-7-x-x 9-8-x-x"))
                      (trickseq 'h (str2split "Q8765 10 9 KJ") :leader 2 :tricks (str2tricks "3-4-A-2"))
                      (trickseq 'c (str2split "- - - -") :tricks (str2tricks "10-8-x-x")))))
      eq?)

(test (take-tops (str2deal "N: ♠ 1063 ♥ KQ64 ♦ A94 ♣ Q53
                            E: ♠ K ♥ 10987 ♦ K1063 ♣ KJ74
                            S: ♠ Q872 ♥ 53 ♦ J2 ♣ A10986
                            W: ♠ AJ954 ♥ AJ2 ♦ Q875 ♣ 2"))
      (make-instance 'play-scenario
                :phases (list (trickseq 'c (str2split "10986 - Q5 KJ7") :tricks (str2tricks "3-4-A-2"))
                              (trickseq 'd (str2split "94 K106 J Q87") :tricks (str2tricks "2-5-A-3")
                                                                       :leader 2))
                :remaining (list (str2split "10986 - Q5 KJ7")
                                 (str2split "J Q87 94 K106")
                                 (str2split "KQ64 10987 53 AJ2")
                                 (str2split "1063 K Q872 AJ954")))
      eq?)

(defun greedy-deal (deal)
    (labels ((process (scenario)
                (if (filter #'id (remaining scenario))
                    (let ((continuation (pass-if (lambda (s)
                                                    (or (not s)
                                                        (second (phases s))
                                                        (tricks (first (phases s)))))
                                                 (take-tops scenario))))
                        (if continuation (process (combine scenario continuation))
                                         scenario))
                    scenario)))
       (process (take-tops deal))))

(greedy-deal (str2deal "N: ♠ 1063 ♥ KQ64 ♦ A94 ♣ Q53
                        E: ♠ K ♥ 10987 ♦ K1063 ♣ KJ74
                        S: ♠ Q872 ♥ 53 ♦ J2 ♣ A10986
                        W: ♠ AJ954 ♥ AJ2 ♦ Q875 ♣ 2"))
