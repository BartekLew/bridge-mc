; Copyright (C) 2025  Bartosz "Lew" Pastudzki <lew@wiedz.net.pl>
 
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

; One compilation unit, so calls to functions defined further on don't warn
(with-compilation-unit ()
    (load "bridge.cl")
    (load "compscore.cl")
    (load "training.cl")
    (load "grading.cl"))
(ql:quickload '(:clack :clack-cors :jonathan :flexi-streams))

(defclass table ()
    ((users :initform '() :reader users)
     (mode :initform 'init)
     (lock :initform nil)
     (cond :initform nil)
     (hands :initform '(nil nil nil nil))
     (auction :initform nil)))

(defvar *table* (make-instance 'table))

(defmethod sitting-resp ((table table))
    (with-slots (users) table
        (if (= (length users) 4) `(:status ok
                                   :ready ,users)
                                 `(:status ok
                                   :waiting ,users))))

(defmethod sit ((table table) name)
    (with-slots (users lock cond) table
        (if (not lock) (setf lock (bt:make-lock)))
        (if (not cond) (setf cond (bt:make-condition-variable)))

        (let ((ans (or (find name users :test #'equal)
                       (and (< (length users) 4)
                            (bt:with-lock-held (lock)
                                (setf users (cons name users))
                                (when (= (length users) 4)
                                      (setf users (second (deal 4 users))))
                                (sb-thread:condition-broadcast cond)))
                                T)))
           (if ans (sitting-resp table) 
                   '(:status rejected)))))

(defmethod setting ((table table))
    (with-slots (users lock cond) table
        (if (not lock) (setf lock (bt:make-lock)))
        (if (not cond) (setf cond (bt:make-condition-variable)))

        (bt:with-lock-held (lock)
            (if (not (= (length users) 4)) (bt:condition-wait cond lock :timeout 20))
            (sitting-resp table))))

(defmethod for-uid ((table table) user function)
    (with-slots (users) table
        (let ((uid (position user users :test #'equal)))
            (if uid (funcall function uid)
                    `(:status error
                      :message (format nil "User not logged in: ~a" user))))))

(defmethod add-hand ((table table) user hand)
    (with-slots (users hands cond lock) table
        (if (= (length hands) 4)
            (for-uid table user (lambda (uid)
                                    (setf (nth uid hands) (str2hand hand))
                                    (if (not (position nil hands))
                                        (bt:with-lock-held (lock)
                                           (sb-thread:condition-broadcast cond)))
                                    `(:status ok)))
            `(:status error
              :message "Bad state. Not all players are there."))))

(defmethod auction ((table table) user since)
    (with-slots (users auction lock cond) table
       (bt:with-lock-held (lock)
          (for-uid table user 
                   (lambda (uid)
                      (if (and (<= since (length auction))
                          (not (= (mod (length auction) 4) uid)))
                        (bt:condition-wait cond lock :timeout 20))
                      (if (= (mod (length auction) 4) uid)
                          `(:your-bid ,auction)
                          `(:others-bid ,auction)))))))

(defun read-body (env)
  (let ((body (getf env :raw-body)))
    (etypecase body
      (string body)
      (stream (let ((stream (flexi-streams:make-flexi-stream body :external-format :utf-8)))
                (with-output-to-string (out)
                    (loop for line = (read-line stream nil)
                          while line
                          do (write-line line out))))))))

(defun suitstr (suit)
    (if (not suit) "NT"
        (let ((suitno (if (numberp suit) suit (position suit '(C D H S)))))
            (nth suitno '("♣" "♦" "♥" "♠")))))

(defun sim-axis (hands)
    (let ((trump (apply #'best-suit (mapcar #'suits (reorder hands '(0 2))))))
        (fold (lambda (acc val)
                (let-from* val (suit roll outcome)
                   (if (or (not acc)
                           (> (second (score outcome))
                              (third acc)))
                       (list suit (handid (nth roll hands)) (second (score outcome)) outcome)
                       acc)))
              nil
              (mapcar (lambda* (suit roll)
                         (list suit roll (apply #'play-deal (cons suit (roll roll hands)))))
                      (list-prod `(nil ,trump) '(0 2))))))

(defun suitcmp (a b)
    (> (or (suitno a) 4)
       (or (suitno b) 4)))

;; Splitting play into short sequences of tricks to make it easier to follow.

; Numbers of tricks in consecutive sequences; the last one takes the rest
(defparameter *sequence-sizes* '(5 5 3))

; Cut points (numbers of tricks played so far) dividing n tricks by sizes
(defun sequence-cuts (sizes n)
    (let ((sum 0))
        (append (loop for size in (butlast sizes)
                      while (< (+ sum size) n)
                      collect (incf sum size))
                (list n))))

(test (sequence-cuts '(5 5 3) 13) '(5 10 13) equal)
(test (sequence-cuts '(5 5 3) 7) '(5 7) equal)

; Tricks of outcome grouped into sequences of *sequence-sizes* tricks, each
; with its own summary. Winners are positions of trick winners relative to
; trick leader. Hands are in order: declarer, LHO, dummy, RHO.
(defun play-sequences (outcome suit hands)
    (loop for start = 0 then end
          for end in (sequence-cuts *sequence-sizes* (length (tricks outcome)))
          collect `(:tricks ,(mapcar (curry #'mapcar #'cardstr)
                                     (subseq (tricks outcome) start end))
                    :winners ,(subseq (winner-nos outcome) start end)
                    :comment ,(format nil "~A"
                                  (apply #'summarize-outcome
                                         (append (list outcome suit) hands
                                                 (list :start start :end end :show-trump nil)))))))

; Scores of the side winning the auction and of the other one competing
; over it, given (suit declarer tricks) of both sides' contracts
(defun deal-score (a b)
    (let ((winner (cond ((> (third a) (third b)) 0)
                        ((= (third a) (third b)) (if (suitcmp (first a) (first b)) 0 1))
                        (t 1))))
       (let-from! (nth winner (list a b)) (wsuit wdec wtricks)
          (declare (ignore wdec))
          (let-from! (nth (- 1 winner) (list a b)) (dsuit ddec dtricks)
             (declare (ignore ddec))
             (let ((wscore (contract-score wsuit wtricks)))
                (list wscore
                      (contract-score dsuit dtricks :level (if (suitcmp dsuit wsuit)
                                                               (- wtricks 6)
                                                               (- wtricks 5))
                                                    :double (>= wscore 300))))))))

(defparameter *sims-dir* (merge-pathnames "sims/" (uiop:getcwd)))

(defun sim-path (id &optional (dir *sims-dir*))
    (merge-pathnames (format nil "~A.json" id) dir))

(defun valid-sim-id? (id)
    (and (> (length id) 0)
         (every (lambda (c) (or (alphanumericp c) (char= c #\-))) id)))

(defun new-sim-id ()
    (format nil "~(~36R~36R~)" (get-universal-time) (random (expt 36 3))))

(defun write-sim (id sim &optional (dir *sims-dir*))
    (ensure-directories-exist dir)
    (with-open-file (out (sim-path id dir) :direction :output :if-exists :supersede
                                           :external-format :utf-8)
        (write-string (jonathan:to-json sim) out)))

(defun save-sim (hands result)
    (let ((id (new-sim-id)))
        (write-sim id `(:hands ,hands :result ,result))
        id))

(defun load-sim (id &optional (dir *sims-dir*))
    (if (valid-sim-id? id)
        (with-open-file (in (sim-path id dir) :if-does-not-exist nil :external-format :utf-8)
            (if in (let ((str (make-string (file-length in))))
                      (subseq str 0 (read-sequence str in)))))))

;; Editing play of saved simulations: tricks are removed from the end of
;; play or added there. Sequences cut or made by the user have no comment;
;; edited axes are marked EDITED.

(define-condition edit-error (simple-error) ())

(defun edit-error (fmt &rest args)
    (error 'edit-error :format-control fmt :format-arguments args))

(defparameter *seats* '("N" "E" "S" "W"))

(defun str2card (str)
    (and (stringp str) (> (length str) 1)
         (let ((suit (str2suitno (subseq str 0 1)))
               (rank (position (subseq str 1) '("2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A")
                               :test #'equal)))
            (and suit rank (list suit rank)))))

; Hands of the deal by seat (N E S W) from hands of saved sim
(defun sim-hands (formulas)
    (let ((hands (mapcar #'str2hand formulas)))
        (mapcar #'suits (if (= (length hands) 4) hands
                            (append hands (list (make-instance 'hand :remaining hands)))))))

; Computed hand has no proper id; it's always the last one (W)
(defun declarer-seat (axis)
    (or (position (getf axis :|DECLARER|) *seats* :test #'equal) 3))

(defun axis-tricks (axis)
    (loop for seq in (getf axis :|SEQUENCES|) append (getf seq :|TRICKS|)))

; Hands left by seat, seat to lead and tricks taken by declarer's side after
; tricks of axis are played
(defun play-state (hands axis)
    (let* ((trump (str2suitno (getf axis :|TRUMP|)))
           (declarer (declarer-seat axis))
           (leader (mod (+ declarer 1) 4))
           (taken 0))
        (loop for trick in (axis-tricks axis)
              for cards = (mapcar #'str2card trick)
              for winner = (mod (+ leader (trick-winner cards trump)) 4)
              do (setf hands (play-trick hands leader cards))
                 (setf leader winner)
                 (if (evenp (- winner declarer)) (incf taken)))
        (values hands leader taken)))

(defun play-done? (hands axis)
    (every #'null (apply #'append (play-state hands axis))))

(defun plain-seq (tricks winners)
    `(:|TRICKS| ,tricks :|WINNERS| ,winners))

; Sequences without the last trick; empty ones at the end are dropped
(defun pop-trick (seqs)
    (let ((seqs (loop with ans = (reverse seqs)
                      while (and ans (not (getf (car ans) :|TRICKS|)))
                      do (pop ans)
                      finally (return (reverse ans)))))
        (if (not seqs) (edit-error "No tricks to remove"))
        (let* ((last-seq (car (last seqs)))
               (tricks (butlast (getf last-seq :|TRICKS|))))
            (append (butlast seqs)
                    (if tricks (list (plain-seq tricks (butlast (getf last-seq :|WINNERS|)))))))))

(defun push-trick (seqs trick winner)
    (let ((last-seq (car (last seqs))))
        (if last-seq
            (append (butlast seqs)
                    (list (plain-seq (append (getf last-seq :|TRICKS|) (list trick))
                                     (append (getf last-seq :|WINNERS|) (list winner)))))
            (list (plain-seq (list trick) (list winner))))))

(defun split-seqs (seqs)
    (if (or (not seqs) (not (getf (car (last seqs)) :|TRICKS|)))
        (edit-error "Next tricks go to a new sequence already"))
    (append seqs (list (plain-seq nil nil))))

; (suit declarer tricks) of axis as deal-score takes it
(defun axis-contract (axis)
    (let ((suit (str2suitno (getf axis :|TRUMP|))))
        (list (and suit (nth suit '(c d h s)))
              (getf axis :|DECLARER|)
              (getf axis :|SCORE|))))

; Score of the deal (deal-score) played in the axes of power, when they are
; one contract of each side (sides, 0 for N-S, by their declarers unless
; given), else nil
(defun power-score (power &optional (sides (mapcar (lambda (axis) (mod (declarer-seat axis) 2)) power)))
    (if (equal (sort (copy-list sides) #'<) '(0 1))
        (apply #'deal-score (mapcar #'axis-contract power))))

; Play of hands in trump (suit number or nil) by seat, as sim-axis gives it
(defun contract-axis (hands trump seat)
    (let* ((side-hands (roll (mod seat 2) hands))
           (inner (- seat (mod seat 2)))
           (suit (and trump (nth trump '(c d h s))))
           (outcome (apply #'play-deal suit (roll inner side-hands))))
        (list suit (nth seat *seats*) (second (score outcome)) outcome)))

; Plays defhands in the contracts of axes, (trump declarer) suit and seat
; numbers, or else in the best one of each side (sim-axis)
(defun sim-deal (defhands &optional axes)
    (let* ((hands (if (= (length defhands) 4)
                      defhands (append defhands
                                       (list (make-instance 'hand :remaining defhands)))))
           (results (if axes
                        (loop for (trump seat) in axes
                              collect (cons (mod seat 2) (contract-axis hands trump seat)))
                        (list (cons 0 (sim-axis hands)) (cons 1 (sim-axis (roll 1 hands))))))
           (power (mapcar (lambda* (roll suit declarer tricks outcome)
                             `(:trump ,(suitstr suit)
                               :declarer ,declarer
                               :score ,tricks
                               :sequences ,(play-sequences outcome suit (roll roll hands))))
                          results))
           (score (power-score power (mapcar #'car results))))
       `(,@(and score `(:score ,score)) :power ,power)))

(defparameter *plan-types* '(:draw :draw-all :establish :duck :cash :ruff :void :cross :endplay :force :play :exit))

(defun plan-json (plan)
    (destructuring-bind (type suit seat) plan
        `(:type ,(string-downcase type)
          :suit ,(if suit (suitstr suit) :null)
          :seat ,(if seat (nth seat *seats*) :null))))

(defun json-plan (obj)
    (let ((type (find (getf obj :|type|) *plan-types* :test #'string-equal))
          (suit (getf obj :|suit|))
          (seat (getf obj :|seat|)))
        (list (or type (edit-error "Bad plan ~A" obj))
              (and (stringp suit) (str2suitno suit))
              (and (stringp seat) (position seat *seats* :test #'equal)))))

(defun plan-label (plan)
    (destructuring-bind (type suit seat) plan
        (let ((suit (and suit (suitstr suit)))
              (seat (and seat (nth seat *seats*))))
            (ecase type
                (:draw (format nil "Draw trumps~@[, ending in ~A~]" seat))
                (:draw-all (format nil "Draw all trumps~@[, ending in ~A~]" seat))
                (:establish (format nil "Establish ~A~@[, ending in ~A~]" suit seat))
                (:duck (format nil "Duck ~A~@[, ending in ~A~]" suit seat))
                (:cash (format nil "Cash ~A~@[, ending in ~A~]" suit seat))
                (:ruff (format nil "Ruff ~A in ~A" suit seat))
                (:void (format nil "Void ~A in ~A" suit seat))
                (:cross (format nil "Cross to ~A" seat))
                (:endplay (format nil "Throw in ~A with ~A" seat suit))
                (:force (format nil "Force ~A with ~A" seat suit))
                (:play (format nil "Play ~A~@[, ending in ~A~]" suit seat))
                (:exit (format nil "Exit with ~A" suit))))))

; Sequences with tricks of plan added as a new one commented with its label.
; Next tricks go to a new sequence again, unless all cards are played.
(defun push-plan (seqs plan tricks trump done)
    (append (loop for seq in seqs when (getf seq :|TRICKS|) collect seq)
            (list `(:|TRICKS| ,(mapcar (lambda (tr) (mapcar #'cardstr tr)) tricks)
                    :|WINNERS| ,(mapcar (lambda (tr) (trick-winner tr trump)) tricks)
                    :|COMMENT| ,(plan-label plan)))
            (if (not done) (list (plain-seq nil nil)))))

; Choice of a plan variant (plan tricks), leader leading its first trick
(defun line-choice (hands leader trump variant)
    (destructuring-bind (plan tricks) variant
        `(:plan ,(plan-json plan)
          :label ,(plan-label plan)
          :tricks ,(loop for tr in tricks
                         collect `(:trick ,(mapcar #'cardstr tr)
                                   :winner ,(trick-winner tr trump)
                                   :leader ,(nth leader *seats*))
                         do (setf hands (play-trick hands leader tr)
                                  leader (trick-taker tr leader trump))))))

; Lines (plan tricks) of the hint search (hint-play) from the end of the
; play of axis axis-no of saved sim id, for action complete of sim-edit:
; searched before it, out of *sim-lock*, as it can take minutes
(defun sim-complete-lines (id axis-no)
    (let* ((sim (jonathan:parse (or (load-sim id) (edit-error "No simulation ~A" id))))
           (power (getf (getf sim :|RESULT|) :|POWER|))
           (axis (if (and (integerp axis-no) (< -1 axis-no (length power)))
                     (nth axis-no power)
                     (edit-error "Bad axis ~A" axis-no))))
        (multiple-value-bind (left leader) (play-state (sim-hands (getf sim :|HANDS|)) axis)
            (if (every #'null (apply #'append left))
                (edit-error "All cards are played"))
            (nth-value 1 (hint-play left leader (str2suitno (getf axis :|TRUMP|)))))))

; Applies action (pop, clear, split, play, trick, plan or hints) to play of
; axis axis-no of saved sim id. Returns (:result ...) with the updated
; result, which gets saved, (:choices ...) of tricks to choose from or
; (:hints ...) of plans of the side on lead, sim left intact. Play adds a
; trick taken by card, trick adds the explicit one, or the tricks of a line
; (as ones of plan, when given), plan the tricks of plan. A plan with
; several variants (see plan-variants) gives them as choices instead, each a
; line of tricks with its plan and label. Complete plays the rest of the
; deal by the hint search, a sequence for each plan, lines being its result
; when searched already (sim-complete-lines).
(defun sim-edit (id axis-no action card trick &optional plan tricks lines)
    (let* ((sim (jonathan:parse (or (load-sim id) (edit-error "No simulation ~A" id))))
           (result (getf sim :|RESULT|))
           (power (getf result :|POWER|))
           (axis (if (and (integerp axis-no) (< -1 axis-no (length power)))
                     (nth axis-no power)
                     (edit-error "Bad axis ~A" axis-no)))
           (hands (sim-hands (getf sim :|HANDS|)))
           (trump (str2suitno (getf axis :|TRUMP|)))
           (seqs (getf axis :|SEQUENCES|)))
        (multiple-value-bind (left leader) (play-state hands axis)
            (labels ((check-open ()
                        (if (every #'null (apply #'append left))
                            (edit-error "All cards are played")))
                     (push-cards (cards)
                        (push-trick seqs (mapcar #'cardstr cards) (trick-winner cards trump)))
                     (push-plan-tricks (p tricks)
                        (push-plan seqs p tricks trump
                                   (= (* 4 (length tricks))
                                      (length (apply #'append (apply #'append left))))))
                     (choices (tricks &rest more)
                        (return-from sim-edit
                           `(:choices ,(mapcar (lambda (tr)
                                                  `(:trick ,(mapcar #'cardstr tr)
                                                    :winner ,(trick-winner tr trump)))
                                               tricks)
                             :leader ,(nth leader *seats*)
                             ,@more))))
                (let ((new-seqs
                        (cond ((equal action "pop") (pop-trick seqs))
                              ((equal action "clear") nil)
                              ((equal action "split") (check-open) (split-seqs seqs))
                              ((equal action "hints")
                                 (return-from sim-edit
                                    `(:hints ,(loop for (p variants) in (plan-hints left leader trump)
                                                    collect `(:plan ,(plan-json p)
                                                              :label ,(plan-label p)
                                                              :tricks ,(reduce #'max variants
                                                                               :key (lambda (v) (length (second v))))
                                                              :variants ,(length variants))))))
                              ((equal action "plan")
                                 (check-open)
                                 (let* ((p (json-plan plan))
                                        (variants (or (second (find p (plan-hints left leader trump)
                                                                    :key #'first :test #'equal))
                                                      (plan-variants left leader trump p))))
                                    (cond ((null variants)
                                             (edit-error "~A gives no trick now" (plan-label p)))
                                          ((rest variants)
                                             (return-from sim-edit
                                                `(:choices ,(mapcar (lambda (v) (line-choice left leader trump v))
                                                                    variants)
                                                  :leader ,(nth leader *seats*)
                                                  :plan ,(plan-json p)
                                                  :label ,(plan-label p))))
                                          (t (push-plan-tricks (first (first variants)) (second (first variants)))))))
                              ((equal action "trick")
                                 (check-open)
                                 (let ((lines (mapcar (lambda (tr) (mapcar #'str2card tr)) (or tricks (list trick))))
                                       (h left)
                                       (l leader))
                                    (dolist (cards lines)
                                        (if (not (legal-trick? h l cards))
                                            (edit-error "Trick ~A can't be played now" (mapcar #'cardstr cards)))
                                        (setf h (play-trick h l cards)
                                              l (trick-taker cards l trump)))
                                    (if plan
                                        (push-plan-tricks (json-plan plan) lines)
                                        (push-cards (first lines)))))
                              ((equal action "complete")
                                 (check-open)
                                 (let ((ls (or lines (nth-value 1 (hint-play left leader trump))))
                                       (h left)
                                       (l leader)
                                       (s seqs))
                                    (loop for (p trs) in ls
                                          do (dolist (cards trs)
                                                 (if (not (legal-trick? h l cards))
                                                     (edit-error "The play changed while the hint search ran"))
                                                 (setf h (play-trick h l cards)
                                                       l (trick-taker cards l trump)))
                                             (setf s (push-plan s p trs trump t)))
                                    (if (some #'identity (apply #'append h))
                                        (edit-error "The hint search didn't play all cards"))
                                    s))
                              ((equal action "play")
                                 (check-open)
                                 (let* ((c (or (str2card card) (edit-error "Bad card ~A" card)))
                                        (seat (or (position-if (lambda (h) (member (second c) (nth (first c) h)))
                                                               left)
                                                  (edit-error "Card ~A is played already" card))))
                                    (multiple-value-bind (tricks takes) (card-tricks left leader trump seat c)
                                       (cond ((not tricks)
                                                (edit-error "Card ~A can't be played to this trick" card))
                                             ((and takes (= (length tricks) 1))
                                                (push-cards (first tricks)))
                                             (t (choices tricks :takes (if takes t :false)))))))
                              (t (edit-error "Unknown action ~A" action)))))
                    (setf (getf axis :|SEQUENCES|) new-seqs)
                    (setf (getf axis :|EDITED|) t)
                    (setf (getf axis :|SCORE|) (nth-value 2 (play-state hands axis)))
                    (setf (nth axis-no power) axis)
                    (let ((score (and (every (curry #'play-done? hands) power) (power-score power))))
                        (if score (setf (getf result :|SCORE|) score)))
                    (write-sim id sim)
                    `(:result ,result))))))

;; Inspecting saved simulations from the REPL. Output of box isn't readable
;; reliably, so with-probe sends what body prints to a file:
;;   (with-probe () (sim-show "1u6omxnq4c"))
;;   (with-probe () (pos-show (sim-pos "1u6omxnq4c" 0)))   ; after all tricks
;;   (with-probe () (pos-show (sim-pos "1u6omxnq4c" 0 2))) ; after 2 tricks

(defparameter *probe-file* (merge-pathnames "probe.out" (uiop:getcwd)))

(defmacro with-probe ((&optional (file '*probe-file*)) &body body)
    `(with-open-file (o ,file :direction :output :if-exists :supersede :external-format :utf-8)
        (handler-case (let ((*standard-output* o)) ,@body (values))
            (error (e) (format o "ERROR ~A~%" e)))))

(defun ranks-str (ranks)
    (format nil "~{~A~}" (mapcar (lambda (r) (nth r '(2 3 4 5 6 7 8 9 10 J Q K A))) (reverse ranks))))

; Hand of seat as in test comments: N ♠ 6 ♥ A94 ♦ 9 ♣ Q643
(defun hand-str (hands seat)
    (format nil "~A~{ ~A~}" (nth seat *seats*)
            (loop for suit from 3 downto 0
                  for ranks = (nth suit (nth seat hands))
                  when ranks collect (format nil "~A ~A" (suitstr suit) (ranks-str ranks)))))

; Prints tricks numbered from n, leader leading the first; returns the
; seat to lead after them
(defun print-tricks (tricks leader trump &optional (n 1) (indent "  "))
    (loop for tr in tricks
          for taker = (trick-taker tr leader trump)
          do (format t "~A~2D. ~A: ~{~A~^ ~} > ~A~%" indent n (nth leader *seats*)
                     (mapcar #'cardstr tr) (nth taker *seats*))
             (setf leader taker)
             (incf n))
    leader)

; Hands of the deal and the axes (POWER) of saved sim id
(defun sim-axes (id)
    (let ((sim (jonathan:parse (or (load-sim id) (error "No simulation ~A" id)))))
        (values (sim-hands (getf sim :|HANDS|)) (getf (getf sim :|RESULT|) :|POWER|))))

; Prints the deal and play of each axis of sim id, by sequences
(defun sim-show (id)
    (multiple-value-bind (hands power) (sim-axes id)
        (dotimes (seat 4) (format t "~A~%" (hand-str hands seat)))
        (loop for axis in power
              for i from 0
              for trump = (str2suitno (getf axis :|TRUMP|))
              for leader = (mod (+ (declarer-seat axis) 1) 4)
              for n = 1
              do (format t "~%Axis ~D: ~A by ~A, score ~A~:[~;, edited~]~%" i (suitstr trump)
                         (getf axis :|DECLARER|) (getf axis :|SCORE|) (getf axis :|EDITED|))
                 (dolist (seq (getf axis :|SEQUENCES|))
                     (let ((tricks (mapcar (lambda (tr) (mapcar #'str2card tr)) (getf seq :|TRICKS|))))
                         (format t "  -- ~A~%" (or (getf seq :|COMMENT|) ""))
                         (setf leader (print-tricks tricks leader trump n))
                         (incf n (length tricks))))
                 (format t "  ~A to lead~%" (nth leader *seats*)))))

; Position of axis axis-no of sim id after n of its tricks (all when nil):
; (hands leader trump)
(defun sim-pos (id axis-no &optional n)
    (multiple-value-bind (hands power) (sim-axes id)
        (let ((axis (or (nth axis-no power) (error "Bad axis ~A" axis-no))))
            (multiple-value-bind (left leader)
                    (play-state hands (if n `(:|SEQUENCES| ((:|TRICKS| ,(subseq (axis-tricks axis) 0 n))) ,@axis) axis))
                (list left leader (str2suitno (getf axis :|TRUMP|)))))))

; Prints position (hands leader trump) as in test comments and as a literal,
; then its hints, each variant with its plan and tricks
(defun pos-show (pos)
    (destructuring-bind (hands leader trump) pos
        (format t "~A trumps, ~A on lead~%" (suitstr trump) (nth leader *seats*))
        (dotimes (seat 4) (format t "~A~%" (hand-str hands seat)))
        (format t "~S ~D ~S~%" hands leader trump)
        (loop for (plan variants) in (plan-hints hands leader trump)
              do (format t "~%~A ~S~%" (plan-label plan) plan)
                 (loop for (v tricks) in variants
                       do (format t "  ~S~%" v)
                          (print-tricks tricks leader trump 1 "    ")))))

; Hand objects N E S W of formulas of a saved sim, W the rest when missing
(defun formula-hands (formulas)
    (let ((hs (mapcar #'str2hand formulas)))
        (if (= (length hs) 4) hs (append hs (list (make-instance 'hand :remaining hs))))))

; memo, hint-search's, is for one axis only, as its keys leave out trumps
(defun hint-deal-1 (formulas power save timeout &optional memo)
    (let* ((objs (formula-hands formulas))
           (hands (mapcar #'suits objs)))
        (dotimes (seat 4) (format t "~A~%" (hand-str hands seat)))
        (let ((axes (loop for (trump declarer) in power
                          for i from 0
                          for leader = (mod (+ declarer 1) 4)
                          collect (let* ((t0 (get-internal-real-time))
                                         (old (second (score (apply #'play-deal (and trump (nth trump '(c d h s)))
                                                                    (roll declarer objs)))))
                                         (t1 (get-internal-real-time))
                                         (*hint-deadline* (+ t1 (floor (* timeout internal-time-units-per-second)
                                                                       (length power))))
                                         (*hint-nodes* 0)
                                         (*hint-hits* 0)
                                         (*hint-cuts* 0)
                                         (*hint-time* 0)
                                         (*hint-best* nil)
                                         (*hint-profile* (make-hash-table :test 'equal))
                                         (*solve-hits* 0)
                                         (*solve-misses* 0)
                                         (done t))
                                      (hint-log "axis ~D: ~A by ~A, play-deal took ~D" i (suitstr trump)
                                                (nth declarer *seats*) old)
                                      (multiple-value-bind (ns lines)
                                              (handler-case (if memo
                                                                (hint-play hands leader trump memo)
                                                                (hint-play hands leader trump))
                                                  (hint-timeout ()
                                                      (setf done nil)
                                                      (values (car *hint-best*) (cdr *hint-best*))))
                                          (hint-log "axis ~D ~:[timed out~;done~]: ~D nodes, ~D hits, ~D cuts, ~,1Fs in hints"
                                                    i done *hint-nodes* *hint-hits* *hint-cuts* *hint-time*)
                                          (dolist (line (profile-lines)) (hint-log "  ~A" line))
                                          (let ((tricks (and ns (if (evenp declarer) ns (- 13 ns))))
                                                (t2 (get-internal-real-time)))
                                              (format t "~%Axis ~D: ~A by ~A: play-deal ~D (~,1Fs), hints ~:[best so far ~;~]~A (~,1Fs, ~D nodes, ~D hits, ~D cuts)~%"
                                                      i (suitstr trump) (nth declarer *seats*) old
                                                      (/ (- t1 t0) internal-time-units-per-second) done (or tricks "none")
                                                      (/ (- t2 t1) internal-time-units-per-second)
                                                      *hint-nodes* *hint-hits* *hint-cuts*)
                                              (let ((l leader) (n 1))
                                                  (loop for (plan tricks) in lines
                                                        do (format t "  -- ~A~%" (plan-label plan))
                                                           (setf l (print-tricks tricks l trump n))
                                                           (incf n (length tricks))))
                                              `(:trump ,(suitstr trump)
                                                :declarer ,(nth declarer *seats*)
                                                :score ,(or tricks 0)
                                                :lines ,lines
                                                :summary (,trump ,declarer ,old ,tricks ,done
                                                          ,(round (- t2 t1) internal-time-units-per-second)
                                                          ,*hint-nodes*))))))))
            (let ((result `(,@(let ((score (power-score axes))) (and score `(:score ,score)))
                            :power ,(loop for axis in axes
                                          for trump = (str2suitno (getf axis :trump))
                                          collect `(:trump ,(getf axis :trump)
                                                    :declarer ,(getf axis :declarer)
                                                    :score ,(getf axis :score)
                                                    :sequences ,(loop for (plan tricks) in (getf axis :lines)
                                                                      append (push-plan nil plan tricks trump t)))))))
                (when save
                    (write-sim save `(:hands ,formulas :result ,result)))
                (values (mapcar (lambda (axis) (getf axis :summary)) axes) result)))))

; Plays the deal of formulas by hints (hint-play) in each axis (trump
; declarer), suit and seat numbers, printing its tricks against those of
; play-deal and the hint line. With save, an id, writes the deal played so
; as sim save, each sequence a plan. The search logs to log; the axes share
; timeout seconds equally, an axis timing out giving the best line found so
; far. Returns per axis (trump declarer play-deal-tricks tricks done
; seconds nodes), and the result as sim-deal gives it.
(defun hint-deal (formulas axes &key save (timeout 600) (log "hint-play.log"))
    (with-open-file (*hint-log* (merge-pathnames log (uiop:getcwd)) :direction :output
                                :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
        (let ((*hint-log-time* (get-internal-real-time)))
            (hint-deal-1 formulas axes save timeout))))

; Plays each axis of sim id by hints (hint-deal). With save, writes the
; deal played so as a new sim and returns its id.
(defun hint-sim (id &key save (timeout 600))
    (let ((sim (jonathan:parse (or (load-sim id) (error "No simulation ~A" id)))))
        (hint-deal (getf sim :|HANDS|)
                   (loop for axis in (getf (getf sim :|RESULT|) :|POWER|)
                         collect (list (str2suitno (getf axis :|TRUMP|)) (declarer-seat axis)))
                   :save (and save (new-sim-id)) :timeout timeout)))

;; Play training (training.cl): a random deal bid by the computer, played
;; by the user from S (and N when declaring) against the computer. Games in
;; progress are saved to training/<id>.json, out of reach of GET
;; /api/simdeal, as they hold the hidden hands. A finished one becomes sim
;; <id> with the user's play, play-deal's and, when the hint search is done,
;; the hint line (PENDING till then).

(defparameter *training-dir* (merge-pathnames "training/" (uiop:getcwd)))
(defparameter *training-roles* '("declarer" "leader" "partner" "defender"))
(defparameter *training-hint-timeout* 60)

; Sim files are written by edits and the hint search thread. The searches
; after games take turns, so that they don't all run at once (the caches
; they share are synchronized, the computer's play doesn't wait for them).
(defvar *sim-lock* (sb-thread:make-mutex :name "sim files"))
(defvar *hint-lock* (sb-thread:make-mutex :name "hint search"))

(defun call-str (call)
    (cond ((null call) "Pass")
          ((eq call 'x) "X")
          ((eq call 'xx) "XX")
          (t (format nil "~A~A" (first call) (suitstr (normsuit (second call)))))))

; Formula of seat's hand as the frontend makes it: N: ♠ AK9 ♥  ♦ A8 ♣ KJ96
(defun hand-formula (hands seat)
    (format nil "~A: ~{~A~^ ~}" (nth seat *seats*)
            (loop for suit from 3 downto 0
                  collect (format nil "~A ~A" (suitstr suit) (ranks-str (nth suit (nth seat hands)))))))

; Cards of a hand, spades first, high ones first
(defun hand-cards (suits)
    (loop for s from 3 downto 0
          append (mapcar (lambda (r) (cardstr (list s r))) (reverse (nth s suits)))))

;; Graded bidding debugger (grading.html)
; A hand's measures, its opening by the legacy rules (choose-bid) and by the
; scheme (graded-scheme), and the grade of each bid of the scheme with its
; rule, as texts gives it (in scheme order), and the rule's trace (grade-trace)
(defun grade-view (hand scheme texts)
    (let ((m (hand-measures hand)))
        `(:hand ,(hand-formula (list (suits hand)) 0)
          :measures ,(loop for (key . val) in m
                           append (list (intern (string key) :keyword) val))
          :legacy ,(call-str (choose-bid hand))
          :best ,(call-str (best-graded m scheme))
          :grades ,(loop for (bid . grade) in (graded-bids m scheme)
                         for (nil . rule) in scheme
                         for text in texts
                         collect `(:bid ,(call-str bid) :grade ,(float grade) :rule ,text
                                   :trace ,(grade-trace rule m))))))

; Saves a rule edited in the debugger, {bid, rule} (bid as call-str, rule as
; Lisp text), to the scheme file
(defun save-edited-rule (edit)
    (let* ((name (getf edit :|bid|))
           (entry (or (find name (scheme-file-entries) :key (lambda (e) (call-str (first e)))
                                                        :test #'string=)
                      (edit-error "No bid ~A in ~A" name *scheme-file*))))
        (handler-case (save-rule (first entry) (getf edit :|rule|))
            (error (e) (edit-error "Bad rule for ~A: ~A" name e)))))

; Deals for the debugger's ranges too tight for random hands (def-hand),
; kept by def: a deal makes its generator once, slowly (seconds), so
; batches for the same ranges reuse it
(defvar *grade-deals* (make-hash-table :test #'equal :synchronized t))

(defun grade-deal (def)
    (or (gethash def *grade-deals*)
        (setf (gethash def *grade-deals*)
              (handler-case (make-instance 'deal :~ (list (normdef (read-from-string def))))
                  (error (e) (edit-error "Bad ranges ~A: ~A" def e))))))

; POST /api/grade with a hand as HandEditor's restDef gives it: {hand} the
; hand's grade-view, {def, count?} (ranges) {HANDS} of count hands (10, at
; most 100) dealt for them,
; {hands} (formulas) {HANDS} of these hands, graded again. An optional rule,
; {bid, rule}, is saved to the scheme file first; given alone, the answer is
; {SAVED: true}.
(defun grade-request (request)
    (let ((hand (getf request :|hand|))
          (hands (getf request :|hands|))
          (def (getf request :|def|))
          (edit (getf request :|rule|)))
        (when edit (save-edited-rule edit))
        (let* ((entries (handler-case (scheme-file-entries)
                          (error (e) (edit-error "Cannot read ~A: ~A" *scheme-file* e))))
               (scheme (mapcar (lambda (e) (cons (first e) (second e))) entries))
               (str (file-string *scheme-file*))
               (texts (mapcar (lambda (e) (subseq str (third e) (fourth e))) entries)))
        (flet ((read-hand (str) (handler-case (str2hand str)
                                  (error (e) (edit-error "Bad hand ~A: ~A" str e))))
               (grade-view (hand) (grade-view hand scheme texts)))
            (cond (hand (grade-view (read-hand hand)))
                  (hands `(:hands ,(mapcar (lambda (h) (grade-view (read-hand h))) hands)))
                  (def (let ((ranges (handler-case (normdef (read-from-string def))
                                         (error (e) (edit-error "Bad ranges ~A: ~A" def e)))))
                         `(:hands ,(loop repeat (min 100 (or (getf request :|count|) 10))
                                         collect (grade-view
                                                  (or (def-hand ranges)
                                                      (first (handler-case (build (grade-deal def))
                                                                 (error (e) (edit-error "Cannot deal ~A: ~A" def e))))))))))
                  (edit '(:saved t))
                  (t (edit-error "Neither a hand nor ranges given")))))))

;; Measuring the graded openings in the debugger: a job deals count random
;; hands in a thread and keeps each with its graded bid and measures, so the
;; page can ask for the statistics while it runs and list a bid's hands.
(defstruct gjob
    total (done 0) scheme (stop nil) (state "running") (error nil)
    (records (make-array 1000 :adjustable t :fill-pointer 0))   ; (bid hand measures)
    (lock (sb-thread:make-mutex :name "grade job")))

(defvar *grade-jobs* (make-hash-table :test #'equal :synchronized t))
(defvar *grade-job-count* 0)

; Measures in the statistics, in the order shown
(defparameter *gjob-measures*
    '(hcp losers long second longpower balanced C D H S Cpower Dpower Hpower Spower))

(defun gjob-start (count)
    (let* ((count (max 1 (min 1000000 (or count 10000))))
           (job (make-gjob :total count :scheme (handler-case (graded-scheme)
                                                  (error (e) (edit-error "Cannot read ~A: ~A" *scheme-file* e)))))
           (id (princ-to-string (incf *grade-job-count*))))
        (setf (gethash id *grade-jobs*) job)
        (remhash (princ-to-string (- *grade-job-count* 5)) *grade-jobs*)
        (sb-thread:make-thread
         (lambda ()
             (handler-case
                 (loop until (or (gjob-stop job) (>= (gjob-done job) count))
                       do (dolist (hand (random-deal))
                              (when (< (gjob-done job) count)
                                  (let ((m (hand-measures hand)))
                                      (sb-thread:with-mutex ((gjob-lock job))
                                          (vector-push-extend (list (best-graded m (gjob-scheme job)) hand m)
                                                              (gjob-records job))
                                          (incf (gjob-done job)))))))
               (error (e) (setf (gjob-error job) (princ-to-string e))))
             (setf (gjob-state job) (if (gjob-stop job) "stopped" "done")))
         :name "grade measure")
        `(:id ,id)))

(defun gjob-records-now (job)
    (sb-thread:with-mutex ((gjob-lock job))
        (copy-seq (gjob-records job))))

; Statistics of each bid of the scheme chosen by some hand: its count and
; share, and for each measure its least, greatest and mean value and the
; share of each value
(defun gjob-stats (job)
    (let* ((records (gjob-records-now job))
           (n (length records))
           (by-bid (make-hash-table :test #'equal)))
        (loop for r across records do (push (third r) (gethash (first r) by-bid)))
        `(:state ,(gjob-state job) :done ,n :total ,(gjob-total job)
          ,@(and (gjob-error job) `(:error ,(gjob-error job)))
          :bids ,(loop for (bid) in (gjob-scheme job)
                       for ms = (gethash bid by-bid)
                       when ms
                       collect `(:bid ,(call-str bid) :count ,(length ms) :share ,(float (/ (length ms) n))
                                 :measures ,(loop for key in *gjob-measures*
                                                  collect (measure-stats key ms)))))))

(defun measure-stats (key measures)
    (let ((counts (make-hash-table)) (sum 0) (lo nil) (hi nil) (n (length measures)))
        (dolist (m measures)
            (let ((v (cdr (assoc key m))))
                (incf (gethash v counts 0))
                (incf sum v)
                (setf lo (if lo (min lo v) v) hi (if hi (max hi v) v))))
        `(:name ,(if (= 1 (length (string key))) (string key) (string-downcase key))
          :min ,lo :max ,hi :mean ,(float (/ sum n))
          :probs ,(sort (loop for v being the hash-keys of counts using (hash-value c)
                              collect (list v (float (/ c n))))
                        #'< :key #'first))))

; Hands of the job graded bid (call-str, or null for all hands) for which
; filter, a grade expression (text, empty for none), grades 1 or more: their
; count and the first limit of them
(defun gjob-hands (job bid filter &optional (limit 200))
    (let* ((test (and filter (string/= "" (string-trim '(#\Space #\Tab #\Newline) filter))
                      (handler-case (parse-rule filter)
                          (error (e) (edit-error "Bad filter: ~A" e)))))
           (matches (loop for (b hand m) across (gjob-records-now job)
                          when (and (or (null bid) (string= bid (call-str b)))
                                    (or (null test) (>= (grade test m) 1)))
                          collect (list b hand m))))
        `(:count ,(length matches)
          :hands ,(loop for (b hand m) in matches
                        repeat limit
                        collect `(:hand ,(hand-formula (list (suits hand)) 0) :best ,(call-str b)
                                  :hcp ,(cdr (assoc 'hcp m)) :losers ,(cdr (assoc 'losers m)))))))

(defun gjob-request (path request)
    ; path after /api/grade/measure: "" starts, "/<id>" stats, "/<id>/hands", "/<id>/stop"
    (if (string= path "")
        (gjob-start (getf request :|count|))
        (let* ((parts (loop for start = 1 then (1+ end)
                            for end = (position #\/ path :start start)
                            collect (subseq path start end)
                            while end))
               (job (or (gethash (first parts) *grade-jobs*)
                        (edit-error "No measuring job ~A" (first parts)))))
            (cond ((null (second parts)) (gjob-stats job))
                  ((string= (second parts) "hands")
                   (gjob-hands job (getf request :|bid|) (getf request :|filter|)))
                  ((string= (second parts) "stop") (setf (gjob-stop job) t) '(:stopping t))
                  (t (edit-error "Unknown action ~A" (second parts)))))))

(defun load-training (id)
    (let ((str (load-sim id *training-dir*)))
        (and str (jonathan:parse str))))

; Game as saved: (hands declarer trump mine cards)
(defun training-game (game)
    (let ((contract (getf game :contract)))
        (list (sim-hands (getf game :hands))
              (position (getf contract :declarer) *seats* :test #'equal)
              (str2suitno (getf contract :trump))
              (training-seats (if (equal (getf game :role) "declarer") :declarer :defender))
              (mapcar #'str2card (getf game :cards)))))

;; Boards: games are numbered as in a match, the number giving the dealer
;; and who is vulnerable once the deal is turned (N-S being the player's
;; side; board-dealer, board-vulnerable). The last number handed out is
;; kept in training/board.txt.
(defun board-file ()
    (merge-pathnames "board.txt" *training-dir*))

(defun last-board ()
    (or (ignore-errors (with-open-file (in (board-file)) (parse-integer (read-line in)))) 0))

(defun save-last-board (n)
    (ensure-directories-exist *training-dir*)
    (with-open-file (out (board-file) :direction :output :if-exists :supersede)
        (format out "~D~%" n)))

(defun seat-vulnerable? (vul seat)
    (or (equal vul "Both") (equal vul (if (evenp seat) "NS" "EW"))))

; Declarer's score in game for taken tricks
(defun training-score (game taken)
    (let* ((contract (getf game :contract))
           (declarer (position (getf contract :declarer) *seats* :test #'equal)))
        (contract-score (suitsym (str2suitno (getf contract :trump))) taken
                        :level (getf contract :level)
                        :vulnerable (seat-vulnerable? (getf game :vulnerable) declarer)
                        :double (cdr (assoc (getf contract :doubled) '(("X" . x) ("XX" . xx)) :test #'equal)))))

; 4♠ by S vulnerable making ten tricks; 3NT doubled by E vulnerable two
; down, N-S vulnerable not mattering
(test (list (training-score '(:vulnerable "NS" :contract (:level 4 :trump "♠" :declarer "S" :doubled "")) 10)
            (training-score '(:vulnerable "Both" :contract (:level 3 :trump "NT" :declarer "E" :doubled "X")) 7)
            (training-score '(:vulnerable "NS" :contract (:level 3 :trump "NT" :declarer "E" :doubled "X")) 7))
      '(620 -500 -300)
      equal)

; What the player sees of game id: auction and contract, the hands of his
; seats and dummy's once the opening lead is made (all at the end), tricks
; done and the one going on, the seat to play next and tricks taken by
; declarer's side and by the defence. A finished game has the deal, the id
; of its sim and declarer's score.
(defun training-view (id game)
    (destructuring-bind (hands declarer trump mine cards) (training-game game)
        (multiple-value-bind (left done trick leader) (training-position hands declarer trump cards)
            (let ((over (= (length cards) 52))
                  (taken (count-if (lambda (d) (evenp (- (third d) declarer))) done)))
                `(:id ,id
                  :role ,(getf game :role)
                  :dealer ,(getf game :dealer)
                  :board ,(or (getf game :board) :null)
                  :claimed ,(or (getf game :claimed) :null)
                  :vulnerable ,(or (getf game :vulnerable) "None")
                  ,@(and over `(:score ,(training-score game taken)))
                  :auction ,(getf game :auction)
                  :contract ,(getf game :contract)
                  :mine ,(mapcar (lambda (s) (nth s *seats*)) mine)
                  :hands ,(loop for seat from 0 below 4
                                append (list (intern (nth seat *seats*) :keyword)
                                             (if (or over (member seat mine)
                                                     (and cards (= seat (mod (+ declarer 2) 4))))
                                                 (hand-cards (nth seat left))
                                                 :null)))
                  :counts ,(loop for seat from 0 below 4
                                 append (list (intern (nth seat *seats*) :keyword)
                                              (length (apply #'append (nth seat left)))))
                  :tricks ,(loop for (tr l w) in done
                                 collect `(:cards ,(mapcar #'cardstr tr)
                                           :leader ,(nth l *seats*)
                                           :winner ,(nth w *seats*)))
                  :trick (:cards ,(mapcar #'cardstr trick) :leader ,(nth leader *seats*))
                  :next ,(if over :null (nth (mod (+ leader (length trick)) 4) *seats*))
                  :taken (,taken ,(- (length done) taken))
                  ,@(and over `(:deal ,(getf game :hands) :sim ,(getf game :sim))))))))

; Axis of a training sim with the play of outcome, hands from declarer's
(defun training-axis (label outcome trump declarer hands tricks)
    `(:label ,label
      :trump ,(suitstr trump)
      :declarer ,(nth declarer *seats*)
      :score ,tricks
      :sequences ,(play-sequences outcome trump hands)))

; Plays the contract (trump declarer) of the deal of formulas by hints in a
; thread, adding the line to the axes of sim id when done; memo is the
; search's of the computer's play in the game
(defun training-hints (id formulas trump declarer &optional memo)
    (sb-thread:make-thread
        (lambda ()
            (let ((axis (handler-case
                            (sb-thread:with-mutex (*hint-lock*)
                                (let ((*standard-output* (make-broadcast-stream)))
                                    (first (getf (nth-value 1 (hint-deal-1 formulas (list (list trump declarer))
                                                                           nil *training-hint-timeout* memo))
                                                 :power))))
                          (error (e) (format t "~&[ERROR] hint search of ~A: ~A~%" id e) nil))))
                (sb-thread:with-mutex (*sim-lock*)
                    (let* ((sim (jonathan:parse (load-sim id)))
                           (result (getf sim :result)))
                        (remf result :pending)
                        (if axis
                            (setf (getf result :power)
                                  (append (getf result :power) (list `(:label "Hint search" ,@axis)))))
                        (setf (getf sim :result) result)
                        (write-sim id sim)))))
        :name "training hints"))

; Sim of a finished game: the player's play (edited), play-deal's, and the
; hint search's to come
(defun training-sim (game hands declarer trump cards &optional memo)
    (let* ((objs (roll (- declarer) (mapcar (lambda (s) (make-instance 'hand := (mapcar #'copy-list s)))
                                            hands)))
           (computer (apply #'play-deal (suitsym trump) objs))
           (done (nth-value 1 (training-position hands declarer trump cards)))
           (id (save-sim (getf game :hands)
                         `(:pending t
                           :power ((:edited t
                                    ,@(training-axis "Your play" (training-outcome hands declarer trump cards)
                                                     trump declarer objs
                                                     (count-if (lambda (d) (evenp (- (third d) declarer)))
                                                               done)))
                                   ,(training-axis "Computer" computer trump declarer objs
                                                   (second (score computer))))))))
        (training-hints id (getf game :hands) trump declarer memo)
        id))

; hint-search memos of games in progress by id, as (memo . last used,
; universal time), so that the computer's play by hints and the hint search
; after the game reuse what it found. A game whose memo is gone gets a new
; one, searching again.
(defvar *training-memos* (make-hash-table :test 'equal :synchronized t))

; Positions the memos may hold in all (a position takes about 455 bytes, a
; game's memo 1600 to 40000 of them); past it, the least recently used ones
; are dropped (trim-training-memos). Memos of games in the queue don't count.
(defparameter *training-memo-budget* 300000)

; Memo of game id, made when it has none, its use noted
(defun training-memo (id)
    (let ((e (gethash id *training-memos*)))
        (if e
            (progn (setf (cdr e) (get-universal-time)) (car e))
            (let ((memo (make-hash-table :test 'equal)))
                (setf (gethash id *training-memos*) (cons memo (get-universal-time)))
                memo))))

; Drops the least recently used memos, but that of game keep, till they hold
; at most *training-memo-budget* positions
(defun trim-training-memos (keep)
    (sb-ext:with-locked-hash-table (*training-memos*)
        (let* ((others (sort (loop for id being the hash-keys of *training-memos* using (hash-value e)
                                   unless (equal id keep)
                                   collect (list id (cdr e) (hash-table-count (car e))))
                             #'< :key #'second))
               (total (+ (loop for o in others sum (third o))
                         (let ((e (gethash keep *training-memos*))) (if e (hash-table-count (car e)) 0)))))
            (loop for (id nil n) in others
                  while (> total *training-memo-budget*)
                  do (remhash id *training-memos*)
                     (decf total n)))))

; Over the budget of 8 positions, the memo used longest ago goes, the
; current game's staying though not used since
(test (let ((*training-memos* (make-hash-table :test 'equal))
            (*training-memo-budget* 8))
          (flet ((memo (id n time)
                     (let ((m (make-hash-table)))
                         (dotimes (i n) (setf (gethash i m) t))
                         (setf (gethash id *training-memos*) (cons m time)))))
              (memo "old" 3 1)
              (memo "newer" 3 2)
              (memo "current" 4 0)
              (trim-training-memos "current")
              (sort (loop for id being the hash-keys of *training-memos* collect id) #'string<)))
      '("current" "newer")
      equal)

; Game id with cards played, then the computer's till the player is to play
; again (with claim, all the rest, his seats too); saved, and made a sim
; when finished. Returns training-view.
(defun training-advance (id game cards &optional claim)
    (destructuring-bind (hands declarer trump mine played) (training-game game)
        (declare (ignore played))
        (let* ((memo (training-memo id))
               (cards (computer-plays hands declarer trump cards (if claim nil mine) memo)))
            (trim-training-memos id)
            (setf (getf game :cards) (mapcar #'cardstr cards))
            (when (= (length cards) 52)
                (remhash id *training-memos*)
                (setf (getf game :sim) (training-sim game hands declarer trump cards memo)))
            (write-sim id game *training-dir*)
            (training-view id game))))

; Generator of training deals of defs as /api/mc takes them (nil for random
; deals) and how many deals to try: the deal itself when its hands are given
; (three at least), else random ones fitting the defs
(defun training-generator (defs)
    (let ((defs (match (hand-or-def-parser defs) :error (lambda (e) (edit-error "Bad hands: ~A" e)))))
        (cond ((null defs) (list (lambda () (build full-random)) nil))
              ((and (>= (length defs) 3) (every (lambda (d) (eq (type-of d) 'hand)) defs))
                 (let ((hands (if (= (length defs) 4)
                                  defs (append defs (list (make-instance 'hand :remaining defs))))))
                     (list (lambda () hands) 1)))
              (t (destructuring-bind (d order) (defs-generator defs)
                     (list (lambda () (reorder (build d) order)) 1000))))))

; New game of the player in role (see *training-roles*) on a deal of defs
; (see training-generator) as board number board, its dealer and
; vulnerability (dealt again till the board's dealer deals; fixed hands may
; have any of them deal, in case they are passed out or have the wrong
; declarer), prepared up to the player's first card, the computer's cards
; before it played by hints: (id game memo), not saved yet
(defun training-prepare (role defs board)
    (if (not (member role *training-roles* :test #'equal))
        (edit-error "Bad role ~A" role))
    (destructuring-bind (hands dealer bids (level trump declarer doubled))
            (or (destructuring-bind (generator tries)
                        (handler-case (training-generator defs)
                            (edit-error (e) (error e))
                            (error (e) (edit-error "Bad hands: ~A" e)))
                    (handler-case (training-deal (intern (string-upcase role) :keyword) generator tries
                                                 (board-dealer board) (eql tries 1))
                        (error (e) (edit-error "Cannot deal these hands: ~A" e))))
                (edit-error "These hands are passed out, or don't give you this role with ~A dealing"
                            (nth (board-dealer board) *seats*)))
        (let ((memo (make-hash-table :test 'equal))
              ; copied, as the constant tail of the backquote (:cards nil) is
              ; one list shared by all games, and :cards is set below
              (game (copy-tree
                     `(:hands ,(loop for seat from 0 below 4 collect (hand-formula hands seat))
                      :role ,role
                      :board ,board
                      :dealer ,(nth dealer *seats*)
                      :vulnerable ,(board-vulnerable board)
                      :auction ,(mapcar #'call-str bids)
                      :contract (:level ,level
                                 :trump ,(suitstr trump)
                                 :declarer ,(nth declarer *seats*)
                                 :doubled ,(if doubled (call-str doubled) ""))
                      :cards nil))))
            (destructuring-bind (hs decl tr mine played) (training-game game)
                (declare (ignore played))
                (setf (getf game :cards) (mapcar #'cardstr (computer-plays hs decl tr nil mine memo))))
            (list (new-sim-id) game memo))))

; Saves a game training-prepare made, keeping its memo and its board
; number as the last one, and returns its view
(defun training-start (prepared)
    (destructuring-bind (id game memo) prepared
        (setf (gethash id *training-memos*) (cons memo (get-universal-time)))
        (trim-training-memos id)
        (save-last-board (getf game :board))
        (write-sim id game *training-dir*)
        (training-view id game)))

;; Batches: the games to come are prepared by a worker thread while the
;; player plays, so that a new game starts at once. The queue holds up to
;; batch of them for one key (role defs), numbered on from the last board
;; handed out (next, the worker's next one); asking for a game of another
;; key makes a new queue, the old one's worker stopping after its game.
(defparameter *training-batch* 6)
(defparameter *training-max-batch* 20)

(defstruct tqueue key (batch 0) (games nil) (working nil) (next 1))

(defvar *training-queue* nil)
(defvar *training-queue-lock* (sb-thread:make-mutex :name "training queue"))
(defvar *training-queue-ready* (sb-thread:make-waitqueue :name "training queue ready"))

; Starts the worker of queue q unless it is working or full; called with
; the lock held. It stops when the queue is full or replaced, or a game
; can't be prepared (the player then gets the error preparing one himself).
(defun training-refill (q role defs)
    (when (and (not (tqueue-working q)) (< (length (tqueue-games q)) (tqueue-batch q)))
        (setf (tqueue-working q) t)
        (sb-thread:make-thread
            (lambda ()
                (let ((*random-state* (make-random-state t)))
                    ; however it ends, the queue is told it no longer works
                    (unwind-protect
                        (loop
                            (let* ((board (sb-thread:with-mutex (*training-queue-lock*)
                                              (prog1 (tqueue-next q) (incf (tqueue-next q)))))
                                   (prepared (handler-case (training-prepare role defs board)
                                                 (error (e) (format t "~&[training batch] ~A~%" e) nil))))
                                (sb-thread:with-mutex (*training-queue-lock*)
                                    (if prepared
                                        (setf (tqueue-games q) (append (tqueue-games q) (list prepared)))
                                        ; the number is used again
                                        (if (= (tqueue-next q) (1+ board)) (setf (tqueue-next q) board)))
                                    (when (or (null prepared) (not (eq q *training-queue*))
                                              (>= (length (tqueue-games q)) (tqueue-batch q)))
                                        (setf (tqueue-working q) nil))
                                    (sb-thread:condition-broadcast *training-queue-ready*)
                                    (if (not (tqueue-working q)) (return)))))
                        (sb-thread:with-mutex (*training-queue-lock*)
                            (when (tqueue-working q)
                                (setf (tqueue-working q) nil)
                                (sb-thread:condition-broadcast *training-queue-ready*))))))
            :name "training batch")))

; New game (see training-prepare) from the queue, waiting for the worker's
; next one when it is empty; prepared here when there is no worker (batch
; 0, or it failed), with the queue's next board number. The queue is
; filled up to batch again.
(defun training-new (role &optional defs batch)
    (let* ((batch (if (and (integerp batch) (<= 0 batch *training-max-batch*)) batch *training-batch*))
           (key (list role defs))
           (board nil)
           (prepared (sb-thread:with-mutex (*training-queue-lock*)
                         (let ((q *training-queue*))
                             (when (not (and q (equal (tqueue-key q) key)))
                                 (setf q (make-tqueue :key key :next (1+ (last-board)))
                                       *training-queue* q))
                             (setf (tqueue-batch q) batch)
                             (training-refill q role defs)
                             (prog1 (loop (cond ((tqueue-games q) (return (pop (tqueue-games q))))
                                                ((tqueue-working q)
                                                 (sb-thread:condition-wait *training-queue-ready* *training-queue-lock*))
                                                (t (setf board (tqueue-next q))
                                                   (incf (tqueue-next q))
                                                   (return nil))))
                                 (training-refill q role defs))))))
        (append (training-start (or prepared (training-prepare role defs board)))
                (list :queue (training-queue-view)))))

; Games ready and the worker's state, for the page
(defun training-queue-view ()
    (sb-thread:with-mutex (*training-queue-lock*)
        (let ((q *training-queue*))
            `(:ready ,(if q (length (tqueue-games q)) 0)
              :working ,(if (and q (tqueue-working q)) t :false)))))

; The player claims the rest of game id: all seats play it out by hints,
; as the hint search finds best for both sides (whether it was his to claim
; isn't checked). The game keeps how many cards were played at the claim.
(defun training-claim (id)
    (let ((game (or (load-training id) (edit-error "No game ~A" id))))
        (destructuring-bind (hands declarer trump mine cards) (training-game game)
            (declare (ignore hands declarer trump mine))
            (if (= (length cards) 52) (edit-error "All cards are played"))
            (setf (getf game :claimed) (length cards))
            (training-advance id game cards t))))

; Card played by the player in game id
(defun training-play (id card)
    (let ((game (or (load-training id) (edit-error "No game ~A" id))))
        (destructuring-bind (hands declarer trump mine cards) (training-game game)
            (if (= (length cards) 52) (edit-error "All cards are played"))
            (multiple-value-bind (left done trick leader) (training-position hands declarer trump cards)
                (declare (ignore done))
                (let ((c (or (str2card card) (edit-error "Bad card ~A" card)))
                      (seat (mod (+ leader (length trick)) 4)))
                    (if (not (member seat mine))
                        (edit-error "~A is to play" (nth seat *seats*)))
                    (if (not (member c (legal-cards (nth seat left) (and trick (first (first trick))))
                                     :test #'equal))
                        (edit-error "~A can't be played now" card))
                    (training-advance id game (append cards (list c))))))))

;; Test deals for play by hints, saved as sims/hinttest-NN.json
; A scenario is (name count defs check contract): defs of the hands N E S
; as the generator of mc-deal takes them (W gets the rest), check a test of
; the deal (hands as suits, k its number in the scenario) for what defs
; can't say, contract (trump declarer) of N-S, else the one sim-axis picks.

(defun hand-hcp (hand)
    (loop for ranks in hand sum (loop for r in ranks when (> r 8) sum (- r 8))))

(defun side-hcp (hands side)
    (+ (hand-hcp (nth side hands)) (hand-hcp (nth (+ side 2) hands))))

(defun suit-len (hands seat suit)
    (length (nth suit (nth seat hands))))

(defun hinttest-scenarios ()
    (list
        ; 20 HCP 4-4-4-1 with stoppers, ♥ K singleton opposite ♥ AQxxxxx and
        ; spot cards; the opponents' hearts 3-2, 4-1, 5-0
        (list "strong-nt" 3
              '((:hcp (20 20) :s (4 4 (2 nil)) :h (1 1 (3 3)) :d (4 4 (2 nil)) :c (4 4 (2 nil)))
                nil
                (:hcp (6 6) :h (7 7 (6 6))))
              (lambda (h k) (= (max (suit-len h 1 2) (suit-len h 3 2)) (nth k '(3 4 5))))
              '(nil 0))
        ; Misfits on both sides, each hand long where its partner is short;
        ; power balanced
        (list "misfit" 2
              '((:hcp (9 12) :s (6 7) :h (0 1)) (:hcp (8 12) :d (6 7) :c (0 1)) (:hcp (9 12) :h (6 7) :s (0 1)))
              (lambda (h k) (declare (ignore k))
                  (and (>= (suit-len h 3 0) 5) (<= (suit-len h 3 1) 1) (<= 19 (side-hcp h 0) 21)))
              nil)
        ; A 9-card fit for each side, power balanced
        (list "double-fit" 3
              '((:hcp (8 12) :s (5 5) :h (0 3)) (:hcp (8 12) :h (5 5) :s (0 2)) (:hcp (8 12) :s (4 4) :h (0 2)))
              (lambda (h k) (declare (ignore k)) (and (= (suit-len h 3 2) 4) (<= 19 (side-hcp h 0) 21)))
              nil)
        ; ♠ 5-3 with trumps 4-1, E's long diamonds forcing N
        (list "trump-control" 2
              '((:hcp (13 16) :s (5 5) :d (1 2)) (:hcp (8 11) :s (4 4) :d (5 6)) (:hcp (9 12) :s (3 3)))
              (lambda (h k) (declare (ignore k)) (= (suit-len h 3 3) 1))
              '(3 0))
        ; ♠ 4-4 with shortness in diamonds and clubs opposite each other
        (list "crossruff" 2
              '((:hcp (11 14) :s (4 4) :d (0 1) :c (4 5)) nil (:hcp (10 13) :s (4 4) :c (0 1) :d (4 5)))
              nil
              '(3 0))
        ; NT, a weak dummy with ♣ AKQxxx and no outside entry
        (list "dummy-suit" 2
              '((:hcp (17 19) :s (3 4) :h (3 4) :d (3 4) :c (1 2 (0 0))) nil (:hcp (9 9) :c (6 6 (9 9))))
              nil
              '(nil 0))
        ; Balanced hands, 25-29 HCP, a tenace AQ against the K
        (list "tenaces" 2
              '((:hcp (13 15) :s (3 4) :h (3 4) :d (3 4) :c (3 4)) nil
                (:hcp (12 14) :s (3 4) :h (3 4) :d (3 4) :c (3 4)))
              (lambda (h k) (declare (ignore k))
                  (loop for suit below 4
                        thereis (flet ((ns (r) (or (member r (nth suit (nth 0 h))) (member r (nth suit (nth 2 h))))))
                                    (and (ns 12) (ns 10) (not (ns 11))))))
              nil)
        ; ♠ AKxxx opposite three small, E with ♠ Qxxx
        (list "master-trump" 1
              '((:hcp (13 16) :s (5 5 (7 7))) (:s (4 4 (2 2))) (:hcp (9 12) :s (3 3 (0 1))))
              nil
              '(3 0))
        ; ♠ by N with heart losers, dummy's three trumps needed for ruffing
        ; them before drawing
        (list "ruff-first" 2
              '((:hcp (14 17) :s (5 6) :h (3 3 (0 1))) nil (:hcp (7 10) :s (3 3) :h (0 1)))
              nil
              '(3 0))
        (list "random" 1 nil nil nil)))

(defun normdef (defs)
    (loop for def in defs
          collect (cond ((numberp def) `(,def ,def))
                        ((and (listp def)
                              (numberp (third def)))
                            `(,(first def) ,(second def) (,(third def) ,(third def))))
                        (t def))))

; Hands as suits of a deal of defs passing check
(defun hinttest-hands (defs check k)
    (let ((deal (and defs (make-instance 'deal :~ (mapcar #'normdef defs)))))
        (loop repeat 50000
              for hands = (ignore-errors (mapcar #'suits (if deal (build deal) (random-deal))))
              when (and hands (or (null check) (funcall check hands k)))
              return hands
              finally (error "No deal for ~S" defs))))

; Axes of the deal of formulas as sim-deal picks them, (trump declarer)
(defun auto-axes (formulas)
    (let ((objs (formula-hands formulas)))
        (loop for r in '(0 1)
              collect (destructuring-bind (suit declarer &rest _) (sim-axis (roll r objs))
                          (declare (ignore _))
                          (list (position suit '(c d h s))
                                (or (position declarer *seats* :test #'equal) 3))))))

(defparameter *hint-sim-timeout* 600)

; Simulation of the deal, saved: by play-deal, or with engine "hints" by the
; hint search (hint-deal), its caches its own so that requests running at
; once don't share them. Played in the contracts of axes, (trump declarer),
; else in the ones play-deal picks.
(defun sim-deal-saved (hands raw-hands &optional engine axes)
    (if (equal engine "hints")
        (let ((id (new-sim-id))
              (*suit-solve-cache* (make-hash-table :test 'equal))
              (*discard-facts* (make-hash-table))
              (*standard-output* (make-broadcast-stream))) ; hint-deal prints the play for hinttest
            `(:id ,id ,@(nth-value 1 (hint-deal raw-hands (or axes (auto-axes raw-hands)) :save id
                                                :timeout *hint-sim-timeout*))))
        (let ((result (sim-deal hands axes)))
            `(:id ,(save-sim raw-hands result) ,@result))))

; Deals of the scenarios, (name hands contract), generated from seed
(defun hinttest-deals (seed)
    (let ((*random-state* (sb-ext:seed-random-state seed)))
        (loop for (name count defs check contract) in (hinttest-scenarios)
              append (loop for k below count
                           collect (list name (hinttest-hands defs check k) contract)))))

(defun hinttest-formulas (hands)
    (loop for seat below 3
          collect (format nil "~A: ~A" (nth seat *seats*) (print-hand (nth seat hands)))))

; Prints the deals of run-hinttests with seed and the axes they get
(defun hinttest-preview (&key (seed 7))
    (loop for (name hands contract) in (hinttest-deals seed)
          for n from 1
          for formulas = (hinttest-formulas hands)
          do (format t "~%~D ~A: ~{~{~A by ~A~}~^, ~}~%" n name
                     (mapcar (lambda (a) (list (suitstr (first a)) (nth (second a) *seats*)))
                             (let ((auto (auto-axes formulas))) (if contract (list contract (second auto)) auto))))
             (dotimes (s 4) (format t "  ~A  (~D)~%" (hand-str hands s) (hand-hcp (nth s hands))))
             (unless (equal (mapcar #'suits (formula-hands formulas))
                            (mapcar (lambda (h) (mapcar (lambda (r) (sort (copy-list r) #'<)) h)) hands))
                 (format t "  formulas don't give these hands~%"))))

; Generates the scenarios' deals (seeded) and plays each by hints (those
; numbered in only, or all), saving sims/<prefix>-NN.json, logs and a
; summary in logdir. Timeout is per deal, shared by its axes.
(defun run-hinttests (&key (seed 7) (timeout 600) (prefix "hinttest") (logdir "hinttest-logs/") only
                           (summary "summary.txt"))
    (ensure-directories-exist logdir)
    (let ((rows nil))
        (loop for (name hands contract) in (hinttest-deals seed)
              for n from 1
              for id = (format nil "~A-~2,'0D" prefix n)
              for formulas = (hinttest-formulas hands)
              for auto = (auto-axes formulas)
              when (or (null only) (member n only))
              do (format t "~%=== ~A: ~A ===~%" id name)
                 (push (list id name (hint-deal formulas (if contract (list contract (second auto)) auto)
                                                :save id :timeout timeout
                                                :log (format nil "~A~A.log" logdir id)))
                       rows)
                 (with-open-file (o (format nil "~A~A" logdir summary) :direction :output :if-exists :supersede
                                                                       :external-format :utf-8)
                     (loop for (id name axes) in (reverse rows)
                           do (format o "~A ~13A~{ | ~{~A by ~A: play-deal ~D, hints ~A~:[ (best so far)~;~] ~Ds ~D nodes~}~}~%"
                                      id name
                                      (mapcar (lambda (a) (destructuring-bind (tr d old tricks done secs nodes) a
                                                              (list (suitstr tr) (nth d *seats*) old (or tricks "-")
                                                                    done secs nodes)))
                                              axes)))))
        (reverse rows)))

(defclass result ()
    ((success :reader ok?)
     (val :reader val)))
 
(defmethod initialize-instance ((this result) &key ok error)
    (with-slots (success val) this
        (setf success (if error nil T))
        (setf val (or error ok))))

(defmethod match ((this result) &key ok error)
    (with-slots (success val) this
        (cond (success (if (functionp ok) (funcall ok val) val))
              (t (if (functionp error) (funcall error val) val)))))

(defmethod list-merger (a b)
    (make-instance 'result :ok (append a (list b))))

(defmethod combine ((this result) (other result) &optional (merger #'list-merger))
    (match this :ok (lambda (val)
                        (match other :ok (curry #'funcall merger val)
                                     :error (constantly other)))
                :error (lambda (val)
                          (match other :ok (constantly this)
                                       :error (lambda (v) (make-instance 'result :error (format nil "~A~%~A" val v)))))))

(test (match (combine (make-instance 'result :ok '(1)) (make-instance 'result :error "bad")) :error #'identity)
      "bad" equal)

(defun simple-endpoint (env fields function &key parse-args)
    (let* ((request (jonathan:parse (read-body env)))
           (raw-args (loop for field in fields
                           collect (getf request field)))
           (args (if parse-args (fold (curry #'combine)
                                      (make-instance 'result)
                                      (loop for i from 0 below (length raw-args)
                                            for base in raw-args
                                            collect (if (nth i parse-args)
                                                        (funcall (nth i parse-args) base)
                                                        (make-instance 'result :ok base))))
                                (make-instance 'result :ok raw-args))))
      (match args :error (lambda (error)
                            `(400 (:content-type "application/json")
                                  (,(jonathan:to-json `(:error ,error)))))
                  :ok (lambda (args)
                            `(200 (:content-type "application/json")
                                  (,(jonathan:to-json (apply function args))))))))
       
(defun string-validator (&key length)
    (lambda (x)
        (if (or (not (stringp x))
                (and length (< (length x) length)))
           (make-instance 'result :error (format nil "Bad user name: ~S" x))
           (make-instance 'result :ok x))))

(defun hand-parser (handstr)
    (if (stringp handstr)
        (make-instance 'result :ok (str2hand handstr))
        (make-instance 'result :error (format nil "Wrong hand format: ~A" handstr))))

(defvar *user-validator* (string-validator :length 4))

(defun close-list (lst base)
    (if (< (length lst) (length base))
        (append lst (subseq base (length lst)))
        lst))

(defclass jobs ()
    ((cases) (last-id) (lock)))

(defmethod initialize-instance ((this jobs) &key)
    (with-slots (cases last-id lock) this
        (setf cases (make-hash-table))
        (setf last-id -1)
        (setf lock (bt:make-lock))))

(defmethod add-job ((this jobs) mcc)
    (with-slots (cases last-id lock) this
        (bt:with-lock-held (lock)
            (incf last-id)
            (setf (gethash last-id cases) mcc)
            (list last-id mcc))))
     
(defmethod get-case ((this jobs) id)
    (with-slots (cases) this
        (gethash id cases)))

(defmethod cancel ((this jobs))
    (with-slots (cases) this
        (loop for x being the hash-values of cases
              do (loop for thread in (threads x)
                       do (bt:destroy-thread thread)))))

(defmethod syms ((this jobs))
    (loop for key being the hash-keys of (slot-value this 'cases)  collect key))

(defparameter *jobs* (make-instance 'jobs))

(defun invert-id (ids)
    (mapcar #'first (sort (zip-id ids) (lambda (a b) (< (second a) (second b))))))

(test (invert-id '(2 0 1 3)) '(1 2 0 3) equal)
(test (invert-id '(3 2 0 1)) '(2 3 1 0) equal)

; Deal of defs (hands or ranges of seats N E S W in this order, the seats
; after them random) and the order turning the hands it builds into seats
; N E S W, as (deal order)
(defun defs-generator (defs)
    (let-from! (mapcar #'reverse (divlist (list (f* #'second #'type-of (curry #'eq 'hand)))
                                          (zip-id defs)))
               (consts ranged)
       (list (make-instance 'deal := (mapcar (f* #'second #'pretty-tree) consts)
                                  :~ (mapcar (f* #'second #'normdef) ranged))
             (close-list (invert-id (mapcar #'first (append consts ranged))) '(0 1 2 3)))))

(defun mc-deal (defs measures)
    (destructuring-bind (d reorder) (defs-generator defs)
         (let-from! (add-job *jobs* (make-instance 'mc-case :! `(reorder (build ,d) ',reorder)
                                                            := (or measures '(best-outcomes))))
                    (id mc)
           (sim mc 500)
           (sleep 5)
           `(:status in-progress :job-id ,id
             :data ,(histogram (mapcar (curry #'apply #'append) (select mc)))))))

;; Deals of a group of an MC simulation: the frontend sends the values of a
;; line of its histogram from the top (prefix), the group being the deals
;; whose values start with them

; A histogram value as JSON gives it back: symbols by name, nil as nil
(defun json-value (x)
    (cond ((null x) nil)
          ((consp x) (mapcar #'json-value x))
          ((symbolp x) (symbol-name x))
          (t x)))

; Do values, as the histogram counts them, fall into the group of prefix?
; A nil in prefix past the end of values is the line of rows ending there.
(defun values-in-group? (vals prefix)
    (loop for p in prefix
          for rest = vals then (cdr rest)
          always (if rest (equal (json-value (car rest)) p) (null p))))

(test (values-in-group? '(s 10 nt 9) '("S" 10)) t eq)
(test (values-in-group? '(s 10 nt 9) '("S" 11)) nil eq)
(test (values-in-group? '(s 10) '("S" 10 nil)) t eq)
(test (values-in-group? '(s 10 nt 9) '("S" 10 nil)) nil eq)

; Deals of job's group of prefix, of measures (else all of its props): their
; count and up to limit of them, each with its hands as formulas N E S W
; and its values
(defun mc-group-deals (job prefix measures &optional (limit 100))
    (let* ((rows (select job :fields (cons 'hands (or measures (slot-value job 'props)))))
           (group (loop for row in rows
                        for i from 0
                        for vals = (apply #'append (rest row))
                        when (values-in-group? vals prefix)
                        collect (list i (first row) vals))))
        `(:count ,(length group)
          :deals ,(loop for (i hands vals) in group
                        repeat limit
                        collect `(:index ,i
                                  :hands ,(loop for suits in (mapcar #'suits hands)
                                                for seat in *seats*
                                                collect (format nil "~A: ~A" seat (print-hand suits)))
                                  :values ,vals)))))

; Setups of MC jobs by id: what the frontend sends with a job (its hands
; and measures as a JSON string), given back when it connects to the job
; again
(defparameter *job-setups* (make-hash-table :synchronized t))

; mc-deal, keeping setup of the job
(defun mc-deal-setup (defs measures setup)
    (let ((ans (mc-deal defs measures)))
        (if (stringp setup)
            (setf (gethash (getf ans :job-id) *job-setups*) setup))
        ans))

(defun hand-or-def-parser (defs)
    (fold #'combine
          (make-instance 'result)
          (mapcar (lambda* (type val)
                     (make-instance 'result :ok (cond ((eq type :|hand|) (str2hand val))
                                                      (t (read-from-string val)))))
                  defs)))

; Games prepared one after another don't share their cards (a queued game
; once got the cards of the one prepared after it); here, as training-prepare
; needs hand-or-def-parser
(test (let ((a (second (training-prepare "leader" nil 1)))
            (b (second (training-prepare "leader" nil 2))))
          (eq (member :cards a) (member :cards b)))
      nil
      eq)

(defun measure-parser (names)
    (if (equal names '("")) (make-instance 'result :ok '(best-outcomes))
        (fold #'combine
            (make-instance 'result)
            (mapcar (lambda (name)
                        (let ((contract (make-instance 'contract := name)))
                            (make-instance 'result :ok
                                (eval `(defun ,(read-from-string (format nil "msr-~A" name)) (_ &rest hands)
                                    (declare (ignore _))
                                    (let ((tricks (apply #'best-tricks 
                                                        (cons ',(suit contract)
                                                            (roll ,(- (position (declarer contract)
                                                                                '(N E S W)))
                                                                    hands)))))
                                        (let ((result (with-score ,contract tricks)))
                                            (list tricks (base-score result)))))))))
                    names))))

; Axes (trump declarer), suit and seat numbers, of contract names as
; measures take them (4SN, 3NTE; the level doesn't matter), nil for none
(defun contracts-parser (names)
    (let ((axes (loop for name in names
                      for c = (ignore-errors (make-instance 'contract := name))
                      for suit = (and c (suit c))
                      collect (and suit (declarer c)
                                   (list (or (position suit '(c d h s)) (position suit '(♣ ♦ ♥ ♠)))
                                         (position (declarer c) '(n e s w)))))))
        (if (member nil axes)
            (make-instance 'result :error (format nil "Contract ~A needs a suit and a declarer, like 4SN"
                                                  (nth (position nil axes) names)))
            (make-instance 'result :ok axes))))

(test (val (contracts-parser '("4SN" "3NTE" "5♦W"))) '((3 0) (nil 1) (1 3)) equal)
(test (ok? (contracts-parser '("4S"))) nil eq)
(test (val (contracts-parser nil)) nil eq)

(defun measure-sym-parser (names)
    (make-instance 'result :ok (filter (lambda (sym)
                                            (and sym (ignore-errors (eval `(function ,sym)))))
                                       (mapcar (f* (curry #'format nil "msr-~A") #'read-from-string)
                                               names))))

(defun substr= (ref str)
    (and (>= (length str) (length ref))
         (string= ref (subseq str 0 (length ref)))))

(defun json-response (retcode response)
    `(,retcode (:content-type "application-json")
      (,(jonathan:to-json response))))

; Response of function called with the request body parsed, edit errors
; given as 400
(defun edit-endpoint (env function)
    (let ((request (jonathan:parse (read-body env))))
        (handler-case (json-response 200 (funcall function request))
            (edit-error (e) (json-response 400 `(:error ,(princ-to-string e)))))))

(defun app (env)
  (let ((path (getf env :path-info)))
    (cond
      ((eq (getf env :request-method) :OPTIONS)
       `(200 (:content-type "text/plain")
             ("")))
      ((string= path "/api/sit")
          (simple-endpoint env '(:|user|) (curry #'sit *table*)
                           :parse-args (list *user-validator*)))
      ((string= path "/api/setting")
       `(200 (:content-type "application/json")
        (,(jonathan:to-json (setting *table*)))))
      ((string= path "/api/add-hand")
          (simple-endpoint env '(:|user| :|hand|) (curry #'add-hand *table*)
                               :parse-args (list *user-validator*)))
      ((string= path "/api/auction")
          (simple-endpoint env '(:|user| :|since|) (curry #'auction *table*)
                               :parse-args (list *user-validator*)))
      ((string= path "/api/simdeal")
          (simple-endpoint env '(:|hands| :|hands| :|engine| :|contracts|) #'sim-deal-saved
                               :parse-args (list (f* (curry #'mapcar #'hand-parser)
                                                     (curry #'fold #'combine (make-instance 'result)))
                                                 nil nil #'contracts-parser)))
      ((string= path "/api/simedit")
       (edit-endpoint env (lambda (request)
                             (let ((lines (and (equal (getf request :|action|) "complete")
                                               (sim-complete-lines (getf request :|id|) (getf request :|axis|)))))
                                 (sb-thread:with-mutex (*sim-lock*)
                                    (sim-edit (getf request :|id|) (getf request :|axis|)
                                              (getf request :|action|) (getf request :|card|)
                                              (getf request :|trick|) (getf request :|plan|)
                                              (getf request :|tricks|) lines))))))
      ((string= path "/api/train")
       (edit-endpoint env (lambda (request) (training-new (getf request :|role|) (getf request :|defs|)
                                                          (getf request :|batch|)))))
      ((string= path "/api/train-queue")
       (json-response 200 (training-queue-view)))
      ((string= path "/api/grade")
       (edit-endpoint env #'grade-request))
      ((substr= "/api/grade/measure" path)
       (let ((rest (subseq path (length "/api/grade/measure"))))
          (edit-endpoint env (lambda (request) (gjob-request rest request)))))
      ((and (substr= "/api/train/" path) (search "/claim" path))
       (let ((id (subseq path (length "/api/train/") (search "/claim" path))))
          (edit-endpoint env (lambda (request) (declare (ignore request)) (training-claim id)))))
      ((and (substr= "/api/train/" path) (search "/play" path))
       (let ((id (subseq path (length "/api/train/") (search "/play" path))))
          (edit-endpoint env (lambda (request) (training-play id (getf request :|card|))))))
      ((substr= "/api/train/" path)
       (let* ((id (subseq path (length "/api/train/")))
              (game (load-training id)))
          (if game (json-response 200 (training-view id game))
                   (json-response 404 `(:status :not-found)))))
      ((substr= "/api/simdeal/" path)
       (let ((saved (load-sim (subseq path (length "/api/simdeal/")))))
          (if saved `(200 (:content-type "application/json") (,saved))
                    (json-response 404 `(:status :not-found)))))
      ((string= path "/api/mc")
          (simple-endpoint env '(:|defs| :|measures| :|setup|) #'mc-deal-setup
                               :parse-args (list #'hand-or-def-parser #'measure-parser nil)))
      ((and (substr= "/api/mc/" path) (search "/deals" path))
       (let ((job (get-case *jobs* (parse-integer path :start (length "/api/mc/") :junk-allowed t))))
          (if job (simple-endpoint env '(:|prefix| :|measures|)
                                   (lambda (prefix measures) (mc-group-deals job prefix measures))
                                   :parse-args (list nil #'measure-sym-parser))
                  (json-response 404 `(:status :not-found)))))
      ((substr= "/api/mc/" path)
       (let ((job-id (read-from-string (subseq path (length "/api/mc/")))))
          (let ((job (get-case *jobs* job-id)))
             (if job (simple-endpoint env '(:|measures|)
                        (lambda (measures)
                            `(:status ,(if (threads job) :in-progress :success)
                              :setup ,(gethash job-id *job-setups*)
                              :data ,(histogram (mapcar (curry #'apply #'append)
                                                    (if measures (select job :fields measures)
                                                                 (select job))))))
                        :parse-args (list #'measure-sym-parser))
                     (json-response 400 `(:status :not-found))))))
              
      (t
       `(404 (:content-type "text/plain") ("Not found"))))))

(defun wrap-errors (app)
  (lambda (env)
    (let ((backtrace nil)
          (error-message nil))
      (restart-case
          (handler-bind
              ((error
                (lambda (e)
                  ;; Capture error message
                  (setf error-message (princ-to-string e))
                  ;; Capture backtrace if SBCL
                  (when (find-package 'sb-debug)
                    (setf backtrace
                          (with-output-to-string (s)
                            (ignore-errors
                              (funcall (find-symbol "PRINT-BACKTRACE" "SB-DEBUG")
                                       :count 30 :stream s)))))
                  ;; Abort normal execution
                  (invoke-restart 'abort))))
                ;; Call the app normally
                (funcall app env))
        ;; Abort restart clause
        (abort ()
          ;; Print error to stdout
          (format t "~&[ERROR] ~A~%" error-message)
          (when backtrace
            (format t "~&[TRACEBACK]~%~A~%" backtrace))
          (force-output)
          ;; Return 500 JSON response
          `(500 (:content-type "application/json")
                (,(jonathan:to-json
                   `(:error ,error-message)))))))))


;; Wrap app manually with CORS middleware
(defparameter *wrapped-app*
  (wrap-errors
    (clack-cors:make-cors-middleware
        'app
        :allowed-origin "*"
        :allowed-methods "GET, POST, OPTIONS"
        :allowed-headers "Content-Type")))

;; Start server
(defparameter *server*
    (clack:clackup *wrapped-app* :server :hunchentoot :port 5000 :max-connections 20))

(defun restart-server ()
    (clack:stop *server*)
    (setf *server* (clack:clackup *wrapped-app* :server :hunchentoot :port 5000 :max-connections 20)))
