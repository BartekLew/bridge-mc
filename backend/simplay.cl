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
;;  2. Alternatives: nondeterministic choice among lazily generated options,
;;     used to compose naive lines out of the sequences suit-tops etc. offer.
;;  3. Trickseq: generating one-suit trick sequences. Intentionally, missing
;;     cards are replaced with nulls so that they can be handled separately.
;;  4. Greedy tricks sequence – will create base for further optimization.


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
                            (format nil "~{~A~}" (reverse (mapcar #'rankstr hand))))
                         split)))

(defun best (compare lst)
    (if lst
        (fold (lambda (acc val) (if (funcall compare val acc) val acc))
              (first lst)
              (rest lst))))

(test (best #'> '(1 -5 10 14 8 -100)) 14 eq)

(defun mod+ (denominator &rest operands)
    (mod (apply #'+ operands) denominator))

;; -- lazy streams: a stream is either nil, or (head . thunk) where thunk
;; is a 0-arg function that, when called, produces the rest of the stream.
;; Kept minimal on purpose - just enough to make list-prod* lazy.
(defmacro lcons (head tail)
    `(cons ,head (lambda () ,tail)))

(defun lcar (stream) (car stream))
(defun lcdr (stream) (funcall (cdr stream)))

(defun lconcat (stream more)
    ;; more: a thunk producing the stream to append once `stream` runs out
    (if stream (lcons (lcar stream) (lconcat (lcdr stream) more))
               (funcall more)))

(defun lmap (fn stream)
    (if stream (lcons (funcall fn (lcar stream)) (lmap fn (lcdr stream)))))

(defun lmapcan (fn lst)
    ;; lst: an ordinary list; fn: element -> stream. Lazily concatenates
    ;; the streams (fn x) produce, one x at a time.
    (if lst (lconcat (funcall fn (car lst)) (lambda () (lmapcan fn (cdr lst))))))

(test (let ((s (lmap (curry #'+ 1) (lcons 1 (lcons 2 (lcons 3 nil))))))
        (list (lcar s) (lcar (lcdr s)) (lcar (lcdr (lcdr s)))))
      '(2 3 4) equal)

;; ====================================================
;; 2. Alternatives
;; ====================================================
;; Nondeterministic choice: an alternative holds a list of candidate
;; values for one "slot"; decide explores the (lazy) cartesian product
;; across several slots looking for a good-enough combination.
(defclass alternative ()
    ((options :initarg :options :reader options)))

(defun alternative (&rest options)
    (make-instance 'alternative :options options))

(defun list-prod* (&rest lists)
    ;; lazy cartesian product - same tuples as list-prod, produced one at a
    ;; time so a caller can stop early without paying for the rest
    (labels ((self (lists)
                (if (not lists) (lcons nil nil)
                    (lmapcan (lambda (v) (lmap (curry #'cons v) (self (cdr lists))))
                             (car lists)))))
       (self lists)))

(test (let ((s (list-prod* '(1 2) '(a b))))
        (loop for cell = s then (lcdr cell)
              while cell
              collect (lcar cell)))
      '((1 a) (1 b) (2 a) (2 b))
      equal)

(defun decide (process grade &rest alternatives)
    ;; Explores combinations (one option per alternative) looking for the
    ;; best-graded (process ...) result. Stops as soon as a combination
    ;; grades 1.0; a 0.0 grade is never kept, even as a last resort.
    ;; Returns (list result grade), or nil if nothing scored above 0.0.
    (labels ((scan (stream best)
                (if (not stream) best
                    (let ((result (apply process (lcar stream))))
                       (if (not result)
                           (scan (lcdr stream) best)
                           (let ((g (funcall grade result)))
                              (cond ((>= g 1.0) (list result g))
                                    ((and (> g 0.0) (or (not best) (> g (second best))))
                                     (scan (lcdr stream) (list result g)))
                                    (t (scan (lcdr stream) best)))))))))
       (scan (apply #'list-prod* (mapcar #'options alternatives)) nil)))

;; -- test case decoupled from bridge: exact change from a fixed set of
;; coin denominations. process sums the picked coins, grade rewards sums
;; close to the expected total, 1.0 only on an exact match.
(defvar *coin-values* '(5.0 2.0 1.0 0.50 0.20 0.10 0.05 0.02 0.01))

(defun coin-grade (expected coins)
    (let ((value (apply #'+ coins)))
        (/ (min expected value) (max expected value))))

(defun coin-change (expected count)
    (apply #'decide #'list (curry #'coin-grade expected)
           (loop repeat count collect (apply #'alternative *coin-values*))))

(test (coin-change 0.30 1) (list '(0.2) 0.6666666) equal)
(test (coin-change 0.30 2) (list '(0.2 0.1) 1.0) equal)
(test (coin-change 0.07 2) (list '(0.05 0.02) 1.0) equal)

;; ====================================================
;; 3. Trickseq
;; ====================================================
;; Structure made to store trick sequences in one suit,
;; missing cards possible, additional metadata stored
(defclass trickseq ()
    ((suit :initform nil :initarg :suit)
     (tricks :initform nil :initarg :tricks :reader tricks)
     (remaining :initarg :remaining :reader remaining)))

(defmethod print-object ((this trickseq) o)
    (format o "SEQ<~A~A:~A>" (suit-uc (slot-value this 'suit))
                             (joinstr " " (mapcar #'trickstr (tricks this)))
                             (suitsplitstr (remaining this))))

(defmethod eq? ((a trickseq) (b trickseq))
    (and (eq (slot-value a 'suit) (slot-value b 'suit))
         (equal (tricks a) (tricks b))
         (equal (remaining a) (remaining b))))

(defmethod eq? ((a list) (b list))
    (not (filter #'not (mapcar #'eq? a b))))

(defmethod top-trick ((this trickseq) winner)
    ;; creates a sequence after winner took trick (others laid smallest)
    (with-slots (suit tricks remaining) this
        (let-from! (apply (curry #'mapcar #'list)
                          (loop for player from 0 to 3
                                for cards in remaining
                                collect (if (= player winner)
                                            (let ((lastno (- (length cards) 1)))
                                                (list (nth lastno cards) (subseq cards 0 lastno)))
                                            (list (first cards) (rest cards)))))
                   (ranks rest)
            (make-instance 'trickseq :suit suit
                                     :tricks (append tricks (list ranks))
                                     :remaining (roll (- winner) rest)))))

(test (top-trick (make-instance 'trickseq :suit 'c :remaining (str2split "AJ106 Q4 K532 987")) 2)
      (make-instance 'trickseq :suit 'c :remaining (str2split "532 98 AJ10 Q") 
                               :tricks (str2tricks "6-4-K-7"))
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
 
(defmethod winner-offset ((this trickseq))
    ;; Each trick's ranks are stored relative to whoever led *that* trick,
    ;; not to the original leader - so the winner-index of each trick is a
    ;; rotation offset, and folding them with mod+ accumulates back to an
    ;; absolute (well, leader-relative) final position. Void hands (nil
    ;; rank) are filtered out first, rather than compared - some of the 4
    ;; can be void, and a single remaining holder must win outright. This
    ;; same offset is also exactly how far `remaining` has been rotated
    ;; away from the frame this trickseq was originally built in.
    (with-slots (tricks) this
        (fold (curry #'mod+ 4)
              0
              (loop for tr in tricks
                    collect (first (best (lambda (a b) (> (second a) (second b)))
                                         (filter #'second (zip-id tr))))))))

(defmethod ends-on? ((this trickseq) player)
    (eq player (winner-offset this)))

(defmethod unrolled-remaining ((this trickseq))
    ;; `remaining` sits in whatever local frame the last trick left it in;
    ;; this undoes every continuation roll to re-express it back in the
    ;; original (position 0 = this trickseq's own leader) frame, so it can
    ;; be merged with other suits that all share that same reference frame.
    (with-slots (remaining) this
        (let ((s (winner-offset this)))
            (loop for i from 0 to 3 collect (nth (mod (- i s) 4) remaining)))))

(defun trickseq (suit remaining &optional tricks)
    (make-instance 'trickseq :suit suit :tricks tricks :remaining remaining))

(test (ends-on? (trickseq 's (str2split "J10 - 53 Q") (str2tricks "6-4-K-7 2-8-A-9")) 0)
      T eq)
(test (ends-on? (trickseq 's (str2split "53 Q J10 -") (str2tricks "A-4-2-7 6-9-K-8")) 2)
      T eq)

(test (top-trick-opts (trickseq 'c (str2split "AJ106 Q4 K532 987")))
      (list (trickseq 'c (str2split "J106 Q K53 98") (str2tricks "A-4-2-7"))
            (trickseq 'c (str2split "532 98 AJ10 Q") (str2tricks "6-4-K-7")))
      eq?)


(test (top-trick-opts (trickseq 'd (str2split "J106 - K532 Q98")))
      (list (trickseq 'd (str2split "532 Q9 J10 ")(str2tricks "6-x-K-8")))
      eq?)

(test (top-trick-opts (trickseq 'd (str2split "8 - 3 -")))
      (list (trickseq 'd '(nil nil nil nil) (str2tricks "8-x-3-x") ))
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
      (list (trickseq 's (str2split "J10 - 53 Q") (str2tricks "6-4-K-7 2-8-A-9"))
            (trickseq 's (str2split "53 Q J10 -") (str2tricks "A-4-2-7 6-9-K-8")))
      eq?)
      
;; previously produced this pair of candidates twice over (4 total) - one
;; path in each duplicate pair relied on the illegal-duck bug fixed above,
;; and happened to converge on the same terminal state as the legal path
(test (suit-tops (trickseq 'd (str2split "AJ106 974 K532 Q8")))
      (list (trickseq 'd '(nil nil nil nil) (str2tricks "6-4-K-8 2-Q-A-7 J-9-3-x 10-x-5-x"))
            (trickseq 'd '(nil nil nil nil) (str2tricks "A-4-2-8 6-7-K-Q 3-x-J-9 10-x-5-x")))
      eq?)

;; partner void from the start, so there is never a second holder to
;; check for - continuation must not crash trying to mod nil by 2
(test (suit-tops (trickseq 'd (str2split "AK - - -")))
      (list (trickseq 'd '(nil nil nil nil) (str2tricks "A-x-x-x K-x-x-x")))
      eq?)

(defmethod roll-lead ((this trickseq))
    ;; Reinterprets this candidate as if partner, not the original leader,
    ;; had led its very first trick. Every later trick is already framed
    ;; relative to "whoever won the previous trick" (a fact about the
    ;; cards, not a label), so only the first trick needs relabeling -
    ;; whoever wins it still leads trick two either way. Only meaningful
    ;; for candidates that already end on partner (see suit-options):
    ;; that guarantees partner held a card in the suit from the very
    ;; start too, so the swap can never manufacture an impossible trick.
    (with-slots (suit tricks remaining) this
        (make-instance 'trickseq :suit suit
                                 :tricks (cons (roll 2 (first tricks)) (rest tricks))
                                 :remaining remaining)))

(test (tricks (roll-lead (trickseq 's (str2split "53 Q J10 -") (str2tricks "A-4-2-7 6-9-K-8"))))
      (str2tricks "2-7-A-4 6-9-K-8")
      equal)

(test (ends-on? (roll-lead (trickseq 's (str2split "53 Q J10 -") (str2tricks "A-4-2-7 6-9-K-8"))) 0)
      T eq)

(defun suit-target (candidates)
    ;; best case for this suit alone: its longest naive-tops candidate
    (if candidates (apply #'max (mapcar (f* #'tricks #'length) candidates)) 0))

;; Combined naive-tops result across all 4 suits: the merged trick
;; sequence, what's left in every suit (not just the ones played), and
;; who is now on lead - so the other partnership's own naive-tops (or
;; whatever comes next for this one) can pick up from here. `remaining`
;; is always kept self-consistent with `on-lead`: position 0 in every
;; suit's leftover holding is whoever `on-lead` says is next to act,
;; mirroring how trickseq's own `remaining` always leads with position 0.
(defclass topsline ()
    ((tricks :initarg :tricks :reader tricks)
     (remaining :initarg :remaining :reader remaining)
     (on-lead :initarg :on-lead :reader on-lead)))

(defmethod print-object ((this topsline) o)
    (format o "LINE<~A tricks, on-lead ~A, remaining ~A>"
              (length (tricks this)) (on-lead this) (remaining this)))

(defun suit-options (holding candidates)
    ;; Builds one suit's alternative option values: skipping it (using
    ;; the untouched holding as-is), using it while leader is on lead (as
    ;; computed - every candidate here requires that, since
    ;; top-trick-opts refuses to start from a void leader), or using it
    ;; while partner is on lead (only for candidates that already end on
    ;; partner, reframed via roll-lead). Every option is
    ;; (required-initiator ends-on suited-tricks remaining), leader/
    ;; partner tagged 0/2 throughout - remaining is the same either way
    ;; a given candidate is framed, since roll-lead never touches it.
    (cons (list 0 0 nil holding)
          (append (mapcar (lambda (c) (list 0 (if (ends-on? c 2) 2 0)
                                            (suited-tricks c) (unrolled-remaining c)))
                          candidates)
                  (mapcar (lambda (c) (list 2 2 (suited-tricks (roll-lead c)) (unrolled-remaining c)))
                          (filter (lambda (c) (ends-on? c 2)) candidates)))))

(defun compose-tops (&rest suit-choices)
    ;; Stitches up to 4 chosen suit-options into one trickline: suits
    ;; that keep leader in control go first (any order - they never change
    ;; who's on lead), then at most one suit that hands the lead to partner
    ;; (the "transfer" - it must go last among leader-block suits, since
    ;; once it's played leader no longer has the lead to feed anything
    ;; else its natural way), then the partner-block suits (any order -
    ;; every one of them starts and ends on partner, so they chain freely).
    ;; Two transfers would be a contradiction; a partner-block with no
    ;; transfer at all is simply unreachable. suit-choices arrive in c d
    ;; h s order (matching naive-tops), so their `remaining` slots can be
    ;; collected positionally regardless of which block each landed in.
    (let* ((leader-block (filter (lambda (c) (eq (first c) 0)) suit-choices))
           (partner-block (filter (lambda (c) (eq (first c) 2)) suit-choices))
           (transfers (filter (lambda (c) (eq (second c) 2)) leader-block))
           (stays (filter (lambda (c) (eq (second c) 0)) leader-block)))
       (if (or (> (length transfers) 1)
               (and partner-block (not transfers)))
           nil
           (let ((merged (apply #'append (mapcar #'third (append stays transfers partner-block)))))
              ;; a deal only has 13 tricks total - each suit's own count
              ;; can run past its "real" share once opponents void out and
              ;; the rest is just filler discards, but the 4 suits share
              ;; one 13-trick budget, not 4 independent ones. remaining
              ;; stays leader-relative here (position 0 = original leader,
              ;; same as unrolled-remaining's own convention) - naive-tops
              ;; is the one that knows the actual seat numbering, so it
              ;; does the final conversion to absolute terms.
              (if (<= (length merged) 13)
                  (make-instance 'topsline :tricks merged
                                           :on-lead (if transfers 2 0)
                                           :remaining (mapcar #'fourth suit-choices)))))))

(defun tops-grade (target line)
    ;; capped at 13: the suit-independent target can overstate what's
    ;; actually reachable (see compose-tops), and 13 is the hard ceiling
    ;; on tricks in any deal, so the best truly achievable line must
    ;; still grade 1.0 rather than fall short against an inflated target
    (if (= target 0) 0.0 (/ (length (tricks line)) (min target 13))))

(defun naive-tops (deal leader)
    ;; deal: 4 hands in a fixed absolute order; leader: seat index (0-3)
    ;; of whoever is on lead. Searches, suit by suit, for the naive
    ;; top-trick-taking line that gets closest to each suit's own best case.
    (let* ((ours (roll (- leader) deal))
           (holdings (loop for suit in '(c d h s) collect (mapcar (curry* #'hand-suit (h) (h suit)) ours)))
           (per-suit (loop for suit in '(c d h s)
                           for holding in holdings
                           collect (suit-tops (trickseq suit holding))))
           (target (apply #'+ (mapcar #'suit-target per-suit)))
           (alts (loop for holding in holdings
                       for candidates in per-suit
                       collect (apply #'alternative (suit-options holding candidates))))
           (out (apply #'decide #'compose-tops (curry #'tops-grade target) alts)))
       (if out
           (let ((line (first out)))
              (list (make-instance 'topsline :tricks (tricks line)
                                             ;; bring remaining and on-lead back out of the
                                             ;; leader-rolled working frame into deal's own
                                             ;; seat numbering, so callers never have to
                                             ;; account for which seat happened to lead
                                             :remaining (mapcar (curry #'roll leader) (remaining line))
                                             :on-lead (mod (+ leader (on-lead line)) 4))
                    (second out))))))

;; worked example: leader takes AK in spades and the ace in diamonds
;; (staying on lead throughout, no transfer needed), then either clubs or
;; hearts serves as the one transfer to partner's hand (they both end
;; there naturally), letting partner run the rest - 2+1+5+6 suit-by-suit
;; sums to 14, but suits share one 13-trick budget, so target is capped
;; at 13 for grading and the actual best (13 tricks - decide happens to
;; find the line that drops diamonds' free ace rather than trimming a
;; club, an arbitrary tie-break between equally-long lines worth
;; revisiting later) grades as the perfect result it actually is
(test (second (naive-tops (str2deal "S: ♣ QJ43 ♦ A543 ♥ 2 ♠ AK43
W: ♣ 76 ♦ KQJ10 ♥ 876 ♠ QJ109
N: ♣ AK1098 ♦ 2 ♥ AKQJ109 ♠ 2
E: ♣ 52 ♦ 9876 ♥ 543 ♠ 8765") 0))
      1 =)

;; every test above used leader=0, which can't catch a sign error in how
;; `leader` rotates `deal` (roll and its negation coincide at 0) - this
;; one specifically exercises a non-zero leader: E holds AK of spades
;; outright and must take exactly those 2 tricks
(test (tricks (first (naive-tops (str2deal "N: ♠ 9743 ♥ K104 ♦ QJ3 ♣ 764
E: ♠ AK10 ♥ J986 ♦ K74 ♣ 1082
S: ♠ QJ8652 ♥ AQ732 ♦ A ♣ A
W: ♠  ♥ 5 ♦ 1098652 ♣ KQJ953") 1)))
      (suited-tricks (trickseq 's nil (str2tricks "A-2-x-3 K-5-x-4")))
      equal)

;; the run of clubs (N cashes K,Q then goes void; S takes over and runs
;; A,J,8) hands the lead to South, not North - on-lead and remaining
;; must reflect that in the caller's own N/E/S/W seat numbering, not
;; whichever seat happened to lead first
(test (let ((line (first (naive-tops (str2deal "N: ♠ A742 ♥ 974 ♦ 985 ♣ KQ9
E: ♠ Q9 ♥ 653 ♦ AK6432 ♣ 64
S: ♠ KJ86 ♥ Q108 ♦ J ♣ AJ873
W: ♠ 1053 ♥ AKJ2 ♦ Q107 ♣ 1052") 0))))
        (list (on-lead line) (remaining line)))
      (list 2 (list '(nil nil nil nil)
                    (str2split "985 AK6432 J Q107")
                    (str2split "974 653 Q108 AKJ2")
                    '(nil nil nil nil)))
      equal)
