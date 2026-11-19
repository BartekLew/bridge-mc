(load "bidding.cl")

(defvar full-random (make-instance 'deal))

(defun better-score (a b)
    (> (second (score a)) (second (score b))))

(defun query-card (suit hand &optional so-far)
    (if so-far (format t "~A...~%" (mapcar #'cardstr (cards so-far))))
    (format t "~A?~% " hand)
    (let ((choice (card (read-line))))
        (if (and choice
                 (find (second choice) (hand-suit hand (first choice)))
                 (or (not suit)
                     (eq (suitno (first choice)) (suitno suit))
                     (not (hand-suit hand suit))))
            (take-card hand choice)
            (query-card suit hand))))

(defun manual-trick (trump lead hands &optional (defender-hand nil))
    "Handles playing one trick in a bridge deal. 
     If lead is present, defenders started, so the user chooses cards from the 2nd and 4th hand.
     If lead is absent, the player starts. 
     In defender mode, only one card is chosen manually, and the rest are played by the computer."

     (let* ((first-card (or lead (if (or (not defender-hand) (= defender-hand 0))
                                           (first (query-card nil (first hands)))
                                           (first (first (tricks (apply (curry #'play-deal trump) 
                                                                  (roll 1 hands))))))))
            (base (make-instance 'trick :trump trump :suit (first first-card)))
            (after-first (apply #'play (cons base (take-card (first hands) first-card))))
            (after-second (if (or (eq defender-hand 1) (and (not defender-hand) lead))
                              (apply #'play (cons after-first (query-card (first first-card) (second hands) after-first)))
                              (finesse-if-possible after-first (second hands) (third hands))))
            (after-third (if (or (eq defender-hand 2) (and (not defender-hand) (not lead)))
                              (apply #'play (cons after-second (query-card (first first-card) (third hands) after-second)))
                              (finesse-if-possible after-second (third hands) (fourth hands)))))
        (if (or (eq defender-hand 3) (and (not defender-hand) lead))
            (apply #'play (cons after-third (query-card (first first-card) (fourth hands) after-third)))
            (beat-or-low after-third (fourth hands)))))

(defun manual-play (trump lead hands &optional defender-hand acc)
    "Handles playing a deal with computer. Lead is provided if computer starts.
     defender-hand is provided if game is played in defence mode"

    (let* ((trick (manual-trick trump lead hands defender-hand)))
        (format t "~A~%" (mapcar #'cardstr (cards trick)))
        (if (> (apply #'+ (mapcar (curry #'apply #'+) (suits (first (remaining trick))))) 0)
            (let ((new-hands (roll (- (winner trick)) (remaining trick))))
                 (if (and (not defender-hand) (xor lead (= (mod (winner trick) 2) 1)))
                     (manual-play trump (first (first (tricks (apply (curry #'play-deal trump) (roll 1 new-hands)))))
                                  new-hands
                                  (and defender-hand (mod (- defender-hand (winner trick)) 4))
                                  (append acc (list trick)))
                     (manual-play trump nil new-hands
                                  (and defender-hand (mod (- defender-hand (winner trick)) 4))
                                  (append acc (list trick)))))
            (append acc (list trick)))))

(defun skip (lst)
    (if (or (not lst) (car lst)) lst (skip (cdr lst))))


(defun who-plays (bidding)
    (labels ((player (rev-bids)
                (let* ((partnership (mod (- (length rev-bids) 1) 2))
                       (pbids (loop for bid in (reverse rev-bids)
                                    for i from 0
                                    append (if (= (mod i 2) partnership) (list bid) nil)))
                       (suit (second (first rev-bids)))
                       (first-in-suit (fold (lambda (acc bid)
                                                (if acc acc
                                                    (if (and (consp (second bid))
                                                             (eq (second (second bid)) suit))
                                                        (first bid) nil)))
                                            nil
                                            (zip-id pbids))))
                   (mod (+ (* 2 first-in-suit) partnership) 4))))
                   
     (let ((last-bids (skip (reverse bidding))))
        (cond ((not last-bids) nil)
              ((eq (car last-bids) 'x)
                  (let ((doubled (skip (cdr last-bids))))
                    (player doubled)))
              (t (player last-bids))))))
                     
(test (who-plays '(nil (1 S) nil nil nil)) 1 eq)

(test (who-plays '((1 C) nil (1 D) (2 C) nil nil nil)) 3 eq)

(test (who-plays '(nil nil (1 S) (1 NT) nil nil x))
      3 eq)

(test (who-plays '(nil (1 H) (1 S) (2 H) x nil nil nil)) 1 eq)

(test (who-plays '(nil (1 NT) nil (2 NT) nil (3 C) nil (3 D) nil (3 H) nil nil nil)) 1 eq)

(test (who-plays '((1 S) x nil (2 H) nil nil nil)) 3 eq)

;; ===================================
;; Play training as a service
;; ===================================
; A game is a random deal bid by the computer and the cards played so far in
; order, from the opening lead; the position is rebuilt from them each time.
; Hands are by seat (0 N, 1 E, 2 S, 3 W) as suits of hand, cards (suit rank),
; suit and trump numbers, trump nil in NT.

; Contract of auction bids (calls from the dealer, seat 0) as (level strain
; declarer doubled), strain a suit number or nil for NT, doubled nil, x or
; xx; nil when passed out
(defun auction-contract (bids)
    (let ((rev (skip (reverse bids)))
          (doubled nil))
        (loop while (member (car rev) '(x xx))
              do (setf doubled (or doubled (car rev))
                       rev (skip (cdr rev))))
        (if rev
            (list (first (car rev)) (normsuit (second (car rev))) (who-plays (reverse rev)) doubled))))

(test (auction-contract '(nil nil (1 NT) nil (2 C) nil (2 H) nil (2 S) nil (4 S) nil nil nil))
      '(4 3 0 nil) equal)
(test (auction-contract '(nil (1 NT) nil nil nil)) '(1 nil 1 nil) equal)
(test (auction-contract '((1 S) nil (2 S) x nil nil xx nil nil nil)) '(2 3 0 xx) equal)
(test (auction-contract '(nil nil nil nil)) nil eq)

; Seat of the player in role (:declarer, :leader, :partner of the leader or
; :defender, one of these two at random) when declarer plays
(defun role-seat (role declarer)
    (mod (+ declarer (ecase role
                         (:declarer 0)
                         (:leader 1)
                         (:partner 3)
                         (:defender (randcar '(1 3)))))
         4))

; New deal of generator (hand objects N E S W, N dealing) bid by the
; computer, turned so the player in role sits S (seat 2), as (hands dealer
; bids contract), contract as auction-contract gives it. With dealer, the
; seat that must deal once the deal is turned, deals where the dealer comes
; out elsewhere are dealt again too; with any-dealer, each deal is first
; tried with N dealing, then E, S and W, else only with N. Passed out deals
; are dealt again, as are those the bidding fails on (the error is printed),
; up to tries deals; nil when none of them fits.
(defun training-deal (role &optional (generator (lambda () (build full-random))) tries dealer any-dealer)
    (loop for try from 1
          while (or (null tries) (<= try tries))
          do
        (let ((objs (funcall generator)))
            (loop for g in (if any-dealer '(0 1 2 3) '(0))
                  do (let* ((bids (handler-case (drain (make-instance 'bidding :deal (roll (- g) objs)))
                                      (error (e)
                                          (format t "~&[ERROR] bidding of ~A: ~A~%" (mapcar #'suits objs) e)
                                          nil)))
                            (contract (auction-contract bids)))
                         (when contract
                             (destructuring-bind (level strain declarer* doubled) contract
                                 (let* ((declarer (mod (+ declarer* g) 4))
                                        (k (mod (- 2 (role-seat role declarer)) 4)))
                                     (when (or (null dealer) (= (mod (+ g k) 4) dealer))
                                         (return-from training-deal
                                             (list (roll k (mapcar #'suits objs)) (mod (+ g k) 4) bids
                                                   (list level strain (mod (+ declarer k) 4) doubled))))))))))))

; Board number n of a match: its dealer (N E S W in turn) and who is
; vulnerable, in the cycle of 16 boards
(defun board-dealer (n)
    (mod (- n 1) 4))

(defun board-vulnerable (n)
    (nth (mod (- n 1) 16)
         '("None" "NS" "EW" "Both" "NS" "EW" "Both" "None" "EW" "Both" "None" "NS" "Both" "None" "NS" "EW")))

(test (loop for n from 1 to 17 collect (list (board-dealer n) (board-vulnerable n)))
      '((0 "None") (1 "NS") (2 "EW") (3 "Both") (0 "NS") (1 "EW") (2 "Both") (3 "None")
        (0 "EW") (1 "Both") (2 "None") (3 "NS") (0 "Both") (1 "None") (2 "NS") (3 "EW") (0 "None"))
      equal)

; Seats the player plays from: declarer's and dummy's (N) or his own (S)
(defun training-seats (role)
    (if (eq role :declarer) '(2 0) '(2)))

; Hands without the cards of trick (so far, led by leader)
(defun trick-left (hands leader trick)
    (loop for seat from 0 below 4
          for i = (mod (- seat leader) 4)
          collect (if (< i (length trick))
                      (without-card (nth seat hands) (nth i trick))
                      (nth seat hands))))

; Position after cards played from the opening lead: hands left, tricks
; done as (cards leader taker), the trick going on (cards so far) and its
; leader
(defun training-position (hands declarer trump cards)
    (let ((leader (mod (+ declarer 1) 4))
          (done nil))
        (loop while (>= (length cards) 4)
              do (let ((tr (subseq cards 0 4)))
                    (setf hands (play-trick hands leader tr))
                    (push (list tr leader (trick-taker tr leader trump)) done)
                    (setf leader (third (first done))
                          cards (nthcdr 4 cards))))
        (values (trick-left hands leader cards) (reverse done) cards leader)))

(test (multiple-value-list
          (training-position '(((0) (0) () ()) ((1) (1) () ()) ((2) () (0) ()) ((3) () () (0))) 3 nil
                             '((0 0) (0 1) (0 2) (0 3) (3 0))))
      '(((() (0) () ()) (() (1) () ()) (() () (0) ()) (() () () ()))
        ((((0 0) (0 1) (0 2) (0 3)) 0 3))
        ((3 0))
        3)
      equal)

; Card the computer plays from the seat next to play to trick (cards so far,
; led by leader), hands without them: a lead is the first card of play-deal
; from the position, the others as manual-trick plays them. When that isn't
; a legal card, the lowest legal one.
(defun computer-card (hands leader trump trick)
    (let* ((n (length trick))
           (seat (mod (+ leader n) 4))
           (objs (mapcar (lambda (s) (make-instance 'hand := (mapcar #'copy-list s)))
                         (roll (- leader) hands)))
           (legal (legal-cards (nth seat hands) (and trick (first (first trick)))))
           (card (if (= n 0)
                     (first (first (tricks (apply #'play-deal (suitsym trump) (roll 1 objs)))))
                     (let ((tr (make-instance 'trick :trump trump :suit (first (first trick)))))
                         (loop for card in trick
                               for h in objs
                               do (play tr card h))
                         (let ((ans (case n
                                        (1 (finesse-if-possible tr (second objs) (third objs)))
                                        (2 (finesse-if-possible tr (third objs) (fourth objs)))
                                        (t (beat-or-low tr (fourth objs))))))
                             (and ans (lastcar (cards ans))))))))
        (if (member card legal :test #'equal) card (first legal))))

; A lead from the leader's hand; fourth hand overtakes the king with the ace
(test (computer-card '(((5) () () ()) ((10) () () ()) ((11) () () ()) ((6) () () ())) 0 nil nil)
      '(0 5) equal)
(test (computer-card '(((5) () () ()) ((10) () () ()) ((2) () () ()) ((6 12) () () ())) 0 nil
                     '((0 3) (0 4) (0 11)))
      '(0 12) equal)

;; Computer play by hints
; Seconds the hint search may take for one card before the computer takes
; the best found so far, or computer-card when none (nil: no limit)
(defparameter *hint-card-seconds* nil)

; File each card of the computer's play by hints is logged to, with the
; time it took, when not nil
(defparameter *hint-card-log* "hint-card.log")

; Card of the seat next to play to trick (cards so far from leader, hands as
; at its start) by hints: the first tricks of the variants of plan-hints
; starting with those cards, played out as a game tree, each seat taking the
; card best for its side, a whole trick worth its tricks for N-S and what
; hint-search finds after it (memo, the search's, kept for the game),
; searched by alpha-beta, the lines in the order of hint-choices. Past
; *hint-deadline* the best card found so far, if any. Nil when no line starts
; with the cards, as when the player chose one no hint has.
(defun hint-card (hands leader trump trick memo)
    (let* ((n (length trick))
           (firsts (remove-duplicates
                       (loop for choice in (hint-choices (plan-hints hands leader trump) hands leader trump)
                             append (loop for tricks in (rest choice)
                                          for tr = (first tricks)
                                          when (equal (subseq tr 0 n) trick) collect tr))
                       :test #'equal :from-end t))
           (found nil))
        ; (value card) of the best card at index k of the tricks cands, value
        ; a bound when not within alpha beta
        (labels ((best (k cands alpha beta)
                     (if (= k 4)
                         (multiple-value-bind (ns after next) (line-result hands leader trump (list (first cands)))
                             (list (+ ns (car (hint-search after next trump (- alpha ns) (- beta ns) 1 memo))) nil))
                         (let ((maxp (evenp (+ leader k)))
                               (ans nil))
                             (dolist (card (remove-duplicates (mapcar (lambda (tr) (nth k tr)) cands)
                                                              :test #'equal :from-end t)
                                           ans)
                                 (let ((v (first (best (1+ k) (remove-if-not (lambda (tr) (equal (nth k tr) card))
                                                                             cands)
                                                       alpha beta))))
                                     (when (or (null ans) (if maxp (> v (first ans)) (< v (first ans))))
                                         (setf ans (list v card))
                                         (if (= k n) (setf found card)))
                                     (if maxp (setf alpha (max alpha v)) (setf beta (min beta v)))
                                     (if (>= alpha beta) (return ans))))))))
            (and firsts
                 (handler-case (second (best n firsts -1 14))
                     (hint-timeout (e) (if found found (error e))))))))

; The computer's card at trick (so far, led by leader) by hints, start the
; hands at its start, left without its cards; computer-card when the hints
; have none in time
(defun computer-hint-card (start left leader trump trick memo)
    (let* ((t0 (get-internal-real-time))
           (hint (let ((*hint-deadline* (and *hint-card-seconds*
                                             (+ t0 (* *hint-card-seconds* internal-time-units-per-second)))))
                     (handler-case (hint-card start leader trump trick memo)
                         (error (e) (format t "~&[hint-card] ~A~%" e) nil))))
           (card (or hint (computer-card left leader trump trick))))
        (when *hint-card-log*
            (with-open-file (o *hint-card-log* :direction :output :if-exists :append
                               :if-does-not-exist :create :external-format :utf-8)
                (format o "~{~2,'0D~^:~} trick ~D, ~A plays ~A~:[ (computer-card)~;~] after ~,2Fs~%"
                        (multiple-value-bind (s m h) (decode-universal-time (get-universal-time)) (list h m s))
                        (- 14 (floor (cards-left start) 4)) (nth (mod (+ leader (length trick)) 4) '(n e s w))
                        (cardstr card) hint
                        (/ (- (get-internal-real-time) t0) internal-time-units-per-second))))
        card))

; Cards played, the computer's ones added until a seat of mine is to play or
; all cards are played; by hints, memo being the hint search's for the game
(defun computer-plays (hands declarer trump cards mine &optional (memo (make-hash-table :test 'equal)))
    (loop
        (multiple-value-bind (left done trick leader) (training-position hands declarer trump cards)
            (declare (ignore done))
            (if (or (= (length cards) 52) (member (mod (+ leader (length trick)) 4) mine))
                (return cards))
            (let ((start (training-position hands declarer trump (subseq cards 0 (- (length cards) (length trick))))))
                (setf cards (append cards (list (computer-hint-card start left leader trump trick memo))))))))

(defparameter *training-hands*
    (mapcar #'suits (str2deal "N: ♣ KJ96 ♦ A8 ♥ AJ83 ♠ AK9
E: ♣ Q5 ♦ J9762 ♥ Q ♠ J6543
S: ♣ 432 ♦ 1043 ♥ K10954 ♠ 87
W: ♣ A1087 ♦ KQ5 ♥ 762 ♠ Q102")))

; Fixed hands dealt for each dealer, S declaring: when some dealer gives S
; the contract, the deal comes out with that dealer and S declaring
(test (loop for dealer from 0 below 4
            for d = (training-deal :declarer (lambda () (mapcar (lambda (s) (make-instance 'hand := (mapcar #'copy-list s)))
                                                                *training-hands*))
                                   1 dealer t)
            always (or (null d) (and (= (second d) dealer) (= (third (fourth d)) 2))))
      t
      eq)

; The computer plays a whole deal legally (tested in slow-tests.cl); with
; declarer's seats left to the player (S declaring) it stops after the
; opening lead
; (with a second for the lead: the search for it takes some 45 seconds)
(test (let ((*hint-card-seconds* 1)) (length (computer-plays *training-hands* 2 2 nil '(2 0)))) 1 eql)

; Play of cards from the opening lead as an outcome of play-deal, for
; play-sequences
(defun training-outcome (hands declarer trump cards)
    (let ((tricks (mapcar #'first (nth-value 1 (training-position hands declarer trump cards)))))
        (make-instance 'outcome :tricks tricks
                                :winners (mapcar (lambda (tr) (nth (trick-winner tr trump) tr)) tricks)
                                :winner-nos (mapcar (lambda (tr) (trick-winner tr trump)) tricks)
                                :finesses (mapcar (constantly nil) tricks))))

(defun summarized-outcome (label trump outcome hands)
    (format t "~A: ~A~%~A~%" label outcome (apply (curry #'summarize-outcome outcome trump) hands)))

(defparameter *deal* nil)
(defun play-training (&key deal roll trump defender-hand)
    (flet ((play (suit hands outcome)
              (let ((lead (cond ((not defender-hand)
                                    (format t "dummy: ~a~%hand: ~a~%trump: ~a~%"
                                            (third hands) (first hands) (suit-uc suit))
                                    (first (first (tricks outcome))))
                                ((eq defender-hand 0)
                                    (format t "trump: ~A~%you lead: " (suit-uc suit))
                                    (let ((card (first (query-card nil (second hands)))))
                                        (format t "dummy: ~A~%" (third hands))
                                        card))
                                (t (format t "dummy: ~a, trump: ~A~%" (third hands) (suit-uc suit))))))
                  (summarized-outcome "you" suit 
                        (new-outcome (manual-play suit lead (roll -1 hands) defender-hand))
                        hands)
                  (summarized-outcome "cpu" suit outcome hands)
                  (format t "hands: ~A~%" hands))))
        (if (and deal roll)
            (let ((ref (apply (curry #'play-deal trump) (roll roll deal))))
                (play trump (roll (or roll 0) deal) ref))
            (let* ((hands (or deal (build full-random)))
                   (bidding (drain (make-instance 'bidding :deal hands))))
                (if (not deal) (setf *deal* hands))
                (if (find-if #'id bidding)
                    (let ((suit (second (first (skip (reverse bidding)))))
                          (rhands (roll (- (who-plays bidding)) hands)))
                      (format t "bidding: ~a~%" bidding)
                      (play suit rhands (apply (curry #'play-deal suit) rhands))))))))

