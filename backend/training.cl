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
                                                    (if (eq (second (second bid)) suit) (first bid) nil)))
                                            nil
                                            (zip-id pbids))))
                   (+ (* 2 first-in-suit) partnership))))
                   
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

