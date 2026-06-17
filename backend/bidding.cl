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

; (load "bridge.cl")

; Module: bidding.cl — bridge bidding heuristics (SAYC‑like)
; Purpose:
; - For a given hand and bidding history, produce the next call (or a table mapping calls to conditions).
; - Operates on hand metrics: suit lengths (lens), HCP per suit (power), total losers.
; Dependencies:
; - Utility functions/macros from bridge.cl: str2hand, suits, suit-hcp, suitno, suitsym,
;   fold, filter, mapcar, curry, lambda-dot, letcar, let-from*, let-from!, with-results, test, etc.
; Main parts:
; - assess-hand/balanced?: compute metrics and balancedness.
; - openings: base opening table (alist -> rule s-expr).
; - choose-bid: picks the first matching call from a given table (e.g., openings, nt-responses).
; - nt-responses, 2C-responses, basic-responses: responses after specific openings.
; - merge-shapes and extractors (min-hcp, max-hcp, longers): working with rules/shapes.
; - further-bid: generates continuations using known/min ranges and last partner bid.
; - Class bidding: thin state machine that advances bidding to the first found call.
;
; Conventions:
; - Suits: C, D, H, S, NT; suitsym/suitno convert between symbol and index.
; - power is HCP per suit; hcp is the sum.
; - losers is simplified LTC; lower is better.
; - “Rules” are plain s-expressions (and/or, comparisons), evaluated by good-opening?.
; - Bidding tables are alists: (bid . rule).
; - A bid is a pair (level, suit), e.g., '(1 S), '(2 NT).
; 
; Simple version of Losing Trick Count evaluation

;; Simplified LTC for a single suit.
;; Returns the number of losing tricks in a suit based on presence of A/K/Q (12/11/10) and length.
(defun suit-losers (ranks)
    "Calculate the number of losing tricks in a suit based on its ranks."
    (+ (if (or (find 12 ranks) (= (length ranks) 0)) 0 1)
       (if (or (find 11 ranks) (< (length ranks) 2)) 0 1)
       (if (or (find 10 ranks) (< (length ranks) 3)) 0 1)))

(test (suit-losers '(12 2)) 1 eq)
(test (suit-losers '(12 11 6 5 4)) 1 eq)
(test (suit-losers '()) 0 eq)
(test (suit-losers '(11 10)) 1 eq)
(test (suit-losers '(10 9 8 7)) 2 eq)
(test (suit-losers '(12 10 9 8 7)) 1 eq)
(test (suit-losers '(7)) 1 eq)

;; Hand assessment core:
;; - Returns (lens, power, losers); optional measure allows injecting a predicate/metric over these three.
;; - Used by good-opening?, choose-bid and all response tables.
(defun assess-hand (hand &optional measure)
    "Assess a hand by calculating its length, high card points (HCP), and losing tricks.
    Optionally apply a measure function to the results."
    (let ((vals (list (mapcar #'length (suits hand))
                      (mapcar #'suit-hcp (suits hand))
                      (apply #'+ (mapcar #'suit-losers (suits hand))))))
       (if measure (apply measure vals)
           vals)))

;; Hand balancedness by shape/strength. Used in openings and NT responses.
(defun balanced? (lens power losers)
    "Determine if a hand is balanced based on its length, power, and losing tricks."
    (declare (ignore losers))
    (and (not (find-if (curry #'> 2) lens))
         (not (find-if (curry #'< 4) (reorder lens '(2 3))))
         (not (find-if (curry #'< 5) (reorder lens '(0 1))))
         (or (< (apply #'+ power) 12) 
             (< (length (filter (curry #'> 3) power)) 2))))
        
(test (assess-hand (str2hand "♣ A65 ♦ A875 ♥ QJ52 ♠ J7") #'balanced?) T eq)
(test (assess-hand (str2hand "♣ J72 ♦ AQJ85 ♥ Q9 ♠ KQ10") #'balanced?) nil eq) ; Two weak suits
(test (assess-hand (str2hand "♣ KJ7 ♦ AJ542 ♥ 9872 ♠ A") #'balanced?) nil eq) ; Single spade
(test (assess-hand (str2hand "♣ A65 ♦ A875 ♥ QJ5 ♠ AJ7") #'balanced?) T eq)
(test (assess-hand (str2hand "♣ A43 ♦ AQJ852 ♥ Q9 ♠ KQ") #'balanced?) nil eq) ; Minor six
(test (assess-hand (str2hand "♣ KJ7 ♦ AJ ♥ AJ972 ♠ AQ4") #'balanced?) nil eq) ; Major 5

;; Very simple SAYC-like base
; =======================================================
; List of openings with rules as defined in good-opening?
; =======================================================

;; Rule evaluator: substitutes local symbols (C, D, H, S, hcp, power, balanced, losers, etc.)
;; into the rule s-expression and evals it. Used by choose-bid/test-bid and openings.
(defun good-opening? (rules lens power losers)
    "Check if a hand meets the criteria for a good opening bid based on given rules."
    (eval (sublis `((C . ,(first lens))
                    (D . ,(second lens))
                    (H . ,(third lens))
                    (S . ,(fourth lens))
                    (Cpower . ,(first power))
                    (Dpower . ,(second power))
                    (Hpower . ,(third power))
                    (Spower . ,(fourth power))
                    (lens . ',lens)
                    (power . ',power)
                    (hcp . ,(apply #'+ power))
                    (losers . ,losers)
                    (balanced . (balanced? ',lens ',power nil)))
                  rules)))

(defvar balanced (append (loop for s in '(c d h s)
                               collect `(>= ,s 2))
                         (loop for s in '(c d)
                               collect `(<= ,s 5))
                         (loop for s in '(h s)
                               collect `(<= ,s 4))))
                   
;; Main opening table (SAYC‑like, simplified):
;; - Keys are calls (level, suit).
;; - Values are rules in the syntax understood by good-opening?.
;; - Uses balanced?, hcp, losers and per-suit length/power.
(defparameter openings 
                 `(((2 C) . (or (> hcp 22)
                                (and (not balanced)
                                     (cond ((find-if (curry #'< 4) (subseq lens 2 4)) (<= losers 4))
                                           ((find-if (curry #'< 4) (subseq lens 0 2)) (<= losers 3))))))
                   ((2 NT) . (and ,@balanced (> hcp 18) (<= hcp 22)))
                   ((1 NT) . (and ,@balanced (> hcp 14) (< hcp 18)))
                   ((1 S) . (and (or (< losers 6) (> hcp 11))
                                 (> losers 4) (< hcp 23)
                                 (>= S 5)))
                   ((1 H) . (and (or (< losers 6) (> hcp 11))
                                 (> losers 4) (< hcp 23)
                                 (>= H 5) (< S 5)))
                   ((1 D) . (and (or (< losers 6) (> hcp 11))
                                 (> losers 3) (< hcp 23)
                                 (>= D 4) (>= D C) (< H 5) (< S 5)))
                   ((1 C) . (and (or (< losers 6) (> hcp 11))
                                 (> losers 3) (< hcp 23)
                                 (>= C 3) (< H 5) (< S 5)))
                   ((2 D) . (and (> hcp 5) (>= Dpower 5) (or (> losers 5) (< hcp 11)) (= D 6)))
                   ((2 H) . (and (> hcp 5) (>= Hpower 5) (or (> losers 5) (< hcp 11)) (= H 6)))
                   ((2 S) . (and (> hcp 5) (>= Spower 5) (or (> losers 5) (< hcp 11)) (= S 6)))
                   ((3 C) . (and (> hcp 5) (>= Cpower 5) (< hcp 11) (>= C 7)))
                   ((3 D) . (and (> hcp 5) (>= Dpower 5) (< hcp 11) (>= D 7)))
                   ((3 H) . (and (> hcp 5) (>= Hpower 5) (< hcp 11) (>= H 7)))
                   ((3 S) . (and (> hcp 5) (>= Spower 5) (< hcp 11) (>= S 7)))
                  ))
                                   
(defun open-bid (bid)
    (cdr (assoc bid openings :test #'equal)))

(defun test-bid (bid hand)
    (assess-hand hand (curry #'good-opening? (open-bid bid))))

(test (test-bid '(2 C) (str2hand "♣ AKQJ5 ♦ KQ543 ♥ AQ ♠ 7")) T eq)
(test (test-bid '(2 C) (str2hand "♣ AKQJ5 ♦ AK3 ♥ AK3 ♠ 543")) T eq)
(test (test-bid '(2 C) (str2hand "♣ AKQJ ♦ AQ83 ♥ K103 ♠ K43")) nil eq)
(test (test-bid '(2 C) (str2hand "♣ AKQJ ♦ AQ83 ♥ K103 ♠ K43")) nil eq)
(test (test-bid '(2 NT) (str2hand "♣ AKQJ ♦ AQ83 ♥ K103 ♠ K43")) T eq)
(test (test-bid '(2 NT) (str2hand "♣ AQJ5 ♦ AQ83 ♥ K103 ♠ K43")) T eq)
(test (test-bid '(1 NT) (str2hand "♣ AQJ5 ♦ AQ108 ♥ 1093 ♠ K43")) T eq)

(test (test-bid '(1 S) (str2hand "♣ 852 ♦ AQ ♥ AQJ3 ♠ A10943")) T eq)
(test (test-bid '(1 S) (str2hand "♣ 852 ♦ A10 ♥ AQJ103 ♠ A10943")) T eq)
(test (test-bid '(1 H) (str2hand "♣ 852 ♦ AQ ♥ AQJ83 ♠ A1043")) T eq)
(test (test-bid '(1 H) (str2hand "♣ 8 ♦ AQ ♥ AQJ83 ♠ AK1043")) nil eq) ; eligible for 2C
(test (test-bid '(2 C) (str2hand "♣ 8 ♦ AQ ♥ AQJ83 ♠ AK1043")) T eq) ; <-- so 2C

(test (test-bid '(1 C) (str2hand "♣ A32 ♦ AQ10 ♥ QJ83 ♠ 943")) T eq)
(test (test-bid '(1 D) (str2hand "♣ A32 ♦ AQ105 ♥ QJ8 ♠ 943")) T eq)
(test (test-bid '(1 D) (str2hand "♣ A10932 ♦ AQ105 ♥ QJ ♠ 94")) nil eq) ;more clubs than diamonds
(test (test-bid '(1 C) (str2hand "♣ A10932 ♦ AQ105 ♥ QJ ♠ 94")) T eq) ; <-- so 1C
(test (test-bid '(1 NT) (str2hand "♣ A32 ♦ AQ10 ♥ QJ83 ♠ KQ43")) nil eq) ; <-- too strong for 1NT
(test (test-bid '(2 NT) (str2hand "♣ A32 ♦ AQ10 ♥ QJ83 ♠ KQ43")) nil eq) ; <-- and too weak for 2NT
(test (test-bid '(1 C) (str2hand "♣ A32 ♦ AQ10 ♥ QJ83 ♠ KQ43")) T eq) ; <-- so 1C

(test (test-bid '(1 H) (str2hand "♣ 85 ♦ K10 ♥ AQJ1032 ♠ 943")) nil eq) ; <-- too weak for 1H
(test (test-bid '(2 H) (str2hand "♣ 85 ♦ K10 ♥ AQJ1032 ♠ 943")) T eq) ; <-- but ok for 2H
(test (test-bid '(3 H) (str2hand "♣ 85 ♦ K10 ♥ AQJ10932 ♠ 43")) T eq) ; <-- 7 for 3H
(test (test-bid '(3 H) (str2hand "♣ 85 ♦ 109 ♥ AQJ10932 ♠ 43")) T eq) ; <-- this is fine too

;; Picks the first matching call from a table based on assess-hand.
;; Returns the call itself (e.g., '(1 S)) or nil if nothing matches.
(defun choose-bid (hand &optional (openings openings))
    "Choose the best bid for a given hand based on a list of possible openings.
    The function evaluates each bid's conditions against the hand's characteristics."
    (let ((measures (assess-hand hand)))
        (car (find-if (lambda-dot (bid meaning)
                        (if (apply #'good-opening? (cons meaning measures)) bid))
                      openings))))

(test (choose-bid (str2hand "♣ KJ9753 ♦ 94 ♥ A84 ♠ A9")) '(1 C) equal)
(test (choose-bid (str2hand "♣ Q86 ♦ KJ8 ♥ AQ1094 ♠ Q3")) '(1 H) equal)
(test (choose-bid (str2hand "♣ AJ ♦ K73 ♥ K52 ♠ Q10942")) '(1 S) equal)
(test (choose-bid (str2hand "♣ Q1097642 ♦ Q98 ♥ 103 ♠ 7")) nil equal)
(test (choose-bid (str2hand "♣ AQ109762 ♦ Q98 ♥ 103 ♠ 7")) '(3 C) equal)
(test (choose-bid (str2hand "♣ K6 ♦ KQJ ♥ AK108 ♠ KQ62")) '(2 NT) equal)
(test (choose-bid (str2hand "♣ 72 ♦ KJ862 ♥  ♠ AQ8652")) '(1 S) equal)
(test (choose-bid (str2hand "♣ K103 ♦ 102 ♥ AKJ10862 ♠ J")) '(1 H) equal)
(test (choose-bid (str2hand "♣ AKQ10952 ♦ Q63 ♥ 9 ♠ K2")) '(1 C) equal)
(test (choose-bid (str2hand "♣ A95 ♦ 83 ♥ A4 ♠ 1076543")) nil equal)

;; Helper: build (and (>= value min) (<= value max)) form for use in tables.
(defun range (value range)
   `(and (>= ,value ,(first range)) (<= ,value ,(second range))))

;; Response table after 1NT/2NT opening. level=1 or 2.
;; Returns alist bid->rule; uses range, balanced and suit lengths.
(defun nt-responses (level)
    (let ((invite-range (if (= level 1) '(8 9) '(4 5)))
          (game-range (if (= level 1) '(10 15) '(6 10)))
          (slam-invite-range (if (= level 1) '(16 17) '(12 13)))
          (slam-range (if (= level 1) 18 14))
          (base-level (+ level 1)))
       `(((,base-level C) . (and (>= hcp ,(first invite-range))
                                 (or (= S 4) (= H 4))))
         ((,base-level D) . (>= H 5))
         ((,base-level H) . (>= S 5))
         ((,base-level S) . (and (or (< hcp ,(first invite-range))
                                     (> hcp ,(- (second game-range) 2)))
                                 (or (and (> hcp ,(first invite-range)) (>= C 5))
                                          (>= C 6))))
         ,@(if (= level 1) `(((3 C) . (and (or (< hcp 8) (> hcp 13)) 
                                           (or (and (>= hcp 7) (>= D 5)) (>= D 6))))
                             ((2 NT) . (and (>= hcp 8) (<= hcp 9)
                                       (< H 5) (< S 5)
                                       (< C 6) (< D 6)))))
         ((3 NT) . ,(range 'hcp game-range))
         ((4 NT) . ,(range 'hcp slam-invite-range))
         ((6 NT) . (>= hcp ,slam-range)))))

(test (choose-bid (str2hand "♣ 96 ♦ AK7654 ♥ K7 ♠ KQ5") (nt-responses 1)) '(3 C) equal)
(test (choose-bid (str2hand "♣ A106 ♦ AK6 ♥ K106542 ♠ 4") (nt-responses 1)) '(2 D) equal)
(test (choose-bid (str2hand "♣ 10 ♦ J42 ♥ Q10854 ♠ 10764") (nt-responses 1)) '(2 D) equal)
(test (choose-bid (str2hand "♣ 72 ♦ 864 ♥ K653 ♠ AK86") (nt-responses 1)) '(2 C) equal)
(test (choose-bid (str2hand "♣ A9753 ♦ AQ5 ♥ 72 ♠ J108") (nt-responses 1)) '(3 NT) equal)
(test (choose-bid (str2hand "♣ 10765 ♦ A872 ♥ 753 ♠ K9") (nt-responses 1)) nil equal)
(test (choose-bid (str2hand "♣ A95 ♦ 83 ♥ A4 ♠ 1076543") (nt-responses 1)) '(2 H) equal)
(test (choose-bid (str2hand "♣ Q864 ♦ 86 ♥ A1093 ♠ Q52") (nt-responses 1)) '(2 C) equal)
(test (choose-bid (str2hand "♣ AJ753 ♦ A75 ♥ 72 ♠ 1098") (nt-responses 1)) '(2 NT) equal)

(test (choose-bid (str2hand "♣ 9864 ♦ 86 ♥ A1093 ♠ 752") (nt-responses 2)) '(3 C) equal)
(test (choose-bid (str2hand "♣ 9864 ♦ 86 ♥ AK93 ♠ 752") (nt-responses 2)) '(3 C) equal)
(test (choose-bid (str2hand "♣ AJ753 ♦ A75 ♥ 72 ♠ 1098") (nt-responses 2)) '(3 S) equal)
(test (choose-bid (str2hand "♣ A10753 ♦ Q75 ♥ 72 ♠ 1098") (nt-responses 2)) '(3 NT) equal)

;; Opener's replies to Stayman (after 1NT -> 2C or 2NT -> 3C):
;; - Prefer 2H/3H with a 4-card heart suit (even with 4 spades).
;; - Else bid 2S/3S with a 4-card spade suit.
;; - Else deny a major with 2D/3D.
(defun stayman-responses (level)
  (let ((base-level (+ level 1)))
    `(((,base-level H) . (>= H 4))
      ((,base-level S) . (>= S 4))
      ((,base-level D) . (and (< H 4) (< S 4))))))

;; Responder's rebids after opener answers 2H/3H to Stayman (1NT/2NT context).
;; Covers:
;; - Fit in hearts: invite (3H) with 8-9 HCP, bid game (4H) with 10-15 HCP.
;; - No 4 hearts: 2NT invite (8-9 HCP) or 3NT sign-off (10-15 HCP).
;; - Minor 5-card with 4 spades: 3C/3D (natural) showing 5+ minor and 4 spades.

;; Responder's rebids after opener denies a major (2D/3D) to Stayman.
;; - No 4-card major: 2NT invite (8-9 HCP) or 3NT sign-off (10-15 HCP).

;; Responder's rebids after opener answers 2S/3S to Stayman (1NT/2NT context).
;; - Fit in spades: invite (3S) with 8-9 HCP, bid game (4S) with 10-15 HCP.
;; - No 4 spades: 2NT invite (8-9 HCP) or 3NT sign-off (10-15 HCP).
;; - Minor 5-card with 4 hearts: 3C/3D (natural) showing 5+ minor and 4 hearts.

;; Opener corrections after responder signs off in 3NT in a Stayman auction.
;; If opener holds both 4-card majors, correct to a major game (prefer spades).

;; Responses after a strong 2C opening: distinguish strength/shape and balanced vs unbalanced weak hands.
;; It assumes 2D response is an only strong response.
(defparameter 2C-responses
        '(((2 D) . (>= hcp 6))
          ((2 H) . (and (<= hcp 5) (>= H 4)))
          ((2 S) . (and (<= hcp 5) (>= S 4)))
          ((2 NT) . (and (<= hcp 5) balanced))
          ((3 C) . (and (<= hcp 5) (>= C 5) (not balanced)))
          ((3 D) . (and (<= hcp 5) (>= D 5) (not balanced)))))

(test (choose-bid (str2hand "♣ 10 ♦ J42 ♥ Q10854 ♠ 10764") 2C-responses) '(2 H) equal)
(test (choose-bid (str2hand "♣ 72 ♦ 864 ♥ K653 ♠ AK86") 2C-responses) '(2 D) equal)
(test (choose-bid (str2hand "♣ A9753 ♦ AQ5 ♥ 72 ♠ J108") 2C-responses) '(2 D) equal)
(test (choose-bid (str2hand "♣ 10765 ♦ A872 ♥ 753 ♠ K9") 2C-responses) '(2 D) equal)
(test (choose-bid (str2hand "♣ A95 ♦ 83 ♥ 104 ♠ 1076543") 2C-responses) '(2 S) equal)
(test (choose-bid (str2hand "♣ Q864 ♦ 86 ♥ A1093 ♠ Q52") 2C-responses) '(2 D) equal)
(test (choose-bid (str2hand "♣ AJ753 ♦ 875 ♥ 72 ♠ 1098") 2C-responses) '(2 NT) equal)
(test (choose-bid (str2hand "♣ AJ7532 ♦ 85 ♥ 72 ♠ 1098") 2C-responses) '(3 C) equal)

;; “Fit” predicate with partner’s opened suit; different thresholds for minors/majors.
(defun fit (suit)
    (cond ((eq suit 'C) '(>= C 5))
          ((eq suit 'D) '(>= D 4))
          (t `(>= ,suit 3))))

;; Responses after a 1-level suit opening (non-NT).
;; Produces an alist of calls with conditions based on hcp/fit/lack of cheaper suit bids.
(defun basic-responses (bid)
    (let* ((level (first bid))
           (open-suit (suitno (second bid)))
           (fit (fit (suitsym open-suit)))
           (no-biddable-4 (loop for suit from (+ open-suit 1) to 3
                                         collect `(< ,(suitsym suit) 4)))
           (no-lower-5 (loop for suit from 0 below open-suit
                             collect `(< ,(suitsym suit) 5))))
       (if (or (> level 1) (eq open-suit nil))
           (error "Wrong bid (only 1-suit bids accepted)"))

       (append (loop for suit from (+ open-suit 1) to 3
                     collect `((1 ,(suitsym suit)) . (and (>= hcp 6) (>= ,(suitsym suit) 4))))
               `(((1 NT) . (and (>= hcp 6) (<= hcp 10)
                                ,@no-biddable-4
                                (not ,fit))))
               (if (eq (suitsym open-suit) 'C)
                   `(((2 C) . (and (>= hcp 6) (<= hcp 9) (>= C 5) ,@no-biddable-4)))
                   `(((2 C) . (and (>= hcp 11) ,@no-biddable-4 ,@no-lower-5
                                   (or (not ,fit) (>= hcp 15))))))
               `(((2 ,(suitsym open-suit)) . (and (>= hcp 6) (<= hcp 9) ,fit 
                                                  ,@no-biddable-4))
                 ((3 ,(suitsym open-suit)) . (and (>= hcp 10) (<= hcp 12) ,fit 
                                                  ,@no-biddable-4 ,@no-lower-5)))
               (if (>= open-suit 2)
                   `(((4 ,(suitsym open-suit)) . (and (>= hcp 13) (<= hcp 15) ,fit))))
               (loop for suit-no in (filter (curry #'> open-suit) '(1 2 3))
                     collect `((2 ,(suitsym suit-no)) . (and (>= hcp 11) (>= ,(suitsym suit-no) 5)))))))

(test (choose-bid (str2hand "♣ AJ7532 ♦ J5 ♥ 72 ♠ 1098") (basic-responses '(1 H))) '(1 NT) equal)
(test (choose-bid (str2hand "♣ AJ7532 ♦ J5 ♥ 72 ♠ 1098") (basic-responses '(1 S))) '(2 S) equal)
(test (choose-bid (str2hand "♣ 72 ♦ KQJ4 ♥ K653 ♠ AK8") (basic-responses '(1 S))) '(2 C) equal)
(test (choose-bid (str2hand "♣ 72 ♦ KQJ ♥ K6532 ♠ AK8") (basic-responses '(1 S))) '(2 H) equal)
(test (choose-bid (str2hand "♣ 72 ♦ K854 ♥ K653 ♠ AK8") (basic-responses '(1 S))) '(4 S) equal)
(test (choose-bid (str2hand "♣ 96 ♦ AK7654 ♥ K7 ♠ KQ5") (basic-responses '(1 C))) '(1 D) equal)
(test (choose-bid (str2hand "♣ 96 ♦ AK765 ♥ K7 ♠ KQ52") (basic-responses '(1 H))) '(1 S) equal)

;; Inference and combination section:
;; From this point we start extracting and combining constraints inferred from both our and partner's
;; bidding as well as our hand evaluation. These helpers let us track what partner has revealed, what
;; we have already shown, and whether there is more informative bidding available in the current context.
;; Extractors from rule trees: find “less-than/not-greater-than” constraints for a given symbol.
(defun match-smaller (sym lst)
    (or (matchlist `(< ,sym _) (curry #'+ -1) lst)
        (matchlist `(<= ,sym _) #'id lst)))
        
;; “Greater-than/not-less-than” constraints for a given symbol.
(defun match-greater (sym lst)
    (or (matchlist `(> ,sym _) (curry #'+ 1) lst)
        (matchlist `(>= ,sym _) #'id lst)))
        
;; For suit-length constraints: return (suit, minimal length), preferring stricter bounds.
(defun match-longer (lst)
    (let ((ans (or (matchlist '(> _ _) (lambda (suit len) (list suit (+ len 1))) lst)
                   (matchlist '(>= _ _) #'id* lst))))
       (if (find (first ans) '(C D H S))
           ans)))
        
;; Combine non-nil conditions into (and ...); collapse to a single element when possible.
(defun and-condition (conds)
    (let ((significant (filter #'listp conds)))
        (if (> (length significant) 1) `(and ,@significant) (car significant))))

;; Merge two consecutive shapes (rules) made by the same player.
;; Keeps all constraints present in either shape; for the same measure chooses the more restrictive:
;; - for lower bounds (>/>=) pick the higher threshold; if equal, '>' is stricter than '>='
;; - for upper bounds (</<=) pick the lower threshold; if equal, '<' is stricter than '<='
(defun merge-shapes (a b)
    (labels ((extract-or-hcp-losers (form)
                (when (and (listp form) (eq (first form) 'or))
                  (let* ((subs (cdr form))
                         (hcp-lb (find-if (lambda (x)
                                            (and (listp x) (= (length x) 3)
                                                 (or (eq (first x) '>=) (eq (first x) '>))
                                                 (eq (second x) 'hcp)))
                                          subs))
                         (losers-ub (find-if (lambda (x)
                                               (and (listp x) (= (length x) 3)
                                                    (or (eq (first x) '<=) (eq (first x) '<))
                                                    (eq (second x) 'losers)))
                                             subs)))
                    (remove nil (list hcp-lb losers-ub)))))
             (clauses (shape)
                (cond ((not shape) nil)
                      ((and (listp shape) (eq (first shape) 'and))
                       (let ((acc nil))
                         (dolist (sub (filter #'listp (cdr shape)) (reverse acc))
                           (if (and (listp sub) (eq (first sub) 'or))
                               (let ((pair (extract-or-hcp-losers sub)))
                                 (when pair (setf acc (append pair acc))))
                               (push sub acc)))))
                      ((and (listp shape) (eq (first shape) 'or))
                       (extract-or-hcp-losers shape))
                      (t (list shape))))
             (lower-op? (op) (or (eq op '>=) (eq op '>)))
             (upper-op? (op) (or (eq op '<=) (eq op '<)))
             (pick-lower (x y)
                (cond ((not x) y)
                      ((not y) x)
                      (t (destructuring-bind (op1 var1 val1) x
                           (declare (ignore var1))
                           (destructuring-bind (op2 var2 val2) y
                             (declare (ignore var2))
                             (cond ((> val1 val2) x)
                                   ((< val1 val2) y)
                                   (t (if (and (eq op1 '>) (not (eq op2 '>))) x
                                          (if (and (eq op2 '>) (not (eq op1 '>))) y
                                              x)))))))))
             (pick-upper (x y)
                (cond ((not x) y)
                      ((not y) x)
                      (t (destructuring-bind (op1 var1 val1) x
                           (declare (ignore var1))
                           (destructuring-bind (op2 var2 val2) y
                             (declare (ignore var2))
                             (cond ((< val1 val2) x)
                                   ((> val1 val2) y)
                                   (t (if (and (eq op1 '<) (not (eq op2 '<))) x
                                          (if (and (eq op2 '<) (not (eq op1 '<))) y
                                              x))))))))))
        (let ((lowers nil) (uppers nil) (others nil) (order nil))
            (dolist (cl (append (clauses a) (clauses b)))
                (if (and (listp cl) (= (length cl) 3) (symbolp (first cl)))
                    (let ((op (first cl)) (var (second cl)) (val (third cl)))
                        (declare (ignore val))
                        (cond ((lower-op? op)
                               (let ((cell (assoc var lowers)))
                                 (if cell
                                     (setf (cdr cell) (pick-lower (cdr cell) cl))
                                     (progn (push (cons var cl) lowers)
                                            (pushnew var order :test #'eq)))))
                              ((upper-op? op)
                               (let ((cell (assoc var uppers)))
                                 (if cell
                                     (setf (cdr cell) (pick-upper (cdr cell) cl))
                                     (progn (push (cons var cl) uppers)
                                            (pushnew var order :test #'eq)))))
                              (t (unless (find cl others :test #'equal)
                                   (setf others (append others (list cl)))))))
                    (unless (find cl others :test #'equal)
                        (setf others (append others (list cl))))))
            (let* ((vars (reverse order))
                   (lb-hcp (cdr (assoc 'hcp lowers)))
                   (ub-hcp (cdr (assoc 'hcp uppers)))
                   (lb-losers (cdr (assoc 'losers lowers)))
                   (ub-losers (cdr (assoc 'losers uppers)))
                   (special-or (build-or lb-hcp ub-losers))
                   (bounds (apply #'append
                                  (mapcar (lambda (v)
                                            (cond ((eq v 'hcp)
                                                   (remove nil (list ub-hcp)))
                                                  ((eq v 'losers)
                                                   (remove nil (list lb-losers)))
                                                  (t
                                                   (let ((lb (cdr (assoc v lowers)))
                                                         (ub (cdr (assoc v uppers))))
                                                     (remove nil (list lb ub))))))
                                          vars)))
                   (merged (append others (remove nil (list special-or)) bounds)))
              (cond ((null merged) nil)
                    ((= (length merged) 1) (car merged))
                    (t `(and ,@merged)))))))

(test (merge-shapes '(and (>= hcp 12) (<= hcp 22) (>= S 5))
                    '(and (>= hcp 10) (>= H 5)))
      '(and (>= hcp 12) (<= hcp 22) (>= S 5) (>= H 5))
      equal)

(test (merge-shapes '(and (>= hcp 12) (<= hcp 22) (>= S 5))
                    '(and (>= hcp 15) (<= hcp 17) (>= S 3)))
      '(and (>= hcp 15) (<= hcp 17) (>= S 5))
      equal)

(test (merge-shapes '(and (>= hcp 12))
                    '(and (> hcp 12) (<= S 5)))
      '(and (> hcp 12) (<= S 5))
      equal)

;; Drop all ors from combined shapes. They would be misleading for further-bid.
(test (merge-shapes '(and (>= hcp 8) (or (>= h 4) (>= s 4)))
                    '(>= s 4))
      '(and (>= hcp 8) (>= s 4))
      equal)

;; Preserve special OR for (>= hcp _) or (<= losers _), even when mixed with other constraints.
(test (merge-shapes '(or (>= hcp 12) (<= losers 7))
                    '(>= S 4))
      '(and (or (>= hcp 12) (<= losers 7)) (>= S 4))
      equal)

;; Prefer stricter bounds when both shapes contribute special-OR components, with >/< handled too.
(test (merge-shapes '(or (> hcp 14) (< losers 6))
                    '(and (>= hcp 12) (<= losers 8)))
      '(or (> hcp 14) (< losers 6))
      equal)

;; If the two clauses come from separate shapes, the result still groups them under OR.
(test (merge-shapes '(>= hcp 12)
                    '(<= losers 7))
      '(or (>= hcp 12) (<= losers 7))
      equal)

;; > and < variants should also be grouped and preserved.
(test (merge-shapes '(> hcp 11)
                    '(< losers 8))
      '(or (> hcp 11) (< losers 8))
      equal)

;; Helpers to traverse rule trees (AND/OR or single comparisons).
(defun find-in-bid (bid-meaning fn)
    (letcar bid-meaning
        (if (find head '(AND OR))
            (find-if #'id (mapcar fn tail))
            (funcall fn bid-meaning))))

(test (find-in-bid '(and (or (>= hcp 10) (<= losers 8))) 
                   (curry #'matchlist '(or (>= hcp _) (<= losers _)) #'id*))
      '(10 8)
      equal)

(defun filter-bid (bid-meaning fn)
    (letcar bid-meaning
        (if (find head '(AND OR))
            (filter #'id (mapcar fn tail))
            (apply fn bid-meaning))))

;; Extract useful metadata from shape definitions:
;; - max-hcp/min-hcp: strength bounds
;; - longers: long suits for fit seeking
(defun max-hcp (bid-meaning)
    (find-in-bid bid-meaning (curry #'match-smaller 'hcp))) 

(defun min-hcp (bid-meaning)
    (find-in-bid bid-meaning (orf (curry #'match-greater 'hcp)
                                  (curry #'matchlist '(or (>= hcp _) (_ losers _))
                                                     (lambda (hcp &rest _) 
                                                        (declare (ignore _)) 
                                                        hcp))
                                  (curry #'matchlist '(or (> hcp _) (_ losers _))
                                                     (lambda (hcp &rest _) 
                                                        (declare (ignore _)) 
                                                        (+ hcp 1))))))

(test (min-hcp '(and (or (>= hcp 18) (< losers 6)) (> c 5)))
      18 eq)

(defun longers (bid-meaning)
    (filter-bid bid-meaning (curry #'match-longer)))

;; Compute target “game” contract for a given suit.
(defun game-in (suit)
    (cond ((not suit) '(3 NT))
          ((find suit '(C D)) (list 5 suit))
          (T (list 4 suit))))

;; Compute target “invite” contract for a given suit.
(defun invite-in (suit)
    (cond ((or (not suit) (eq suit 'nt)) '(2 NT))
          ((find suit '(C D)) (list 4 suit))
          (T (list 3 suit))))

;; Bidding arithmetic to measure distance/jump between bids, treating NT as the highest suit (4).
(defun suitno* (suit)
    (cond ((not suit) 4)
          ((eq suit 'NT) 4)
          (t (suitno suit))))

;; Closest legal call in the given suit relative to the base bid.
(defun closest-bid (suit base)
    (if (> (suitno suit) (suitno* (second base)))
        (list (first base) suit)
        (list (+ (first base) 1) suit)))

;; Jump by the given number of “levels” relative to the base bid.
(defun jump-bid (suit base levels)
    (if (> (suitno suit) (suitno* (second base)))
        (list (+ (first base) levels) suit)
        (list (+ (first base) levels 1) suit)))

;; Jump difference between base and a concrete bid (accounts for suit order).
(defun bid-jump (base bid)
    (let ((level-diff (- (first bid) (first base)))
          (suit-diff (if (> (suitno* (second bid)) (suitno* (second base))) 0 -1)))
       (+ level-diff suit-diff)))

(test (bid-jump '(1 S) '(2 S)) 0 eq)
(test (bid-jump '(1 S) '(2 D)) 0 eq)
(test (bid-jump '(1 C) '(2 D)) 1 eq)
(test (bid-jump '(1 C) '(3 D)) 2 eq)
(test (bid-jump '(2 S) '(2 H)) -1 eq)

(defun propagate-from-previous (extract set lst)
    "extract: function f(e) -> v, set: function g(v, e) -> e', lst: a list
     returns a list with values extracted from previous elements are set into next"
    (labels ((walk (prev rest &optional acc)
                (if (not rest) (reverse acc)
                    (letcar rest
                        (walk head tail (cons (funcall set (funcall extract prev) head)
                                              acc))))))
        (if lst (cons (car lst) (walk (car lst) (cdr lst))))))

(test (propagate-from-previous #'id #'+ '(1 2 3 4 5))
      '(1 3 5 7 9)
      equal)

;; Build (bid . rule) pair for suit continuations: conditions on hcp/losers
;; and minimum fit vs known partner length.
;; Note: (- 8 len) — we aim for an 8+ card fit across the partnership (if suit applies)
(defun biddef (bid min-hcp max-hcp min-losers max-losers len)
    (let ((suit (second bid)))
        (cons bid (build-and (build-or (if min-hcp `(>= hcp ,min-hcp))
                                       (if (and len max-losers) `(<= losers ,max-losers)))
                             (if max-hcp `(<= hcp ,max-hcp))
                             (if (and len min-losers) `(>= losers ,min-losers))
                             (if len `(>= ,suit ,(- 8 len)))))))

(test (biddef '(2 C) 22 nil nil nil nil) '((2 C) . (>= hcp 22)) equal)
(test (biddef '(1 NT) 15 17 nil nil nil) '((1 NT) . (and (>= hcp 15) (<= hcp 17))) equal)
(test (biddef '(1 S) 12 22 4 7 3)
      '((1 S) . (and (or (>= hcp 12) (<= losers 7)) (<= hcp 22) (>= losers 4) (>= S 5)))
      equal)

(defun hcp-tricks (hcp)
    (floor (/ hcp 3)))

(defun hcp-losers (hcp)
    (+ 7 (floor (/ (- 12 hcp) 3))))
    
;; Further-bid generator:
;; First we create context object containing:
;;    + known-shape: our bounds (e.g., '(and (>= hcp 12) ...))
;;    + partner-shape: minimum assumptions inferred from partner’s previous bid
;; Then we call next-bid function:
;;    + context
;;    + last-bid: partner’s last call, used for jump calculations
;; Output:
;; - alist mapping bids to rules, covering: game, invite, raises, closest fit, natural slams

(defclass bidding-context ()
    ((our-shape :initarg :our :reader our-shape)
     (partner-shape :initarg :partner :reader partner-shape)))

(defmethod print-object((this bidding-context) out)
    (with-slots (our-shape partner-shape) this
        (format out "BIDDING-CONTEXT<~A/~A>" our-shape partner-shape)))

(defmethod established? ((this bidding-context) suit)
    (let ((my (find-in-bid (our-shape this) (curry #'match-greater suit)))
          (partner (find-in-bid (partner-shape this) (curry #'match-greater suit))))
        (and my partner (>= (+ my partner) 8))))

(test (established? (make-instance 'bidding-context :our '(and (>= hcp 12) (>= S 5))
                                                    :partner '(and (>= hcp 6) (>= S 3)))
                    'S)
      T eq)

(test (established? (make-instance 'bidding-context :our '(and (>= hcp 12) (>= S 5))
                                                    :partner '(and (>= hcp 6) (>= S 3)))
                    'H)
      nil eq)

(test (established? (make-instance 'bidding-context :our '(>= H 4)
                                                    :partner '(> hcp 12))
                    'H)
      nil eq)

(defmethod bid-ladder ((this bidding-context) last-bid &optional (suit 'nt) length)
    (with-results ((min-hcp (partner-shape this))
                   (max-hcp (partner-shape this)))
        (let ((my-min (min-hcp (our-shape this)))
              (free-levels (bid-jump last-bid (invite-in suit))))
            (labels ((grade (level min max)
                        (biddef level min max
                                     (if max (hcp-losers max)) (hcp-losers min) 
                                     (if suit length)))
                     (milestone (level)
                      (let* ((hcp (+ 25 (* 3 (- level 4)) (if (not (eq suit 'nt) )0 3)))
                             (losers (- 7 level 1)))
                         `(,(biddef (list level suit)
                                         (- hcp min-hcp) nil
                                         nil (+ losers (hcp-tricks min-hcp))
                                         (if suit length))
                           ,@(if (and (or (< level 6) (> (suitno* suit) 1))
                                      (or (not max-hcp) (> (- max-hcp min-hcp) 1)))
                                 `(,(biddef (list (- level (if (and (eq suit 'nt)
                                                                    (= level 6))
                                                               2 1))
                                                  suit)
                                            (- hcp min-hcp 2) nil
                                            nil
                                            (+ losers (hcp-tricks min-hcp) 1)
                                            (if suit length))))))))
               (propagate-from-previous
                   (if (eq suit 'nt) (lambda (entry) (min-hcp (cdr entry)))
                                     (f* #'cdr 
                                         (curry* #'find-in-bid (e)
                                             (e (curry #'matchlist '(or (>= hcp _) (<= losers _)) #'id*)))))
                   (if (eq suit 'nt)
                       (lambda (lim entry)
                           (let ((bid (car entry)) (rule (cdr entry)))
                                (cons bid (build-and rule (list '< 'hcp lim)))))
                       (lambda (lim entry)
                           (let ((bid (car entry)) (rule (cdr entry)))
                                (cons bid (append rule (if (first lim) `((< hcp ,(first lim))))
                                                       (if (second lim) `((> losers ,(second lim)))))))))
                 `(,@(milestone 6)
                   ,@(milestone (cond ((< (suitno* suit) 2) 5)
                                      ((< (suitno* suit) 4) 4)
                                      (t 3)))
                   ,@(if (= free-levels 2)
                         `(,(grade (jump-bid suit last-bid 1) (+ my-min 6) nil)))
                   ,@(if (>= free-levels 1)
                         (if (or max-hcp (eq suit (second last-bid)))
                             `(,(grade (jump-bid suit last-bid 0) my-min nil))
                             `(,(grade (jump-bid suit last-bid 0) (+ my-min 3) nil))))))))))

(let ((ctx (make-instance 'bidding-context :our '(and (>= hcp 6) (> s 4)) 
                                           :partner '(and (>= hcp 12) (<= hcp 14) (>= c 3)))))
    (test (bid-ladder ctx '(1 NT))
          '(((6 NT) >= HCP 22) ((4 NT) AND (>= HCP 20) (< HCP 22))
            ((3 NT) AND (>= HCP 13) (< HCP 20)) ((2 NT) AND (>= HCP 11) (< HCP 13)))
          equal)
    (test (bid-ladder ctx '(1 NT) 'c 3)
          '(((6 C) AND (OR (>= HCP 19) (<= LOSERS 4)) (>= C 5))
            ((5 C) AND (OR (>= HCP 16) (<= LOSERS 5)) (>= C 5) (< HCP 19) (> LOSERS 4))
            ((4 C) AND (OR (>= HCP 14) (<= LOSERS 6)) (>= C 5) (< HCP 16) (> LOSERS 5))
            ((3 C) AND (OR (>= HCP 12) (<= LOSERS 7)) (>= C 5) (< HCP 14) (> LOSERS 6))
            ((2 C) AND (OR (>= HCP 6) (<= LOSERS 9)) (>= C 5) (< HCP 12) (> LOSERS 7)))
          equal))

(defun below-4-lev (x)
    (let ((lvl (seektree '(0 0) x))) (and lvl (< lvl 4))))

(defmethod new-suit-bids ((this bidding-context) last-bid)
    (with-slots (our-shape partner-shape) this
      (let* ((my-min (min-hcp our-shape))
             (last-suit (second last-bid)))
        (filter #'below-4-lev
          (loop for s in (if (< (suitno* last-suit) 3)
                             (roll (- (+ (suitno* last-suit) 1)) '(C D H S))
                             '(C D H S))
                unless (or (eq s last-suit)
                           (find-in-bid our-shape (curry #'match-greater s)))
                collect (let* ((partner-max (find-in-bid partner-shape (curry #'match-smaller s)))
                               (need (max 4 (if partner-max (- 8 partner-max) 4)))
                               (bid (closest-bid s last-bid))
                               (enter-new-level (> (first bid) (first last-bid)))
                               (fit-with-last (established? this last-suit))
                               (hc-thresh (and my-min (if (or enter-new-level fit-with-last)
                                                          (+ my-min 3) nil)))
                               (rule (build-and `(>= ,s ,need)
                                                (if hc-thresh `(>= hcp ,hc-thresh)))))
                          (and rule (cons bid rule))))))))

(defmethod extend-suit-bids ((this bidding-context) last-bid)
    (with-slots (our-shape partner-shape) this
      (let* ((my-min (min-hcp our-shape)))
        (filter #'below-4-lev
          (apply #'append
            (loop for s in '(C D H S)
                  for prev-len = (find-in-bid our-shape (curry #'match-greater s))
                  when prev-len
                  collect
                    (let* ((min-extra (+ prev-len 1))
                           (cheap (closest-bid s last-bid))
                           (jump (jump-bid s last-bid 1))
                           (cheap-rule (build-and `(>= ,s ,min-extra)
                                                  (if my-min `(<= hcp ,(+ my-min 5)))))
                           (jump-rule (build-and `(>= ,s ,min-extra)
                                                 (if my-min `(>= hcp ,(+ my-min 6))))))
                      (remove nil
                        (list (and jump-rule (cons jump jump-rule))
                              (and cheap-rule (cons cheap cheap-rule)))))))))))

(defmethod next-bid ((this bidding-context) last-bid)
    (with-slots (our-shape partner-shape) this
        (with-results ((longers partner-shape))
            (filter (f* #'first (curry #'bid-jump last-bid) (curry #'<= 0))
                (append
                    (new-suit-bids this last-bid)
                    (apply #'append (mapcar (curry #'apply (curry #'bid-ladder this last-bid)) longers))
                    (extend-suit-bids this last-bid)
                    (bid-ladder this last-bid))))))

(defun further-bid (our partner last-bid)
    (next-bid (make-instance 'bidding-context :our our :partner partner) last-bid))

(test (choose-bid (str2hand "♣ 9 ♦ AK7654 ♥ K8 ♠ KJ105")
                  (further-bid '(and (>= hcp 12) (<= hcp 22) (>= D 5))
                               '(and (>= hcp 6) (>= S 4))
                               '(1 S)))
      '(3 S)
      equal)

(test (choose-bid (str2hand "♣ A ♦ AK7654 ♥ K7 ♠ KJ105")
                  (further-bid '(and (>= hcp 12) (<= hcp 22) (>= D 5))
                               '(and (> hcp 5) (>= S 4))
                               '(1 S)))
      '(4 S)
      equal)
      
(test (choose-bid (str2hand "♣ Q54 ♦ AK106 ♥ QJ ♠ KQ105")
                  (further-bid '(and (>= hcp 12) (<= hcp 22) (>= D 4))
                               '(and (>= hcp 6) (>= S 4))
                               '(1 S)))
      '(3 S)
      equal)

(test (choose-bid (str2hand "♣ 954 ♦ AK76 ♥ 107 ♠ KQ105")
                  (further-bid '(and (>= hcp 12) (<= hcp 22) (>= D 5))
                               '(and (>= hcp 6) (>= S 4))
                               '(1 S)))
      '(2 S)
      equal)
      
(test (choose-bid (str2hand "♣ J954 ♦ 76 ♥ Q107 ♠ A1095")
                  (further-bid '(and (>= hcp 6) (>= S 4))
                               '(and (>= hcp 12) (<= hcp 22) (>= D 5) (>= S 4))
                               '(2 S)))
      nil
      equal)

(test (choose-bid (str2hand "♣ AJ95 ♦ 72 ♥ Q107 ♠ A1095")
                  (further-bid '(and (>= hcp 6) (>= S 4))
                               '(and (>= hcp 12) (<= hcp 22) (>= D 5) (>= S 4))
                               '(2 S)))
      '(3 C)
      equal)

(test (choose-bid (str2hand "♣ 1093 ♦ K6 ♥ 10872 ♠ AJ109")
                  (further-bid '(>= hcp 8)
                               '(and balanced (>= hcp 15) (<= hcp 17) (>= h 4))
                               '(2 H)))
      '(2 S)
      equal)
      
;; Test that further bid choses only from biddable answers
(test (choose-bid (str2hand "♣ AJ9 ♦ K86 ♥ Q107 ♠ AJ109")
                  (further-bid '(and balanced (>= hcp 15) (<= hcp 17))
                               '(and (>= hcp 8) (<= hcp 14) (>= h 4))
                               '(3 NT)))
      nil
      equal)

;; Bidding scheme selection based on bidding sequence (history-aware).
(defun bidding-scheme-for (bids meanings)
    (cond ((equal bids '((1 NT))) (nt-responses 1))
          ((equal bids '((2 NT))) (nt-responses 2))
          ((and (= (length bids) 1) (eq (seektree '(0 0) bids) 1))
              (basic-responses (first bids)))
          ((equal bids '((2 C))) 2c-responses)
          ((equal bids '((1 NT) (2 C)))
              (stayman-responses 1))
          ((equal bids '((2 NT) (2 C)))
              (stayman-responses 2))
          (t (further-bid (first meanings) (second meanings) (car (last bids))))))

;; Simple bidding state machine (bot vs. empty seats).
;; - deal: four hands (N,E,S,W)
;; - bids: last bidding sequence
;; - meanings: interpretation of the last call
;; - bid-scheme: current table (openings/responses/further-bid)
(defclass bidding ()
    ((deal :initarg :deal)
     (bids :initform nil)
     (meanings :initform (list nil nil))
     (bid-scheme :initform openings)))

(defmethod print-object((this bidding) out)
    (with-slots (meanings bids) this
        (format out "BIDDING<~A : ~A>" meanings bids)))

;; Advance to the next call:
;; - Scan (N,E,S,W) for the first hand matching the current scheme.
;; - Rotate the deal (roll), update history and meanings.
;; - Choose the next response scheme based on the call (2C/NT/1x/else->further-bid).
(defmethod next ((this bidding))
    (with-slots (deal bids meanings bid-scheme) this
        (labels ((apply-bid (call)
                   (setf bids (append bids (list call)))
                   (when call
                     (let ((our-shape (first meanings))
                           (partner-shape (second meanings))
                           (bid-meaning (assoc call bid-scheme)))
                       (setf meanings (list partner-shape
                                            (merge-shapes our-shape (cdr bid-meaning))))
                       (setf bid-scheme (bidding-scheme-for (remove nil bids) meanings))))
                   (setf deal (roll -1 deal))
                   call))
          (let* ((last-call (car (last bids)))
                 (third-ago (and (>= (length bids) 3)
                                 (nth (- (length bids) 3) bids)))
                 (auto-pass (or last-call third-ago)))
            (if auto-pass
                (apply-bid nil)
                (apply-bid (choose-bid (first deal) bid-scheme)))))))

(defmethod drain ((this bidding) &optional acc)
    (let ((res (next this)))
        (let ((new-acc (cons res acc)))
          (if (and (> (length new-acc) 3)
                   (null (first new-acc))
                   (null (second new-acc))
                   (null (third new-acc)))
              (reverse new-acc)
              (drain this new-acc)))))
    
(test (drain (make-instance 'bidding
                    :deal (list (str2hand "N: ♠ Q1097 ♥ 3 ♦ KJ1063 ♣ K102")
                                (str2hand "E: ♠ 62 ♥ AJ75 ♦ A942 ♣ 987")
                                (str2hand "S: ♠ AKJ5 ♥ K1064 ♦ Q87 ♣ A6")
                                (str2hand "W: ♠ 843 ♥ Q982 ♦ 5 ♣ QJ543"))))
      '(nil nil (1 NT) nil (2 C) nil (2 H) nil (2 S) nil (4 S) nil nil nil) 
      equal)

(test (drain (make-instance 'bidding
                    :deal (list (str2hand "N: ♠ AQ1097 ♥ 32 ♦ KQJ10 ♣ K10")
                                (str2hand "E: ♠ K62 ♥ AJ7654 ♦ A ♣ 9872")
                                (str2hand "S: ♠ J853 ♥ KQ10 ♦ 987 ♣ AJ6")
                                (str2hand "W: ♠ 4 ♥ 98 ♦ 65432 ♣ Q543"))))
      '((1 S) nil (3 S) nil (4 S) nil nil nil)
      equal)
