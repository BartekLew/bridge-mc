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

; Simple version of Loosing Trick Count evaluation

(defun suit-loosers (ranks)
    (+ (if (or (find 12 ranks) (= (length ranks) 0)) 0 1)
       (if (or (find 11 ranks) (< (length ranks) 2)) 0 1)
       (if (or (find 10 ranks) (< (length ranks) 3)) 0 1)))

(test (suit-loosers '(12 2)) 1 eq)
(test (suit-loosers '(12 11 6 5 4)) 1 eq)
(test (suit-loosers '()) 0 eq)
(test (suit-loosers '(11 10)) 1 eq)
(test (suit-loosers '(10 9 8 7)) 2 eq)
(test (suit-loosers '(12 10 9 8 7)) 1 eq)
(test (suit-loosers '(7)) 1 eq)

(defun assess-hand (hand &optional measure)
    (let ((vals (list (mapcar #'length (suits hand))
                      (mapcar #'suit-hcp (suits hand))
                      (apply #'+ (mapcar #'suit-loosers (suits hand))))))
       (if measure (apply measure vals)
           vals)))

(defun balanced? (lens power loosers)
    (declare (ignore loosers))
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

(defun good-opening? (rules lens power loosers)
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
                    (loosers . ,loosers)
                    (balanced . (balanced? ',lens ',power nil)))
                  rules)))

(defparameter openings 
                 '(((2 C) . (or (> hcp 22)
                                (and (not balanced)
                                     (cond ((find-if (curry #'< 4) (subseq lens 2 4)) (<= loosers 4))
                                           ((find-if (curry #'< 4) (subseq lens 0 2)) (<= loosers 3))))))
                   ((2 NT) . (and balanced (not (find-if (curry #'> 3) power))
                                  (> hcp 18)))
                   ((1 NT) . (and balanced (> hcp 14) (< hcp 18)))
                   ((1 S) . (and (or (< loosers 6) (> hcp 11))
                                 (> loosers 4) (< hcp 23)
                                 (>= S 5)
                                 (not (and (> hcp 16) (find-if (curry #'< 4) (subseq lens 0 3))))))
                   ((1 H) . (and (or (< loosers 6) (> hcp 11))
                                 (> loosers 4) (< hcp 23)
                                 (>= H 5)
                                 (not (and (> hcp 16) (find-if (curry #'< 4) (subseq lens 0 2))))))
                   ((1 D) . (and (or (< loosers 6) (> hcp 11))
                                 (> loosers 3) (< hcp 23)
                                 (>= D 4) (>= D C)
                                 (not (and (> hcp 16) (>= C 5)))))
                   ((1 C) . (and (or (< loosers 6) (> hcp 11))
                                 (> loosers 3) (< hcp 23)
                                 (>= C 3)))
                   ((2 D) . (and (> hcp 5) (>= Dpower 5) (or (> loosers 5) (< hcp 11)) (= D 6)))
                   ((2 H) . (and (> hcp 5) (>= Hpower 5) (or (> loosers 5) (< hcp 11)) (= H 6)))
                   ((2 S) . (and (> hcp 5) (>= Spower 5) (or (> loosers 5) (< hcp 11)) (= S 6)))
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
(test (test-bid '(2 NT) (str2hand "♣ AQJ5 ♦ AKQ8 ♥ 1093 ♠ K43")) nil eq) ; <- missing hearts
(test (test-bid '(1 C) (str2hand "♣ AQJ5 ♦ AKQ8 ♥ 1093 ♠ K43")) T eq)    ; <- so 1C
(test (test-bid '(1 D) (str2hand "♣ AQJ5 ♦ AKQ8 ♥ 1093 ♠ K43")) T eq)    ; <- or 1D
(test (test-bid '(1 NT) (str2hand "♣ AQJ5 ♦ AQ108 ♥ 1093 ♠ K43")) T eq)

(test (test-bid '(1 S) (str2hand "♣ 852 ♦ AQ ♥ AQJ3 ♠ A10943")) T eq)
(test (test-bid '(1 S) (str2hand "♣ 852 ♦ A10 ♥ AQJ103 ♠ A10943")) T eq)
(test (test-bid '(1 S) (str2hand "♣ 852 ♦ AQ ♥ AK1053 ♠ A10943")) NIL eq) ; eligible for reverse
(test (test-bid '(1 H) (str2hand "♣ 852 ♦ AQ ♥ AK1053 ♠ A10943")) T eq) ; <- so 1H is ok
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

(defun choose-bid (hand &optional (openings openings))
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

(defun range (value range)
   `(and (>= ,value ,(first range)) (<= ,value ,(second range))))

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

(defun fit (suit)
    (cond ((eq suit 'C) '(>= C 5))
          ((eq suit 'D) '(>= D 4))
          (t `(>= ,suit 3))))

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
                                                  ,@no-biddable-4 ,no-lower-5)))
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

(defun match-smaller (sym lst)
    (or (matchlist `(< ,sym _) (curry #'+ -1) lst)
        (matchlist `(<= ,sym _) #'id lst)))
        
(defun match-greater (sym lst)
    (or (matchlist `(> ,sym _) (curry #'+ 1) lst)
        (matchlist `(>= ,sym _) #'id lst)))
        
(defun match-longer (lst)
    (let ((ans (or (matchlist '(> _ _) (lambda* (suit len) (list suit (+ len 1))) lst)
                   (matchlist '(>= _ _) #'id* lst))))
       (if (find (first ans) '(C D H S))
           ans)))
        
(defun and-condition (conds)
    (let ((significant (filter #'id conds)))
        (if (> (length significant) 1) `(and ,@significant) (car significant))))

(defun combine-shapes (a b)
    (labels ((self (base other)
                (cond ((not base) nil)
                      ((eq (first base) 'and)
                          (if (eq (first other) 'and)
                              (and-condition
                                      (mapcar (lambda* (op measure val)
                                                 (let* ((extractor (if (find op '(> >=)) #'match-greater
                                                                                    #'match-smaller))
                                                        (otherval (find-if #'id
                                                                        (mapcar (curry extractor measure)
                                                                                (cdr other)))))
                                                    (if otherval `(,op ,measure ,(+ val otherval)))))
                                              (cdr base)))
                              (self base `(and ,other))))
                      (t (self `(and ,base) other)))))
         (self a b)))

(test (combine-shapes '(and (>= hcp 12) (<= hcp 22) (>= S 5))
                      '(and (>= hcp 10) (>= H 5)))
      '(>= hcp 22) 
      equal)

(test (combine-shapes '(and (>= hcp 12) (<= hcp 22) (>= S 5))
                      '(and (>= hcp 6) (<= hcp 10) (>= S 3)))
      '(and (>= hcp 18) (<= hcp 32) (>= S 8))
      equal)

(test (combine-shapes '(and (>= hcp 12) (<= hcp 22) (>= S 5))
                      '(and (>= hcp 10) (or (>= hcp 15) (<= S 2))))
      '(>= hcp 22)
      equal)

(defun find-in-bid (bid-meaning fn)
    (letcar bid-meaning
        (if (find head '(AND OR))
            (find-if #'id (mapcar fn tail))
            (apply fn bid-meaning))))

(defun filter-bid (bid-meaning fn)
    (letcar bid-meaning
        (if (find head '(AND OR))
            (filter #'id (mapcar fn tail))
            (apply fn bid-meaning))))

(defun max-hcp (bid-meaning)
    (find-in-bid bid-meaning (curry #'match-smaller 'hcp))) 

(defun min-hcp (bid-meaning)
    (find-in-bid bid-meaning (curry #'match-greater 'hcp)))

(defun longers (bid-meaning)
    (filter-bid bid-meaning (curry #'match-longer)))

(defun game-in (suit)
    (cond ((not suit) '(3 NT))
          ((find suit '(C D)) (list 5 suit))
          (T (list 4 suit))))

(defun invite-in (suit)
    (cond ((not suit) '(2 NT))
          ((find suit '(C D)) (list 4 suit))
          (T (list 3 suit))))

(defun suitno* (suit)
    (cond ((not suit) 4)
          ((eq suit 'NT) 4)
          (t (suitno suit))))

(defun closest-bid (suit base)
    (if (> (suitno suit) (suitno* (second base)))
        (list (first base) suit)
        (list (+ (first base) 1) suit)))

(defun jump-bid (suit base levels)
    (if (> (suitno suit) (suitno* (second base)))
        (list (+ (first base) levels) suit)
        (list (+ (first base) levels 1) suit)))

(defun bid-jump (base bid)
    (let ((level-diff (- (first bid) (first base)))
          (suit-diff (if (> (suitno* (second bid)) (suitno* (second base))) 0 -1)))
       (+ level-diff suit-diff)))

(test (bid-jump '(1 S) '(2 S)) 0 eq)
(test (bid-jump '(1 S) '(2 D)) 0 eq)
(test (bid-jump '(1 C) '(2 D)) 1 eq)
(test (bid-jump '(1 C) '(3 D)) 2 eq)
(test (bid-jump '(2 S) '(2 H)) -1 eq)

(defun suit-biddef (bid min-hcp max-hcp min-losers max-losers len)
    (let ((suit (second bid)))
        (cons bid `(and (or (>= hcp ,min-hcp)
                            (<= loosers ,max-losers))
                        (<= hcp ,max-hcp)
                        (>= loosers ,min-losers)
                        (>= ,suit ,(- 8 len))))))

(defun further-bid (known-shape partner-shape last-bid)
    (let ((my-min (min-hcp known-shape)))
        (with-results ((min-hcp partner-shape)
                       (longers partner-shape))
            `(,@(loop for longer in longers
                      append (let-from* longer (suit len)
                                (let ((game-hcp (if (find suit '(C D)) 27 25))
                                      (game-loosers (if (find suit '(C D)) 2 3))
                                      (invite-jump (bid-jump last-bid (invite-in suit))))
                                  `(,(suit-biddef (game-in suit)
                                                  (- game-hcp min-hcp) (- 29 min-hcp)
                                                  (+ 2 (floor (/ min-hcp 3)))
                                                  (+ game-loosers (floor (/ min-hcp 3)))
                                                  len)
                                    ,(if (> invite-jump 1)
                                         (suit-biddef (invite-in suit)
                                                      (- game-hcp min-hcp 2) (- game-hcp min-hcp)
                                                      (+ 3 (floor (/ min-hcp 3)))
                                                      (+ game-loosers 1 (floor (/ min-hcp 3)))
                                                      len))
                                     ,(if (>= invite-jump 0)
                                          (suit-biddef (jump-bid suit last-bid (if (= invite-jump 0) 0 1))
                                                       (+ my-min 3) (- game-hcp min-hcp 1)
                                                       (+ 3 (floor (/ min-hcp 3)))
                                                       (+ game-loosers 1 (floor (/ min-hcp 3)))
                                                       len))
                                     ,(suit-biddef `(5 ,suit)
                                                   (- 31 min-hcp 2) (- 30 min-hcp)
                                                   (+ 1 (floor (/ min-hcp 3)))
                                                   (+ 1 (floor (/ min-hcp 3)))
                                                   len)
                                     ,@(if (or (not (eq suit (second last-bid)))
                                               (= (first last-bid) 1))
                                         `((,(closest-bid suit last-bid) . (>= ,suit ,(- 8 len)))))
                                                            ))))))))

(test (choose-bid (str2hand "♣ 9 ♦ AK7654 ♥ K7 ♠ KQ105")
                  (further-bid '(and (>= hcp 12) (<= hcp 22) (>= D 5))
                               '(and (>= hcp 6) (>= S 4))
                               '(1 S)))
      '(4 S)
      equal)

(test (choose-bid (str2hand "♣ A ♦ AK7654 ♥ K7 ♠ KQ105")
                  (further-bid '(and (>= hcp 12) (<= hcp 22) (>= D 5))
                               '(and (>= hcp 6) (>= S 4))
                               '(1 S)))
      '(5 S)
      equal)
      
(test (choose-bid (str2hand "♣ 954 ♦ AK106 ♥ QJ ♠ KQ105")
                  (further-bid '(and (>= hcp 12) (<= hcp 22) (>= D 5))
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

(test (choose-bid (str2hand "♣ AJ954 ♦ 76 ♥ Q107 ♠ A1095")
                  (further-bid '(and (>= hcp 6) (>= S 4))
                               '(and (>= hcp 12) (<= hcp 22) (>= D 5) (>= S 4))
                               '(2 S)))
      '(3 S)
      equal)

(defclass bidding ()
    ((deal :initarg deal)
     (bids :initform nil)
     (meanings :initform nil)
     (bid-scheme :initform openings)))

(defmethod next ((this bidding))
    (with-slots (deal bids meanings bid-scheme) this
        (let-from! (fold (lambda (acc val)
                            (if acc acc
                              (let ((bid (choose-bid (nth val deal) bid-scheme)))
                                (if bid (list val bid)))))
                         nil
                         '(0 1 2 3))
                   (passes bid)
            (when passes
                (setf deal (roll (- passes) deal))
                (setf bids (list bid))
                (setf meanings (list (assoc bid bid-scheme) nil))
                (setf bid-scheme (cond ((equal bid '(2 c)) 2c-responses)
                                       ((equal (second bid) 'nt) (nt-responses (first bid)))
                                       ((eq (first bid) 1) (basic-responses bid))
                                       (nil (further-bid nil (fourth meanings) bid))))
                bids))))

(defparameter b (make-instance 'bidding
                    :deal (list (str2hand "N: ♠ Q1097 ♥ 3 ♦ KJ1063 ♣ K102")
                                (str2hand "E: ♠ 62 ♥ AJ75 ♦ A942 ♣ 987")
                                (str2hand "S: ♠ AKJ5 ♥ K1064 ♦ Q87 ♣ A6")
                                (str2hand "W: ♠ 843 ♥ Q982 ♦ 5 ♣ QJ543"))))
(next b)
