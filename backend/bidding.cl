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
         (< (length (filter (curry #'> 3) power)) 2)))
        
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
                   ((2 D) . (and (> hcp 5) (or (> loosers 5) (< hcp 11)) (= D 6)))
                   ((2 H) . (and (> hcp 5) (or (> loosers 5) (< hcp 11)) (= H 6)))
                   ((2 S) . (and (> hcp 5) (or (> loosers 5) (< hcp 11)) (= S 6)))
                   ((3 C) . (and (> hcp 5) (< hcp 11) (>= C 7)))
                   ((3 D) . (and (> hcp 5) (< hcp 11) (>= D 7)))
                   ((3 H) . (and (> hcp 5) (< hcp 11) (>= H 7)))
                   ((3 S) . (and (> hcp 5) (< hcp 11) (>= S 7)))
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

(defun choose-bid (hand)
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

