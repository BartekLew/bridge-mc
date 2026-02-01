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

; This module covers duplicate bridge scoring.

(defun scanstr (opts str)
    (if (not opts) (list nil str)
        (let ((opt (format nil "~A" (car opts))))
            (if (and (>= (length str) (length opt))
                     (string= (string-downcase opt) (string-downcase (subseq str 0 (length opt)))))
                (list (car opts) (subseq str (length opt)))
                (scanstr (cdr opts) str)))))

(test (scanstr '(1 2 3 4 5 6 7) "5NTx")
      '(5 "NTx")
      equal)

(test (scanstr '(♣ ♦ ♥ ♠) "♥x+1")
      '(♥ "x+1")
      equal)

(test (scanstr '(x xx) "x+1")
      '(x "+1")
      equal)

(defclass contract ()
    ((level :reader level)
     (suit :reader suit)
     (declarer :reader declarer)
     (double? :reader double?)
     (outcome :reader outcome)))

(defmethod initialize-instance ((this contract) &key =)
  (let-from! (scanstr '(1 2 3 4 5 6 7) =)
             (level* after-level)
     (let-from! (scanstr '(C D H S NT ♣ ♦ ♥ ♠) after-level)
                (suit* after-suit)
        (let-from! (scanstr '(N E S W) after-suit)
                   (declarer* after-declarer)
          (let-from! (scanstr '(xx x) after-declarer)
                     (double* after-double)
            (let-from! (scanstr '(= + -) (or after-double after-suit))
                       (state after-state)
               (let ((outcome* (if (and state (not (eq state '=)))
                                  (let ((num (read-from-string after-state)))
                                      (if num (format nil "~A~A" state num)))
                                  '=)))
                  (with-slots (level suit declarer double? outcome) this
                    (setf level level*)
                    (setf suit (let ((suit** (cdr (assoc suit* '((c . ♣) (d . ♦) (h . ♥) (s . ♠))))))
                                  (if suit** suit** suit*)))
                    (setf declarer declarer*)
                    (setf double? double*)
                    (setf outcome outcome*)))))))))
                    
(defmethod print-object ((this contract) o)
    (with-slots (level suit declarer double? outcome) this
        (format o "~A~A~A~A~A" level suit (or declarer "") 
                                 (if double? (string-downcase (format nil "~A" double?)) "")
                                 (or outcome ""))))

(test (format nil "~A" (make-instance 'contract := "3NTx+1"))
      "3NTx+1"
      string=)

(test (format nil "~A" (make-instance 'contract := "6CEx="))
      "6♣Ex="
      string=)

(defmethod base-score ((this contract) &key vulnerable)
    (with-slots (level suit declarer double? outcome) this
        (* (contract-score suit (+ level 6 (if (eq outcome '=) 0
                                               (read-from-string outcome)))
                          :level level :double double?
                          :vulnerable vulnerable)
           (if (find declarer '(e w)) -1 1))))

(test (base-score (make-instance 'contract := "3NTE="))
      -400 eq)

(test (base-score (make-instance 'contract := "3NTN+1") :vulnerable T)
      630 eq)

(test (base-score (make-instance 'contract := "6♥W="))
      -980 eq)
 
(test (base-score (make-instance 'contract := "6♥Wx-2"))
      300 eq)

(test (base-score (make-instance 'contract := "6♥Nxx-2"))
      -600 eq)
            
(test (base-score (make-instance 'contract := "4HSx=") :vulnerable T)
      790 eq)

(test (base-score (make-instance 'contract  := "4CEx+1") :vulnerable T)
      -910 eq)

(defparameter *imp-pts* '(20 50 90 130 170 220 270 320 370 430 500 
                          600 750 900 1100 1300 1500 1750 2000 2300 2500 
                          3000 3500 4000))

(defmethod match-points ((a contract) (b contract) &key vulnerable)
    (let ((diff (- (base-score a :vulnerable (find vulnerable '(ns both)))
                   (base-score b :vulnerable (find vulnerable '(ew both))))))
        (* (position-if (curry #'< (abs diff)) *imp-pts*)
           (if (< diff 0) -1 1))))

(test (match-points (make-instance 'contract := "4HS=")
                    (make-instance 'contract := "4SEx-2"))
      3 eq)

(test (match-points (make-instance 'contract := "1NTN=")
                    (make-instance 'contract := "2DS+1"))
      -1 eq)

(test (match-points (make-instance 'contract := "4♥Nx-2")
                    (make-instance 'contract := "3♦E=") :vulnerable 'both)
      -9 eq)

