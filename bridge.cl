; Copyright (C) 2025  Bartosz "Lew" Pastudzki
 
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

(defmacro test (form result test)
  `(let ((ans ,form))
     (if (not (,test ans ,result))
       (format t "TEST FAILED: ~A != ~A" ans ,result))))

(defmacro let-from (lst names &body body)
    (let ((len (length names)))
        `(let ,(loop for i from 0 to (- len 1)
                     collect (list (nth i names)
                                   (nth i lst)))
              ,@body)))

(defmacro let-from* (lst names &body body)
    (let ((len (length names)))
        `(let ,(loop for i from 0 to (- len 1)
                     collect (list (nth i names)
                                   `(nth ,i ,lst)))
              ,@body)))

(defmacro letcar (lst &body body)
    `(let ((head (car ,lst))
           (tail (cdr ,lst)))
        ,@body))
    
(defmacro let-from! (exp names &body body)
    (let ((len (length names)))
        `(let ((ans ,exp))
            (let ,(loop for i from 0 to (- len 1)
                        collect (list (nth i names)
                                     `(nth ,i ans)))
                 ,@body))))

(defmacro peek (body)
    `(let ((ans ,body))
        (format T "> ~A = ~A~%" ',body ans)
        ans))
        
(defun card (x)
    (let* ((asstr (format nil "~a" x))
           (suit (read-from-string (subseq asstr 0 1)))
           (rank (position (read-from-string (subseq asstr 1))
                           '(2 3 4 5 6 7 8 9 10 J Q K A))))
        (and suit rank (list suit rank))))

(test (card "S10") '(s 8) equal)
(test (card "CK") '(c 11) equal)

(defun print-card (x)
    (format nil "~A~A" (first x) (nth (second x)
                                      '(2 3 4 5 6 7 8 9 10 J Q K A))))
(defun all-cards ()
    (apply 'append (loop for suit in '(c d h s)
                         collect (loop for rank from 0 to 12 collect (list suit rank)))))

(defun take (idx lst)
    (let ((x (nth idx lst))
          (hd (subseq lst 0 idx))
          (tl (subseq lst (+ idx 1))))
        (and x (list x (append hd tl)))))

(test (take 3 '(1 2 3 4 5)) '(4 (1 2 3 5)) equal)
(test (take 0 '(1 2 10 100)) '(1 (2 10 100)) equal)
(test (take 2 '(1 2 3)) '(3 (1 2)) equal)

; take x random elements from lst
; return list of: chosen elements list and list of remaining elements
(defun deal (amount lst &optional acc)
    (if (<= amount 0) (list lst acc)
        (let ((once (take (random (length lst)) lst)))
            (let-from* once (card rest)
                (deal (- amount 1) rest (cons card acc))))))

; Comparison function for sorting cards
(defun suit<> (a b)
    (let ((an (position a '(c d h s)))
          (bn (position b '(c d h s))))
        (- an bn)))

(test (< (suit<> 'c 'd) 0) T eq)
(test (> (suit<> 'h 'c) 0) T eq)
(test (= (suit<> 'c 'c) 0) T eq)

(defun card<> (a b)
    (let-from* a (as ar)
        (let-from* b (bs br)
            (let ((ds (suit<> as bs)))
                (if (= ds 0) (- ar br) ds)))))

(test (> (card<> '(s 10) '(c 1)) 0) T eq)
(test (< (card<> '(s 10) '(s 11)) 0) T eq)
(test (> (card<> '(c 2) '(c 1)) 0) T eq)
(test (= (card<> '(h 1) '(h 1)) 0) T eq)

(defun card< (a b)
    (< (card<> a b) 0))

(defun card> (a b)
    (> (card<> a b ) 0))

; create a hand in form of list of lists containing ranks in 4 suits
(defun hand (cards &optional (acc '(()()()())))
    (if cards (let-from* (car cards) (s r)
                  (let ((sn (position s '(c d h s))))
                      (hand (cdr cards)
                            (loop for i from 0 to 3
                                  collect (if (= i sn) (cons r (nth i acc))
                                                       (nth i acc))))))
              (loop for x in acc
                    collect (sort x #'<))))

; use deal function to create number of amount-element
; subsets, so that nothing is left.
(defun deal-all (amount lst &optional acc)
    (if (<= (length lst) 0) acc
        (let-from! (deal amount lst) (rest hand)
            (deal-all amount rest (cons hand acc)))))

; pretty print for deal (result of deal-all function)
(defun print-deal (hands &optional (names '(n e s w)))
    (loop for name in names
          for hand in hands
          do (format t "~A: ~A~%" name (hand hand))))

; create a histogram from list    
(defun histogram (lists &optional acc (total 0))
    (if (not lists) (mapcar (lambda (x) (list (first x) (float (/ (second x) total))))
                            (sort acc (lambda (a b)
                                          (> (second a) (second b)))))
        (let ((p (position-if (lambda (x)
                                (let-from* x (val)
                                    (equal val (car lists))))
                              acc)))
            (histogram (cdr lists) 
                       (if p (loop for i from 0
                                   for x in acc
                                   collect (list (car x) (if (= i p) (+ (second x) 1) (second x))))
                             (cons (list (car lists) 1) acc))
                       (+ total 1)))))

(defun merge-by (merger list &optional acc)
    (if (not list) acc
        (letcar list
            (labels ((self (x lst &optional acc)
                        (if lst (let ((merged (funcall merger x (car lst))))
                                    (if merged (append acc (cons merged (cdr lst)))
                                               (self x (cdr lst) (append acc (list (car lst)))))))))
                (let ((newacc (self head acc)))
                    (if newacc (merge-by merger tail newacc)
                               (merge-by merger tail (cons head acc))))))))

(defun car* (x)
    (if (listp x) (car x) x))

(defun histmerge (a b)
    (if (and a b)
        (let-from* a (aval afreq)
            (let-from* b (bval bfreq bchildren)
                (if bchildren
                    (if (equal (car aval) bval)
                        (list (car aval) (+ afreq bfreq)
                              (cons (list (cdr aval) afreq) bchildren)))
                    (if (equal (car aval) (car bval))
                        (list (car aval) (+ afreq bfreq)
                              (list (list (cdr aval) afreq) (list (cdr bval) bfreq)))))))))

(test (histmerge '((a b c) 0.1) '(a 0.1 (((a b) 0.05)
                                         ((c b) 0.05))))
      '(A 0.2 (((B C) 0.1) ((A B) 0.05) ((C B) 0.05))) equal)

(test (histmerge '(((2 3 4 4) SUPPORT) 0.09506197)
                 '(((2 3 4 4) OPEN) 0.06947221))
      '((2 3 4 4) 0.16453418 (((SUPPORT) 0.09506197) ((OPEN) 0.06947221)))
      equal)

(test (histmerge '(((2 3 4 4) EMPTY) 0.04818073)
                 '((2 3 4 4) 0.16453418 (((SUPPORT) 0.09506197)((OPEN) 0.06947221))))
      '((2 3 4 4) 0.21271491
         (((EMPTY) 0.04818073) ((SUPPORT) 0.09506197) ((OPEN) 0.06947221)))
      equal)


(test (merge-by #'histmerge '(((a b c) 0.7)
                              ((a b d) 0.3)
                              ((c d f) 0.1)))
      '(((C D F) 0.1) (A 1.0 (((B D) 0.3) ((B C) 0.7)))) equal)

(defun hcp-val (rank)
    (if (> rank 8) (- rank 8) 0))

(defun strength (hcp)
    (cond ((< hcp 7) 'empty)
          ((< hcp 12) 'support)
          ((<= hcp 18) 'open)
          ((> hcp 18) 'strong)))

(defun suit-distribution (hand)
    (sort (mapcar #'length hand) #'<))

(defun suit-strength (hand)
    (strength (apply #'+ (mapcar (lambda (lst) (apply #'+ (mapcar #'hcp-val lst))) hand))))

(defun f-cons (&rest functions)
    (lambda (&rest input)
        (mapcar (lambda (f) (apply f input))
                functions)))

(defun mc-case (measures &key (volume 2500))
    (merge-by #'histmerge
        (histogram
            (apply #'append
                (loop for i from 0 to volume
                      collect (mapcar (apply #'f-cons measures)
                                      (mapcar #'hand (deal-all 13 (all-cards)))))))))

(mc-case '(suit-strength suit-distributions))
