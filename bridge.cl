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

;; =============================
;; GENERAL UTILS
;; =============================

(defun id (x) x)

(defun car* (x)
    (if (and (listp x) (= (length x) 1)) (car x) x))

(defun ensure-list (x)
    (if (listp x) x (list x)))

(defmacro test (form result test)
  `(let ((ans ,form))
     (if (not (,test ans ,result))
       (format t "TEST FAILED: ~A~%   != ~A~%" ans ,result))))

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

(defmacro let-from! (exp names &body body)
    (let ((len (length names)))
        `(let ((ans ,exp))
            (let ,(loop for i from 0 to (- len 1)
                        collect (list (nth i names)
                                     `(nth ,i ans)))
                 ,@body))))

(defmacro letcar (lst &body body)
    `(let ((head (car ,lst))
           (tail (cdr ,lst)))
        ,@body))
    
(defmacro peek (body)
    `(let ((ans ,body))
        (format T "> ~A~%  = ~A~%" ',body ans)
        ans))
        
(defun filter (pred lst &optional acc)
    (if (not lst) (reverse acc)
        (letcar lst
            (filter pred tail (if (funcall pred head) (cons head acc) acc)))))

(defun curry (fun &rest base-args)
    (lambda (&rest args)
        (apply fun (append base-args args))))

(defun f* (&rest funs)
    (lambda (&rest args)
        (let ((ans nil))
            (loop for f in funs
                  do (setf ans (if ans (funcall f ans)
                                   (apply f args))))
            ans)))

(test (filter (curry #'< 3) '(1 2 3 4 5 4 3 2 1)) '(4 5 4) equal)

(defun reorder (lst indexes)
    (loop for i in indexes
          collect (nth i lst)))

(test (reorder '(5 10 15 21) '(3 0 1))
      '(21 5 10)
      equal)

(defun seektree (path tree)
    (if path (seektree (cdr path) (nth (car path) tree))
             tree))

(test (seektree '(2 1) '((1 2 4) (5 6 73) (3 (32 1))))
     '(32 1)
     equal)

(defun push-pos (lst pos element)
    (loop for i from 0 to (max (- (length lst) 1) pos)
          collect (if (= pos i) (cons element (nth i lst))
                      (nth i lst))))

(test (push-pos '() 2 5)
      '(() () (5))
      equal)

(test (push-pos '() 0 3)
      '((3))
      equal)

(test (push-pos '(() (1 2) (3 4)) 0 3)
      '((3) (1 2) (3 4))
      equal)

(test (push-pos '((1 2 3) (4 5 6)) 0 4)
      '((4 1 2 3) (4 5 6))
      equal)

(defun randcar (lst)
    (if lst
        (let ((i (random (length lst))))
            (nth i lst))))

(defun max? (&rest args)
    (if args (apply #'max args)))

(defun min? (&rest args)
    (if args (apply #'min args) 13))

(defun randcar-weights (lst)
    (let* ((total-weight (apply #'+ (mapcar #'first lst)))
           (weight (random total-weight)))
        (labels ((self (weight rest)
                    (letcar rest
                        (if (< weight (first head)) (second head)
                                                    (self (- weight (first head)) tail)))))
            (if (> total-weight 0) (self weight lst)))))

(loop for y from 0 to 1000
      do (let ((sut (loop for x from 0 to 160
                          collect (randcar-weights '((1 1) (5 2) (10 3))))))
            (let ((sum (apply #'+ sut)))
                (test (position-if (lambda (x) (> x 3)) sut) nil eq)
                (test (and (> sum 380) (< sum 440)) T eq))))
            
(defun zip (&rest lists)
    (labels ((self (lists &optional acc)
                (if (not (car lists)) (reverse acc)
                    (self (loop for l in lists
                                collect (cdr l))
                          (cons (loop for l in lists
                                      collect (car l))
                                acc)))))
        (self lists)))

(test (zip '(1 2 3 4) '(4 5 6 7) '(8 9 10 11))
      '((1 4 8) (2 5 9) (3 6 10) (4 7 11))
      equal)

(defun fold (fn val lst)
    (if (not lst) val
        (fold fn (apply fn (list val (car lst))) (cdr lst))))

(defun andf (&rest funs)
    (lambda (&rest args)
        (fold (lambda (acc fn)
                 (if acc (apply fn args)))
              T
              funs)))

(test (funcall (andf (curry #'> 10) (curry #'< 5)) 7)
      T eq)

(test (funcall (andf (curry #'> 10) (curry #'< 5)) 11)
      NIL eq)

(defun zip-id (lst)
    (loop for x in lst
          for i from 0
          collect (list i x)))

(defun zip-weight (f list)
    (zip (mapcar f list)
         list))

(defun wrand (weights)
    (let ((sum (apply #'+ weights)))
        (if (eq (type-of sum) 'ratio)
            (let ((d (denominator sum)))
                (setf sum (* sum d))
                (loop for x in weights
                      for i from 0
                      do (setf (nth i weights) (* (nth i weights) d)))))
 
        (let ((target (random sum)))
            (labels ((self (ws tg i)
                           (letcar ws
                                   (if (< tg head) i
                                       (self tail (- tg head) (+ i 1))))))
                (self weights target 0)))))

(defun roll (amount lst)
    (let ((a (mod (- amount) (length lst))))
        (if (= a 0) lst
            (append (subseq lst a) (subseq lst 0 a)))))

(test (roll 2 '(10 100 15 5))
      '(15 5 10 100)
      equal)

(test (roll -1 '(10 100 15 5))
      '(100 15 5 10)
      equal)

(test (roll 3 '(10 100 15 5))
      '(100 15 5 10)
      equal)

(test (roll -3 '(10 100 15 5))
      '(5 10 100 15)
      equal)

(defun xor (a b)
    (or (and a (not b)) (and (not a) b)))

(test (xor T nil) T eq)
(test (xor nil T) T eq)
(test (xor T T) nil eq)
(test (xor nil nil) nil eq)

(defun flatten (lst)
    (fold (lambda (acc val)
              (if (listp val) (append acc val)
                              (append acc (list val))))
           nil lst))

(test (flatten '(1 foo (bar baz) 4 5 (6 8)))
      '(1 foo bar baz 4 5 6 8)
      equal)

(defun mod-or-push (mod init lst val &optional acc)
    (if lst (let ((ans (funcall mod (car lst) val)))
                (if ans (append acc (cons ans (cdr lst)))
                        (mod-or-push mod init (cdr lst) val (append acc (list (car lst))))))
            (append acc (list (funcall init val)))))

(test (mod-or-push (lambda (ref x) (if (= ref x) (* x 2)))
                   #'id
                   '(1 4 5 6 10)
                   1)
      '(2 4 5 6 10)
      equal)

(test (mod-or-push (lambda (ref x) (if (= ref x) (* x 2)))
                   #'id
                   '(1 4 5 6 10)
                   7)
      '(1 4 5 6 10 7)
      equal)

(test (mod-or-push (lambda (ref x) (if (= ref x) (* x 2)))
                   (lambda (x) `(,x 1))
                   '() 1)
      '((1 1)) equal)

(test (fold (curry #'mod-or-push (lambda (ref x) (if (= (car ref) x) `(,x ,(+ (second ref) 1))))
                                 (lambda (x) `(,x 1)))
            nil
            '(1 2 5 1 5 2 10))
      '((1 2) (2 2) (5 2) (10 1))
      equal)

(defun getfork (a b &optional acc)
    (cond ((or (not a) (not b)) (list acc a b))
          ((equal (first a) (first b)) (getfork (cdr a) (cdr b) (append acc (list (car a)))))
          (t (list acc a b))))
 
(test (getfork '(a b 1 5) '(a b 6 5))
     '((a b) (1 5) (6 5))
     equal)

(test (getfork '(a b c) '(d e f))
      '(() (a b c) (d e f))
      equal)

(test (getfork '(a b c) '(a b c))
      '((a b c) () ())
      equal)

;; ===========================
;; Cards and printing them
;; ===========================

(defun card (x)
    (let* ((asstr (format nil "~a" x))
           (suit (read-from-string (subseq asstr 0 1)))
           (rank (position (read-from-string (subseq asstr 1))
                           '(2 3 4 5 6 7 8 9 10 J Q K A))))
     (and suit rank (list suit rank))))

(defun cardstr (c)
    (format nil "~A~A" (nth (first c) '("♣" "♦" "♥" "♠"))
                       (nth (second c) '(2 3 4 5 6 7 8 9 10 J Q K A))))

(test (card "S10") '(s 8) equal)
(test (card "CK") '(c 11) equal)

(defun print-card (x)
    (format nil "~A~A" (first x) (nth (second x)
                                      '(2 3 4 5 6 7 8 9 10 J Q K A))))
(defun all-cards ()
    (apply 'append (loop for suit in '(c d h s)
                         collect (loop for rank from 0 to 12 collect (list suit rank)))))

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

(defun unhand (hand)
    (apply #'append (loop for suit in '(C D H S)
                          for cards in hand
                          collect (loop for card in cards
                                        collect (list suit (position card 
                                                                    '(2 3 4 5 6 7 8 9 10 J Q K A)))))))
    

(test (unhand '((2 3 A K) (2) (Q J) (10)))
      '((C 0) (C 1) (C 12) (C 11) (D 0) (H 10) (H 9) (S 8))
      equal)

(defun print-hand (hand)
    (apply #'format (append (list nil "~A ~A ~A ~A")
                            (loop for suit in '("♣" "♦" "♥" "♠")
                                  for cards in (mapcar (curry #'mapcar (lambda (x) 
                                                                (nth x '(2 3 4 5 6 7 8 9 10 J Q K A))))
                                                       (mapcar (lambda (s) (sort (copy-list s) #'>))
                                                               hand))
                                  collect (format nil "~A ~{~A~}" suit cards)))))

; pretty print for deal (result of deal-all function)
(defun print-deal (hands &optional (names '(n e s w)) (out t))
    (loop for name in names
          for hand in hands
          do (format out "~A: ~A~%" name 
                     (print-hand hand))))

;; ======================
;; DEALING CARDS
;; ======================
;; Generally we stick to structure (rest hand). Whenever we deal a card
;; or cards, we return them with the list of remaining cards BEFORE

(define-condition bad-index (error)
    ((val :initarg :=
          :reader val)
     (max-idx :initarg :/
              :reader max-idx)))

(defun take (idx lst)
    (let ((len (length lst)))
        (if (>= idx len) 
            (error 'bad-index := idx :/ len)))
    (let ((x (nth idx lst))
          (hd (subseq lst 0 idx))
          (tl (subseq lst (+ idx 1))))
        (list x (append hd tl))))

(test (take 3 '(1 2 3 4 5)) '(4 (1 2 3 5)) equal)
(test (take 0 '(1 2 10 100)) '(1 (2 10 100)) equal)
(test (take 2 '(1 2 3)) '(3 (1 2)) equal)

(defun take-match (cond lst)
    (let ((ans (position-if cond lst)))
        (if ans (take ans lst))))
    
; take x random elements from lst
; return list of: chosen elements list and list of remaining elements
(defun deal (amount lst &optional acc)
    (if (<= amount 0) (list lst acc)
        (let ((once (take (random (length lst)) lst)))
            (let-from* once (card rest)
                (deal (- amount 1) rest (cons card acc))))))


; use deal function to create number of amount-element
; subsets, so that nothing is left.
(defun deal-all (amount lst &optional acc)
    (if (<= (length lst) 0) acc
        (let-from! (deal amount lst) (rest hand)
            (deal-all amount rest (cons hand acc)))))

(defun remove-cards (cards deck)
    (list cards
          (remove-if (lambda (x) (find x cards :test #'equal)) deck)))

(test (remove-cards '((S 2) (S 3) (S 10)) '((S 2) (S 3) (S 4) (S 10) (H 2) (H 11)))
      '(((S 2) (S 3) (S 10)) ((S 4) (H 2) (H 11))) equal)

(defun rank-hcp (rank)
    (if (> rank 8) (- rank 8) 0))

(defun card-hcp (card)
    (rank-hcp (second card)))

(defun suitno (suitsym) 
    (position suitsym '(c d h s)))

(defun card-suitno (card)
    (suitno (car card)))

(defun cards-by (classifier rest &optional acc)
    (if rest
        (letcar rest
            (let ((val (funcall classifier head)))
                (cards-by classifier tail (push-pos acc val head))))
    acc))

(test (cards-by #'card-hcp (all-cards))
      '(((S 8) (S 7) (S 6) (S 5) (S 4) (S 3) (S 2) (S 1) (S 0) (H 8) (H 7) (H 6)
        (H 5) (H 4) (H 3) (H 2) (H 1) (H 0) (D 8) (D 7) (D 6) (D 5) (D 4) (D 3)
        (D 2) (D 1) (D 0) (C 8) (C 7) (C 6) (C 5) (C 4) (C 3) (C 2) (C 1) (C 0))
        ((S 9) (H 9) (D 9) (C 9)) ((S 10) (H 10) (D 10) (C 10))
        ((S 11) (H 11) (D 11) (C 11)) ((S 12) (H 12) (D 12) (C 12)))
      equal)

(test (cards-by #'card-suitno 
                '((C 1) (C 3) (C 4) (C 7) (C 9) (C 10) (C 11) (D 0) (D 1) (D 2) (D 3) (D 4)
                  (D 5) (D 6) (D 7) (D 8) (D 9) (D 10) (D 11) (D 12) (H 0) (H 1) (H 2) (H 3)
                  (H 4) (H 6) (H 7) (H 8) (H 9) (H 10) (H 11) (H 12) (S 0) (S 4) (S 5) (S 6)
                  (S 7) (S 8) (S 9)))
      '(((C 11) (C 10) (C 9) (C 7) (C 4) (C 3) (C 1))
        ((D 12) (D 11) (D 10) (D 9) (D 8) (D 7) (D 6) (D 5) (D 4) (D 3) (D 2) (D 1) (D 0))
        ((H 12) (H 11) (H 10) (H 9) (H 8) (H 7) (H 6) (H 4) (H 3) (H 2) (H 1) (H 0))
        ((S 9) (S 8) (S 7) (S 6) (S 5) (S 4) (S 0)))
    equal)

(test (cards-by #'card-suitno
                '((C 1) (H 6) (C 7) (D 10) (S 5) (C 10) (S 7) (H 2) (C 2) (H 9) (S 12) (H 12) (S 8)))
      '(((C 2) (C 10) (C 7) (C 1))
        ((D 10))
        ((H 12) (H 9) (H 2) (H 6))
        ((S 8) (S 12) (S 7) (S 5)))
      equal)

(defun strength (hcp)
    (cond ((< hcp 7) 'empty)
          ((< hcp 12) 'support)
          ((<= hcp 18) 'open)
          ((> hcp 18) 'strong)))

; When building hands for given HCP we need to  decide what combinations
; of ranks are valid to achieve exact HCP value
(defun permuts (n max &optional acc)
    (if (not acc) (permuts n max (list (loop for i from 1 to n collect 0)))
        (labels ((inc (rest &optional acc)
                    (if rest (let ((h (car rest)))
                                 (if (<= (+ h 1) max) 
                                    (append acc (cons (+ h 1) (cdr rest)))
                                    (let ((r+ (inc (cdr rest))))
                                        (if r+ (append (loop for x in acc
                                                             collect 0)
                                                       (cons 0 r+)))))))))
                                     
            (let ((next (inc (car acc))))
                (if next (permuts n max (cons next acc))
                    acc)))))

(defun valid-hcp-sums (value &key (test #'=) supply)
    (filter (lambda (lst) (and (funcall test (apply #'+ (loop for x in lst
                                                              for i from 1
                                                              collect (* i x)))
                                             value)
                               (or (not supply) 
                                   (not (position nil (loop for x in lst
                                                        for y in supply
                                                        collect (<= x y)))))))
            (permuts 4 4)))
    
(test (valid-hcp-sums 10) 
      '((0 1 0 2) (2 0 0 2) (0 0 2 1) (1 1 1 1) (3 0 1 1) (0 3 0 1) (2 2 0 1)
        (4 1 0 1) (1 0 3 0) (0 2 2 0) (2 1 2 0) (4 0 2 0) (1 3 1 0) (3 2 1 0)
        (2 4 0 0) (4 3 0 0)) equal)

(test (valid-hcp-sums 0) '((0 0 0 0)) equal)

(test (valid-hcp-sums 20) 
      '((1 0 1 4) (0 2 0 4) (2 1 0 4) (4 0 0 4) (0 1 2 3) (2 0 2 3) (1 2 1 3)
        (3 1 1 3) (0 4 0 3) (2 3 0 3) (4 2 0 3) (0 0 4 2) (1 1 3 2) (3 0 3 2)
        (0 3 2 2) (2 2 2 2) (4 1 2 2) (1 4 1 2) (3 3 1 2) (4 4 0 2) (0 2 4 1)
        (2 1 4 1) (4 0 4 1) (1 3 3 1) (3 2 3 1) (2 4 2 1) (4 3 2 1) (0 4 4 0)
        (2 3 4 0) (4 2 4 0) (3 4 3 0))
      equal)

(test (valid-hcp-sums 13)
      '((1 0 0 3) (0 1 1 2) (2 0 1 2) (1 2 0 2) (3 1 0 2) (0 0 3 1) (1 1 2 1)
        (3 0 2 1) (0 3 1 1) (2 2 1 1) (4 1 1 1) (1 4 0 1) (3 3 0 1) (1 0 4 0)
        (0 2 3 0) (2 1 3 0) (4 0 3 0) (1 3 2 0) (3 2 2 0) (2 4 1 0) (4 3 1 0))
      equal)

(test (valid-hcp-sums 13 :supply '(4 1 0 4))
      '((1 0 0 3) (3 1 0 2))
      equal)

(test (valid-hcp-sums 10 :test #'<)
      '((1 0 0 2) (0 0 0 2) (0 1 1 1) (2 0 1 1) (1 0 1 1) (0 0 1 1) (1 2 0 1)
        (0 2 0 1) (3 1 0 1) (2 1 0 1) (1 1 0 1) (0 1 0 1) (4 0 0 1) (3 0 0 1)
        (2 0 0 1) (1 0 0 1) (0 0 0 1) (0 0 3 0) (1 1 2 0) (0 1 2 0) (3 0 2 0)
        (2 0 2 0) (1 0 2 0) (0 0 2 0) (0 3 1 0) (2 2 1 0) (1 2 1 0) (0 2 1 0)
        (4 1 1 0) (3 1 1 0) (2 1 1 0) (1 1 1 0) (0 1 1 0) (4 0 1 0) (3 0 1 0)
        (2 0 1 0) (1 0 1 0) (0 0 1 0) (1 4 0 0) (0 4 0 0) (3 3 0 0) (2 3 0 0)
        (1 3 0 0) (0 3 0 0) (4 2 0 0) (3 2 0 0) (2 2 0 0) (1 2 0 0) (0 2 0 0)
        (4 1 0 0) (3 1 0 0) (2 1 0 0) (1 1 0 0) (0 1 0 0) (4 0 0 0) (3 0 0 0)
        (2 0 0 0) (1 0 0 0) (0 0 0 0))
      equal)
 
(test (valid-hcp-sums 20 :test #'>= :supply '(0 2 2 4))
      '((0 2 2 4) (0 1 2 4) (0 0 2 4) (0 2 1 4) (0 1 1 4) (0 2 0 4) (0 2 2 3)
        (0 1 2 3))
      equal)

(defun zip-deals (deals &optional acc)
    (if (not deals) acc
        (let-from* acc (arest achoice)
            (letcar deals
                (zip-deals tail (list (append (first head) arest)
                                      (append (second head) achoice)))))))

(test (zip-deals '(((1 2 3) (3 4 5))
                   ((6 7 8) (9 10 11))))
     '((6 7 8 1 2 3) (9 10 11 3 4 5))
     equal)

(define-condition unknown-kind (error)
    ((kind :initarg :=
           :reader kind)))

(define-condition wrong-distribution (error)
    ((distribution :initarg :=
                   :reader distribution)))

(defun classifier-for-kind (kind)
    (cond ((eq kind 'hcp) #'card-hcp)
          ((eq kind 'distribution) #'card-suitno)
          (t (error 'unknown-kind := kind))))

(defun k-permutation (length opts)
    (apply #'* (loop for x from (+ (- length opts) 1)
                           to length
                     collect x)))

(test (k-permutation 3 2) 6 eq)
(test (k-permutation 4 2) 12 eq)

(defun add-zeros (len lst)
    (let ((to-add (- len (length lst))))
        (if (> to-add 0) (append lst 
                                 (loop for i from 1 to to-add 
                                       collect 0))
                         lst)))

(defun permut-weight (supply permut)
    (apply #'* (loop for p in permut
                     for s in (add-zeros (length permut) supply)
                     collect (k-permutation s p))))
    
(test (permut-weight '(4 4 4 4) '(1 2 0 0)) 48 eq)
(test (permut-weight '(1 2 0 0) '(1 2 0 0)) 2 eq)
(test (permut-weight '(0 0 0 0) '(0 3 0 0)) 0 eq)
(test (permut-weight '(0 1 0 0) '(0 3 0 0)) 0 eq)
(test (permut-weight '(3 3 3) '(1 1 1 1)) 0 eq)
    
(defun permut-for-kind (kind target division &key (test #'=) (total 13))
    (cond ((eq kind 'hcp) (let* ((supply (mapcar #'length division))
                                 (permuts (mapcar (lambda (perm)
                                                     (cons (- total (apply #'+ perm)) perm))
                                                  (valid-hcp-sums target :test test :supply (cdr supply)))))
                             (randcar-weights (zip-weight (curry #'permut-weight supply)
                                                          permuts))))
          ((eq kind 'distribution) (if (= (apply #'+ target) total) target
                                       (error 'wrong-distribution := target)))))

(defmacro -= (form amount)
    `(setf ,form (- ,form ,amount)))

(defun take-many (source getter amount &optional acc)
    (if (= amount 0) (list acc source)
        (let-from! (funcall getter source) (card rest)
            (take-many rest getter (- amount 1) (cons card acc)))))

(defun gen-hand (target kind &key cards (test #'=) (total 13))
    (let* ((division (cards-by (classifier-for-kind kind) 
                               (or cards (all-cards))))
           (permut (permut-for-kind kind target division :test test :total total)))
        (if permut
            (zip-deals (loop for class in division
                             for amount in permut
                             collect (deal amount class))))))

;======================================
; Monte-carlo deals & reporting stats
;======================================

(defun hist-base (vals &optional base)
    (labels ((histmod (ref val)
                (if (equal (first ref) val) (list val (+ (second ref) 1))))
             (histinit (x) `(,x 1)))
        (fold (curry #'mod-or-push #'histmod #'histinit)
              base vals)))
            
(test (hist-base '(a b a d 4 5 f a 4))
     '((a 3) (b 1) (d 1) (4 2) (5 1) (f 1))
     equal)

(test (hist-base '((a b) (a d) (a b) 4 5 f a 4))
     '(((a b) 2) ((a d) 1) (4 2) (5 1) (f 1) (a 1))
     equal)

(test (hist-base '((a b) (a d) (a b) 4 5 f a 4) '((a 5) (4 8)))
     '((a 6) (4 10) ((a b) 2) ((a d) 1) (5 1) (f 1))
     equal)

(defun hist-breakdown (histogram)
    (labels ((rebase (newbase basefreq vals)
                (cond ((and newbase vals)
                            (mapcar (lambda (x) (list (append (ensure-list newbase) 
                                                              (ensure-list (first x))) 
                                                      (second x)))
                                    vals))
                      (newbase `((,(car* newbase) ,basefreq)))
                      (t vals)))
             (brkmod (a b)
                (if a
                    (let-from* a (aval afreq achildren)
                        (let-from* b (bval bfreq)
                            (let-from! (getfork (ensure-list aval) (ensure-list bval)) 
                                       (common arest brest)
                                (cond ((not common) nil)
                                      ((not (or arest brest))
                                           (list aval (+ afreq bfreq)))
                                      ((and (= (length arest) 0) (not achildren))
                                         (list (car* common) (+ afreq bfreq)
                                               `((nil ,afreq) (,(car* brest) ,bfreq))))
                                      (t (list (car* common) (+ afreq bfreq)
                                            (mod-or-push #'brkmod #'id 
                                                         (rebase arest afreq achildren)
                                                         (list (car* brest) bfreq))))))))))
              
              (self (hist &optional base)
                (fold (curry #'mod-or-push #'brkmod #'id) base hist)))

        (self histogram)))
             
(test (hist-breakdown '((a 6) (4 10) ((c d) 3) ((f 4) 3) ((a b) 2) ((a d) 1) (5 1) (f 1) ((c d) 3)))
     '((a 9 ((nil 6) (b 2) (d 1)))
       (4 10) 
       ((c d) 6)
       (f 4 ((4 3) (nil 1))) 
       (5 1))
     equal)

(test (hist-breakdown '(((4 5 15 'foo) 2) ((5 4 15 'bar) 4) ((4 7 15 'bar) 9) ((4 5 7 'baz) 1)))
     '((4 12 ((5 3 (((15 'FOO) 2) 
                    ((7 'BAZ) 1)))
              ((7 15 'BAR) 9)))
       ((5 4 15 'BAR) 4))
     equal)

(defun hist-percent (histogram)
    (let ((total (fold #'+ 0 (mapcar #'second histogram))))
        (sort (mapcar (lambda (entry) 
                        (let-from* entry (val freq children)
                            (let ((head (list val (/ freq total))))
                                (if children (append head (hist-percent children))
                                             head))))
                     histogram)
              (lambda (a b) (> (second a) (second b))) )))
            

(test (hist-percent '((4 12 ((5 3 (((15 'FOO) 2) 
                                   ((7 'BAZ) 1)))
                             ((7 15 'BAR) 9)))
                      ((5 4 15 'BAR) 4)))
      '((4 3/4 ((7 15 'BAR) 3/4)
               (5 1/4 ((15 'FOO) 2/3) 
                      ((7 'BAZ) 1/3)))
        ((5 4 15 'BAR) 1/4))
      equal)

(defun histogram (data)
    (hist-percent (hist-breakdown (hist-base data))))

(test (histogram '(a b 12 15 (a b) (c b) c c a 15 (a b c)))
    '((A 4/11 (NIL 1/2) 
              (B 1/2 (NIL 1/2) (C 1/2)))
      (C 3/11 (NIL 2/3) 
              (B 1/3))
      (15 2/11)
      (B 1/11)
      (12 1/11))
     equal)

(defun repstr (n str)
    (apply (curry #'concatenate 'string)
           (loop for i from 1 to n
                 collect str)))

(defun print-hist (hist &optional (indent 0))
    (loop for entry in hist
          do (let-from* entry (val freq)
                (format t "~A~A: ~A~%" (repstr indent "    ")
                                      val (if (< freq 1/1000) (/ 1 (floor (/ (denominator freq)
                                                                            (numerator freq))))
                                              (format nil "~,2f%" (* freq 100))))
                (if (nthcdr 2 entry) (print-hist (nthcdr 2 entry) (+ indent 1))))))

(defun suit-distribution (hand)
    (sort (mapcar #'length hand) #'<))

(defun fit-n-distrib (trump)
    (list (lambda (hand)
             (length (nth trump hand)))
          (lambda (hand)
             (sort (mapcar #'length (second (take trump hand))) #'<))))

(defun suit-hcp (hand)
    (apply #'+ (mapcar (lambda (lst) (apply #'+ (mapcar #'rank-hcp lst))) hand)))

(defun suit-strength (hand)
    (strength (suit-hcp hand)))

(defun f-cons (&rest functions)
    (lambda (&rest input)
        (mapcar (lambda (f) (apply f input))
                functions)))

(defun mc-case (measures &key (volume 2500) first-hand deal)
    (histogram
        (apply #'append
            (loop for i from 0 to volume
                  collect (mapcar (apply #'f-cons measures)
                                  (mapcar #'hand
                                          (cond (first-hand (let-from! (eval first-hand) (rest)
                                                                (deal-all 13 rest)))
                                                (deal (eval deal))
                                                (t (deal-all 13 (all-cards))))))))))

(mc-case '(suit-strength suit-distribution) :first-hand '(gen-hand 18 'hcp))

;(let ((f (gen-hand 14 'hcp)))
;   (mapcar #'hand (cons (second f) (deal-all 13 (first f)))))

(mc-case (fit-n-distrib 0) :first-hand '(gen-hand '(5 3 3 2) 'distribution))

(defun gen-suit (cards suit &key (hcp-test #'=) hcp len)
    (if hcp (gen-hand hcp 'hcp :total len :test hcp-test 
                               :cards (remove-if (lambda (c) (not (eq suit (car c)))) cards))
            (deal len (remove-if (lambda (c) (not (eq suit (car c)))) cards)))) 

(defun gen-partner-deal (base hcp &rest rules)
    (let ((starter (second (remove-cards (unhand base) (all-cards)))))
         (let ((from-rules (zip-deals (loop for rule in rules
                                            collect (apply #'gen-suit (cons starter rule))))))
              (let ((beyond-rules (gen-hand (- hcp (apply #'+ (mapcar #'card-hcp (second from-rules))))
                                            'hcp 
                                            :total (- 13 (length (second from-rules)))
                                            :cards (remove-if (lambda (c) (find (first c) 
                                                                          (mapcar #'first rules)))
                                                              starter))))
                   (let ((partner (zip-deals (list beyond-rules from-rules))))
                       (append (list (unhand base)(second partner))
                               (deal-all 13 (first partner))))))))

(defun gen-partner-deal* (base hcp &rest rules)
    (let ((starter (second (remove-cards (unhand base) (all-cards)))))
         (let* ((from-rules (zip-deals (loop for rule in rules
                                            collect (apply #'gen-suit (cons starter rule)))))
                (beyond-rules (gen-hand (- hcp (apply #'+ (mapcar #'card-hcp (second from-rules))))
                                        'hcp 
                                        :total (- 13 (length (second from-rules)))
                                        :cards (append (remove-if (lambda (c) 
                                                                    (find (first c) (mapcar #'first rules)))
                                                                  starter)
                                                       (car from-rules)))))
                   (let ((partner (list (car beyond-rules)
                                        (append (second beyond-rules)
                                                (second from-rules)))))
                       (append (list (unhand base)(second partner))
                               (deal-all 13 (first partner)))))))

;; Test  deal
;; Declarer: ♣ QJ7 ♦ QJ ♥ K10642 ♠ QJ6
;; Dummy: ♣ 9832 ♦ A75 ♥ A975 ♠ A10
;; Left: ♣ AK54 ♦ K643 ♥ J8 ♠ 532
;; Right: ♣ 106 ♦ 10982 ♥ Q3 ♠ K9874

;; I need a simplified approach for start. Let's identify immediate loosers:
;; ♣AKx -> ruff. 

(defun non-nil (x)
    (if (not x) nil T))

(defun play-low (suit hand)
    (let ((played (car (nth (suitno suit) hand))))
        (if played (list played (loop for s in '(c d h s)
                                      for cards in hand
                                       collect (if (eq suit s) (cdr cards)
                                                   cards))))))

(defun play-high (suit hand)
    (let ((played (car (nth (suitno suit) hand))))
        (if played (list played (loop for s in '(c d h s)
                                      for cards in hand
                                      collect (if (eq suit s) (subseq cards 0 (- (length cards) 1))
                                                  cards))))))

(test (play-low 'S '((1 2 10) (4 5 6) NIL (0 2 11 12)))
      '(0 ((1 2 10) (4 5 6) NIL (2 11 12)))
      equal)

(defun take-higher (cards high &key use-highest)
    (let ((higher (if use-highest (let ((highest (fold (lambda (acc val) 
                                                           (if (> (second val) (second acc)) 
                                                               val acc))
                                                       '(0 0)
                                                       (zip-id cards))))
                                       (if (> (second highest) high) (first highest)))
                                  (position-if (curry #'< high) cards))))
        (if higher (take higher cards))))

(defun normsuit (suit)
    (if (symbolp suit) (position suit '(c d h s))
                       suit))

(defun suitsym (suit)
    (and suit (nth suit '(c d h s))))

(defclass trick ()
    ((suit :reader suit)
     (trump :reader trump)
     (high :reader high)
     (winner :reader winner)
     (cards :reader cards)
     (remaining :reader remaining)))

(defmethod initialize-instance ((self trick) &key suit trump high winner)
    (setf (slot-value self 'high) (if (listp high) (or high (list (normsuit suit) -1))
                                                   (list (normsuit suit) high)))
    (setf (slot-value self 'suit) (normsuit suit))
    (setf (slot-value self 'trump) (normsuit trump))
    (setf (slot-value self 'winner) (or winner -1))
    (setf (slot-value self 'cards) nil)
    (setf (slot-value self 'remaining) nil))

(defmethod print-object ((self trick) out)
    (with-slots (high suit trump winner remaining) self
        (format out "TRICK<~A/~A @ ~A, w:~A r:~A>" high (suitsym suit) (suitsym trump) winner remaining)))

(defmethod card-higher ((trick trick) a b)
    (with-slots (trump suit) trick
        (let-from* a (asuit arank)
            (let-from* b (bsuit brank)
                (let ((atrump (eq asuit trump))
                      (as (eq asuit suit))
                      (btrump (eq bsuit trump))
                      (bs (eq bsuit suit)))
                    (cond ((and atrump (not btrump)) T)
                          ((and btrump (not atrump)) NIL)
                          ((and as (not bs)) T)
                          ((and bs (not as)) NIL)
                          ((not (or as atrump)) NIL)
                          ((not (or bs btrump)) T)
                          (T (> arank brank))))))))

(test (card-higher (make-instance 'trick :suit 'D :trump 'H)
                   '(0 9) '(3 5))
      NIL eq)

(test (card-higher (make-instance 'trick :suit 'D :trump 'H)
                   '(1 9) '(3 5))
      T eq)

(test (card-higher (make-instance 'trick :suit 'D :trump 'H)
                   '(1 9) '(2 5))
      NIL eq)

(test (card-higher (make-instance 'trick :suit 'D :trump 'H)
                   '(2 9) '(2 5))
      T eq)

(defmethod play ((self trick) card rest)
    (with-slots (high winner remaining cards) self
        (if (and card (card-higher self card high))
            (progn (setf high card)
                   (setf winner (length (slot-value self 'remaining)))))
        (if remaining
            (setf (cdr (last remaining)) (list rest))
            (setf remaining (list rest)))
        (if cards
            (setf (cdr (last cards)) (list card))
            (setf cards (list card)))
        self))

(defmethod winnerp? ((self trick))
    (with-slots (remaining winner) self
        (= (mod (length remaining) 2)
           (mod winner 2))))

(defmethod roll-trick ((self trick) n)
    (with-slots (winner cards remaining) self
        (setf winner (mod (+ winner n) 4))
        (setf cards (roll n cards))
        (setf remaining (roll n remaining)))
    self)

(defmethod as-list ((self trick))
    (cons (car (last (cards self))) (remaining self)))

(defmethod score ((self trick))
    (if (= (mod (winner self) 2) 0) 1 -1))

(defmethod as-summary ((self trick))
    (cons (score self) (remaining self)))

(defmacro make-trick (suit trump &body actions)
    `(let ((trick (make-instance 'trick :suit ,suit :trump ,trump)))
        ,@actions
        trick))

(defclass hand ()
    ((suits :reader suits)))

(defmethod initialize-instance ((self hand) &key =)
    (setf (slot-value self 'suits) (mapcar (lambda (x) (sort x #'<))
                                           =)))

(defmethod print-object ((self hand) out)
    (format out "HAND<~A>" (suits self)))

(defmethod play ((self hand) suit (chooser function))
    (with-slots (suits) self 
        (let ((played (nth suit suits)))
            (if played (let ((ans (funcall chooser played)))
                               (if ans(list (list suit (car ans))
                                            (append (subseq suits 0 suit)
                                                    (cons (second ans)
                                                          (subseq suits (+ suit 1)))))))))))

(defmethod weak-suit ((self hand) &key trump)
    (first (fold (lambda (acc val)
                    (let-from* val (suit rank)
                        (if (and (not (eq trump suit))
                                 (< rank (second acc))) val acc)))
                 '(0 13)
                 (zip-id (mapcar (curry #'apply #'min?) (suits self))))))

(defun lowest-card (cards)
    (list (car cards) (cdr cards)))

(defun beat-or-low (trick hand &key use-highest)
    (with-slots (suit high trump) trick
      (let-from* high (hsuit hrank)
        (let-from! (cond ((and hsuit (eq hsuit trump) (not (eq suit trump)))
                            (or (play hand suit #'lowest-card)
                                (play hand trump (lambda (cards)
                                                    (let ((ans (position-if (curry #'< hrank) cards)))
                                                       (if ans (take ans cards)))))
                                (play hand (weak-suit hand :trump trump) #'lowest-card)
                                (if trump (play hand trump #'lowest-card))))

                         ((and hsuit (eq hsuit suit))
                            (or (play hand suit (lambda (cards)
                                                    (or (take-higher cards hrank
                                                                     :use-highest use-highest)
                                                        (lowest-card cards))))
                                (and trump (play hand trump #'lowest-card))
                                (play hand (weak-suit hand :trump trump) #'lowest-card)
                                (if trump (play hand trump #'lowest-card))))

                         (t (or (play hand suit (lambda (cards)
                                                    (or (take-higher cards -1
                                                                     :use-highest use-highest)
                                                        (lowest-card cards))))
                                (and trump (play hand trump #'lowest-card))
                                (play hand (weak-suit hand :trump trump) #'lowest-card)
                                (if trump (play hand trump #'lowest-card)))))
                   (high remaining)
              (if remaining (play trick high remaining))))))

(test (as-list (beat-or-low (make-instance 'trick :suit 'S :high 4)
                            (make-instance 'hand := '((11 12) () (5 6) (0 5 12)))))
      '((3 5) ((11 12) () (5 6) (0 12)))
      equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'S)
                            (make-instance 'hand := '((11 12) () (5 6) (0 5 12)))))
      '((3 0) ((11 12) () (5 6) (5 12)))
      equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'H :high 10) 
                            (make-instance 'hand := '((11 12) () (5 6) (0 5 12)))))
      '((2 5) ((11 12) () (6) (0 5 12)))
      equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'H :high 10) 
                            (make-instance 'hand := '((11 12) () () (5 12)))))
      '((3 5) ((11 12) () () (12)))
      equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'H :high 10) 
                            (make-instance 'hand := '((11 12) () () (12)))))
      '((0 11) ((12) () () (12)))
      equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'S :high 4) 
                            (make-instance 'hand := '((11 12) () (5 6) (0 3 12)))))
      '((3 12) ((11 12) () (5 6) (0 3)))
      equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'S :high 4) 
                            (make-instance 'hand := '((11 12) () (5 6) (0 5 12))) :use-highest t))
      '((3 12) ((11 12) () (5 6) (0 5)))
      equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'D :high 7) 
                            (make-instance 'hand := '((5 9) NIL (0 2 4 8 11) (4 9 10))) :USE-HIGHEST NIL))
     '((2 0) ((5 9) NIL (2 4 8 11) (4 9 10)))
     equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'D :high 7 :trump 'C) 
                            (make-instance 'hand := '((5 9) NIL (0 2 4 8 11) (4 9 10))) :USE-HIGHEST NIL))
     '((0 5) ((9) NIL (0 2 4 8 11) (4 9 10)))
     equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'D :high '(0 7) :trump 'C) 
                            (make-instance 'hand := '((5 9) NIL (0 2 4 8 11) (4 9 10))) :USE-HIGHEST NIL))
     '((0 9) ((5) NIL (0 2 4 8 11) (4 9 10)))
     equal)

(test (as-list (beat-or-low (make-instance 'trick :suit 'D :high '(0 11) :trump 'C) 
                            (make-instance 'hand := '((5 9) NIL (0 2 4 8 11) (4 9 10))) :USE-HIGHEST NIL))
     '((2 0) ((5 9) NIL (2 4 8 11) (4 9 10)))
     equal)

(defun lastcar (x)
    (car (last x)))

(defun find-suit (test leader left partner right)
    (find-if #'non-nil (loop for suit in '(c d h s)
                             for op1 in left
                             for op2 in right
                             for l in leader
                             for p in partner
                             collect (and (apply test (list suit l op1 p op2))
                                          suit))))

(defun choose-void (trump leader left partner right)
    (let* ((void (find-suit (lambda (suit l op1 p op2)
                               (and l op1 op2 
                                        (not (let ((lc (lastcar l))
                                                   (c1 (lastcar op1))
                                                   (c2 (lastcar op2)))
                                                (and (> lc c1)
                                                     (> lc c2))))
                                        (nth (suitno trump) partner)
                                        (not p) 
                                        (not (eq suit trump))))
                            leader left partner right)))
        (if void (list (second (play-low void leader))
                       (second (play-low void left))
                       (second (play-low trump partner))
                       (second (play-low void right))))))
         
(test (choose-void 'S '((6 7) (1 2 4 11) (6 9) (0 1 3))
                      '((11 12) (3 5 12) (3 5 7 12) (8 12))
                      '(() (0 6 7 8) (1 10) (2 5 6 7 11))
                      '((10) (9 10) (0 2 4 8 11) (4 9 10)))
      '(((7) (1 2 4 11) (6 9) (0 1 3))
        ((12) (3 5 12) (3 5 7 12) (8 12))
        (() (0 6 7 8) (1 10) (5 6 7 11))
        (() (9 10) (0 2 4 8 11) (4 9 10)))
        equal)
         
(test (choose-void 'H '((6 7) (3 5 12) (3 5 7 12) (8 12))
                      '((2 3) (1 2 4 11) (6 9) (0 1 3))
                      '(NIL (0 6 7 8) (1 10) (2 5 6 7 11))
                      '((10) (9 10) (0 2 4 8 11) (4 9 10)))
      '(((7) (3 5 12) (3 5 7 12) (8 12))
        ((3) (1 2 4 11) (6 9) (0 1 3))
        (NIL (0 6 7 8) (10) (2 5 6 7 11))
        (NIL (9 10) (0 2 4 8 11) (4 9 10)))
      equal)

; Don't choose void if you can take otherwise in that suit
(test (choose-void 'H '((7 12) (1 2 4 11) (6 9) (0 1 3))
                      '((6 11) (3 5 12) (3 5 7 12) (8 12))
                      '(() (0 6 7 8) (1 10) (2 5 6 7 11))
                      '((10) (9 10) (0 2 4 8 11) (4 9 10)))
        '()
        equal)

(defun finesse-if-possible (trick h1 h2)
    (with-slots (high suit trump) trick
        (let-from! (and (not (winnerp? trick)) high) (hsuit hrank)
            (let-from! (cond ((and hsuit (eq hsuit trump) (not (eq trump suit)))
                                (or (play h1 suit #'lowest-card)
                                    (if (and (nth suit (suits h2)) (not (eq suit trump)))
                                        (play h1 trump (curry #'take-match (curry #'< hrank)))
                                        (let ((h2-max (or (apply #'max (nth trump (suits h2))) -1)))
                                            (play h1 trump (curry #'take-match (andf (curry #'< hrank)
                                                                                     (curry #'< h2-max))))))
                                    (play h1 (weak-suit h1) #'lowest-card)))
                             ((and hsuit (eq hsuit suit))
                                (or (if (nth suit (suits h2))
                                        (let ((h2-max (apply #'max (nth suit (suits h2)))))
                                           (or (play h1 suit (curry #'take-match (andf (curry #'< h2-max)
                                                                                       (curry #'< hrank))))
                                               (play h1 suit (curry #'take-match (curry #'< hrank)))))
                                        (play h1 suit (curry #'take-match (curry #'< hrank))))
                                    (play h1 suit #'lowest-card)

                                    (if (and (not (nth suit (suits h2)))
                                             (nth trump (suits h2)))
                                        (let ((h2-max (apply #'max (nth trump (suits h2)))))
                                            (play h1 trump (curry #'take-match (curry #'< h2-max))))
                                        (if (not (winnerp? trick))
                                            (play h1 trump #'lowest-card)))

                                    (play h1 (weak-suit h1) #'lowest-card)))
                             (t (or (if (nth suit (suits h2))
                                        (let ((h2-max (apply #'max (nth suit (suits h2)))))
                                           (play h1 suit (curry #'take-match (curry #'< h2-max)))))
                                    (play h1 suit #'lowest-card)

                                    (if (and (not (nth suit (suits h2)))
                                             (nth trump (suits h2)))
                                        (let ((h2-max (apply #'max (nth trump (suits h2)))))
                                            (play h1 trump (curry #'take-match (curry #'< h2-max))))
                                        (if (not (winnerp? trick))
                                            (play h1 trump #'lowest-card)))

                                    (play h1 (weak-suit h1) #'lowest-card))))
                   (high remaining)
                (if remaining (play trick high remaining))))))

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'H)
                                    (make-instance 'hand := '((4 6) (1) (0 1 2 9 10 12) (1 2 4 7)))
                                    (make-instance 'hand := '((2 3 5 7 8 12) (0 3 6 11) (3 7) (0)))))
      '((2 9) ((4 6) (1) (0 1 2 10 12) (1 2 4 7)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'H :high 8)
                                    (make-instance 'hand := '((4 6) (1) (0 1 2 9 12) (1 2 4 7)))
                                    (make-instance 'hand := '((2 3 5 7 8 12) (0 3 6 11) (7) (0)))))
      '((2 9) ((4 6) (1) (0 1 2 12) (1 2 4 7)))
      equal)
      
(test (as-list (finesse-if-possible (make-instance 'trick :suit 'H :high 8 :winner 0)
                                    (make-instance 'hand := '((4 6) (1) (0 1 2 9 12) (1 2 4 7)))
                                    (make-instance 'hand := '((2 3 5 7 8 12) (0 3 6 11) (7) (0)))))
      '((2 9) ((4 6) (1) (0 1 2 12) (1 2 4 7)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'D :high 6)
                                    (make-instance 'hand := '((1 10) (4 5 8 10 12) (4 6) (3 6 8 12)))
                                    (make-instance 'hand := '((0 9 11) (2 7 9) (5 8 11) (5 9 10 11)))))
      '((1 10) ((1 10) (4 5 8 12) (4 6) (3 6 8 12)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'D)
                                    (make-instance 'hand := '((1 10) (4 5 8 12) (4 6) (3 6 8 12)))
                                    (make-instance 'hand := '((0 9 11) (7 9) (5 8 11) (5 9 10 11)))))
      '((1 12) ((1 10) (4 5 8) (4 6) (3 6 8 12)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'D :trump 'C :high 6)
                                    (make-instance 'hand := '((1 10) (4 5 8 12) (4 6) (3 6 8 12)))
                                    (make-instance 'hand := '((0 9 11) () (5 8 11) (5 9 10 11)))))
      '((1 8) ((1 10) (4 5 12) (4 6) (3 6 8 12)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'D :trump 'C :high 6)
                                    (make-instance 'hand := '((1 10) () (4 6) (3 6 8 12)))
                                    (make-instance 'hand := '((0 9 11) (11 12) (5 8 11) (5 9 10 11)))))
      '((0 1) ((10) () (4 6) (3 6 8 12)))
      equal)

; TODO: Finesse-if-possible shouldn't beat partners card.
;(test (as-list (finesse-if-possible (make-instance 'trick :suit 'D :trump 'C :high 11 :winner 0)
;                                    (make-instance 'hand := '((1 10) () (4 6) (3 6 8 12)))
;                                    (make-instance 'hand := '((0 9 11) (9 10) (5 8 11) (5 9 10 11)))))
;      '((3 3) ((1 10) () (4 6) (6 8 12)))
;      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'D)
                                    (make-instance 'hand := '((1 10) (4 5 8 11) (4 6) (3 6 8 12)))
                                    (make-instance 'hand := '((0 9 11) (7 12) (5 8 11) (5 9 10 11)))))
      '((1 4) ((1 10) (5 8 11) (4 6) (3 6 8 12)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'D :high 10)
                                    (make-instance 'hand := '((1 10) (4 5 8 11) (4 6) (3 6 8 12)))
                                    (make-instance 'hand := '((0 9 11) (7 12) (5 8 11) (5 9 10 11)))))
      '((1 11) ((1 10) (4 5 8) (4 6) (3 6 8 12)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'D :high 10)
                                    (make-instance 'hand := '((1 10) (4 5 8 11) (4 6) (3 6 8 12)))
                                    (make-instance 'hand := '((0 9 11) () (5 8 11) (5 9 10 11)))))
      '((1 11) ((1 10) (4 5 8) (4 6) (3 6 8 12)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'H :trump 'H :high 6 :winner 1)
                                    (make-instance 'hand := '((4 6) (1) (0 1 2 9 10 12) (2 4 7)))
                                    (make-instance 'hand := '((0 9 11) (2 7 9) (4 5 8) (9 10 11)))))
      '((2 9) ((4 6) (1) (0 1 2 10 12) (2 4 7)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'H :high 6 :winner 1)
                                    (make-instance 'hand := '((4 6) (1) (0 1 2 9 10 12) (2 4 7)))
                                    (make-instance 'hand := '((0 9 11) (2 7 9) (4 5 8) (9 10 11)))))
      '((2 9) ((4 6) (1) (0 1 2 10 12) (2 4 7)))
      equal)

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'h :high '(2 10) :winner 1)
                                    (make-instance 'hand := '((3 10 12) (1 8) (4 6 9 11) (2 3 10)))
                                    (make-instance 'hand := '((8 9 11) (6 7 9 10) (0) (0 5 7 9)))))
      '((2 11) ((3 10 12) (1 8) (4 6 9) (2 3 10)))
      equal)
                     
(defun choose-high-card (trump leader left partner right)
    (let* ((suit (find-suit (lambda (suit l op1 p op2)
                                (and l op1 op2 suit
                                     (let ((lc (lastcar l))
                                           (c1 (lastcar op1))
                                           (c2 (lastcar op2))
                                           (pc (lastcar p)))
                                         (or (and (> lc c1) (> lc c2))
                                             (and pc (> pc c1) (> pc c2))))))
                            leader left partner right)))
        (if suit (if (let ((lc (lastcar (nth (suitno suit) leader)))
                           (pc (lastcar (nth (suitno suit) partner))))
                         (or (not pc) (> lc pc)))
                     (list NIL
                           (second (play-high suit leader))
                           (second (play-low suit left))
                           (or (second (play-low suit partner)) ;partner may drop non-trump
                               (second (play-low (find-if #'non-nil 
                                                          (loop for suit in '(c d h s)
                                                                for suitno from 0 to 3
                                                                collect (and (not (eq suit trump))
                                                                             (nth suitno partner)
                                                                             suit)))
                                                 partner)))
                           (second (play-low suit right)))
                     (cons T (remaining (make-trick suit trump
                                            (beat-or-low trick (make-instance 'hand := leader))
                                            (beat-or-low trick (make-instance 'hand := left))
                                            (finesse-if-possible trick (make-instance 'hand := partner)
                                                                       (make-instance 'hand := right))
                                            (beat-or-low trick (make-instance 'hand := right)))))))))

(test (choose-high-card 'H '((7 12) (1 2 4 11) (6 9) (0 1 3))
                           '((6 11) (3 5 12) (3 5 7 12) (8 12))
                           '(() (0 6 7 8) (1 10) (2 5 6 7 11))
                           '((10) (9 10) (0 2 4 8 11) (4 9 10)))
        '(NIL ((7) (1 2 4 11) (6 9) (0 1 3))
              ((11) (3 5 12) (3 5 7 12) (8 12))
              (() (6 7 8) (1 10) (2 5 6 7 11))
              (() (9 10) (0 2 4 8 11) (4 9 10)))
        equal)

(test (choose-high-card 'H '((7) (0 1 2 4 11) (6 9) (0 1 3))
                           '((6 11) (3 5 12) (3 5 7 12) (8 12))
                           '((12) (6 7 8) (1 10) (2 5 6 7 11))
                           '((10) (9 10) (0 2 4 8 11) (4 9 10)))
        '(T (() (0 1 2 4 11) (6 9) (0 1 3))
            ((6) (3 5 12) (3 5 7 12) (8 12))
            (() (6 7 8) (1 10) (2 5 6 7 11))
            (() (9 10) (0 2 4 8 11) (4 9 10)))
        equal)

(defun invert-if-needed (lst)
    (letcar lst
        (if head (cons head (roll 2 tail))
            lst)))

(test (invert-if-needed '(T 1 2 3 4))
     '(T 3 4 1 2)
     equal)

(test (invert-if-needed '(nil 1 2 3 4))
     '(nil 1 2 3 4)
     equal)

(test (invert-if-needed nil)
     nil
     equal)

(defun immediate-loosers (trump declarer left dummy right &optional (taken 0) inverted)
    (let-from! (invert-if-needed (or (let ((v (and trump (choose-void trump left dummy right declarer))))
                                        (if v (cons T v)))
                                     (choose-high-card trump left dummy right declarer)))
               (new-inverted left-left left-dummy left-right left-declarer)
        (if (not left-left) taken
            (apply #'immediate-loosers (list trump left-declarer left-left
                                             left-dummy left-right 
                                             (+ taken 1)
                                             (xor inverted new-inverted))))))

(test (immediate-loosers 'H '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))
                            '((2 3 11 12) (1 2 4 11) (6 9) (0 1 3))
                            '((4 8 10) (0 6 7 8) (1 10) (2 5 6 7 11))
                            '((5 9) (9 10) (0 2 4 8 11) (4 9 10)))
      3
      equal)

(test (immediate-loosers 'C '((0 9 11) (2 7 9) (5 8 11) (5 9 10 11)) 
                            '((4 6) (1) (0 1 2 9 10 12) (1 2 4 7))
                            '((2 3 5 7 8 12) (0 3 6 11) (3 7) (0))
                            '((1 10) (4 5 8 10 12) (4 6) (3 6 8 12)))
      5 equal)

(test (immediate-loosers 'H '((4 6) (1) (0 1 2 9 10 12) (1 2 4 7))
                            '((0 9 11) (2 7 9) (5 8 11) (5 9 10 11)) 
                            '((1 10) (4 5 8 10 12) (4 6) (3 6 8 12))
                            '((2 3 5 7 8 12) (0 3 6 11) (3 7) (0)))
      2
      equal)

(defun play-round (suit trump dealer left dummy right)
    (flet ((one-side-trick (a b c d)
              (fold (lambda (acc x) (beat-or-low acc (make-instance 'hand := x)))
              (beat-or-low (make-instance 'trick :suit suit :trump trump)
                           (make-instance 'hand := a) :use-highest T)
              (list b c d)))
           (high-or-finesse (a b c d)
              (make-trick suit trump
                 (beat-or-low trick (make-instance 'hand := a))
                 (beat-or-low trick (make-instance 'hand := b))
                 (finesse-if-possible trick (make-instance 'hand := c)
                                            (make-instance 'hand := d))
                 (beat-or-low trick (make-instance 'hand := d)))))
        (let ((dummy-max (apply #'max? (nth (suitno suit) dummy)))
              (dealer-max (apply #'max? (nth (suitno suit) dealer))))
            (cond ((not (or dummy-max dealer-max)) nil)
                  ((not dummy-max) (one-side-trick dealer left dummy right))
                  ((not dealer-max) (roll-trick (one-side-trick dummy right dealer left) 2))
                  ((> dealer-max dummy-max) (roll-trick (high-or-finesse dummy right dealer left) 2))
                  (t (high-or-finesse dealer left dummy right))))))

(test (as-summary (play-round 'D NIL '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))
                                     '((2 3 11 12) (1 2 4 11) (6 9) (0 1 3))
                                     '((4 8 10) (0 6 7 8) (1 10) (2 5 6 7 11))
                                     '((5 9) (9 10) (0 2 4 8 11) (4 9 10))))
      '(1 ((0 1 6 7) (3 5) (3 5 7 12) (8 12))
          ((2 3 11 12) (2 4 11) (6 9) (0 1 3))
          ((4 8 10) (6 7 8) (1 10) (2 5 6 7 11))
          ((5 9) (10) (0 2 4 8 11) (4 9 10)))
      equal)

(test (as-summary (play-round 'D 'S '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))
                                    '((2 3 11 12) (1 2 4 11) (6 9) (0 1 3))
                                    '((4 8 10) (0 6 7 8) (1 10) (2 5 6 7 11))
                                    '((9) () (0 2 4 8 11) (4 9 10))))
      '(-1 ((0 1 6 7) (5 12) (3 5 7 12) (8 12))
           ((2 3 11 12) (2 4 11) (6 9) (0 1 3))
           ((4 8 10) (6 7 8) (1 10) (2 5 6 7 11))
           ((9) () (0 2 4 8 11) (9 10)))
      equal)

(test (as-summary (play-round 'D NIL '((0 1 6 7) (3 5) (3 5 7 12) (8 12))
                                     '((2 3 11 12) (2 4 11) (6 9) (0 1 3))
                                     '((4 8 10) (6 7 8) (1 10) (2 5 6 7 11))
                                     '((5 9) (10) (0 2 4 8 11) (4 9 10))))
      '(-1 ((0 1 6 7) (5) (3 5 7 12) (8 12))
           ((2 3 11 12) (2 11) (6 9) (0 1 3))
           ((4 8 10) (7 8) (1 10) (2 5 6 7 11))   
           ((5 9) () (0 2 4 8 11) (4 9 10)))
      equal)

(test (as-summary (play-round 'D 'S '((0 1 6 7) () (3 5 7 12) (8 12))
                                    '((2 3 11 12) (2 4 11) (6 9) (0 1 3))
                                     '((4 8 10) (6 7 8) (1 10) (2 5 6 7 11))
                                     '((5 9) () (0 2 4 8 11) (4 9 10))))
      '(1 ((0 1 6 7) () (3 5 7 12) (12))
           ((2 3 11 12) (4 11) (6 9) (0 1 3))
           ((4 8 10) (6 7) (1 10) (2 5 6 7 11))   
           ((5 9) () (0 2 4 8 11) (9 10)))
      equal)

(test (as-summary (play-round 'D NIL '((0 1 6 7) (3 5) (3 5 7 12) (8 12))
                                     '((2 3 11 12) (11) (6 9) (0 1 3))
                                     '((4 8 10) (6 7 8) (1 10) (2 5 6 7 11))
                                     '((5 9) (10) (0 2 4 8 11) (4 9 10))))
      '(-1 ((0 1 6 7) (5) (3 5 7 12) (8 12))
           ((2 3 11 12) () (6 9) (0 1 3))
           ((4 8 10) (7 8) (1 10) (2 5 6 7 11))   
           ((5 9) () (0 2 4 8 11) (4 9 10)))
      equal)

(test (as-summary (play-round 'D NIL '((0 1 6 7) () (3 5 7 12) (8 12))
                                     '((2 3 11 12) (4) (6 9) (0 1 3))
                                     '((4 8 10) (7) (1 10) (2 5 6 7 11))
                                     '((5 9) () (0 2 4 8 11) (4 9 10))))
      '(1 ((1 6 7) () (3 5 7 12) (8 12))
          ((2 3 11 12) () (6 9) (0 1 3))
          ((4 8 10) () (1 10) (2 5 6 7 11))
          ((5 9) () (2 4 8 11) (4 9 10)))
      equal)

(test (play-round 'D NIL '((0 1 6 7) () (3 5 7 12) (8 12))
                         '((2 3 11 12) (4) (6 9) (0 1 3))
                         '((4 8 10) () (1 10) (2 5 6 7 11))
                         '((5 9) () (0 2 4 8 11) (4 9 10)))
      nil 
      equal)

(test (as-summary (PLAY-ROUND 'S NIL '((4 6) (1) (0 1 2 9 10 12) (2 4 7)) 
                                     '((2 3 5 7 8 12) (0 3 6 11) (3 7) NIL)
                                     '((1 10) (4 5 8 10 12) (4 6) (3 6 8))
                                     '((0 9 11) (2 7 9) (5 8 11) (9 10 11))))
      '(-1 ((4 6) (1) (0 1 2 9 10 12) (4 7)) 
           ((2 3 5 7 8 12) (3 6 11) (3 7) NIL)
           ((1 10) (4 5 8 10 12) (4 6) (6 8))
           ((0 9 11) (2 7 9) (5 8 11) (10 11)))
      equal)

(test (as-summary (PLAY-ROUND 'H NIL '((2 3 5 7 8 12) (0 3 6 11) (3 7) NIL)
                                     '((1 10) (4 5 8 10 12) (6 11) (3 6 8))
                                     '((4 6) (1) (0 1 2 9 10 12) (2 4 7)) 
                                     '((0 9 11) (2 7 9) (4 5 8) (9 10 11))))
      '(1 ((2 3 5 7 8 12) (0 3 6 11) (7) NIL)
          ((1 10) (4 5 8 10 12) (11) (3 6 8))
          ((4 6) (1) (0 1 2 10 12) (2 4 7)) 
          ((0 9 11) (2 7 9) (5 8) (9 10 11)))
      equal)

(test (as-summary (PLAY-ROUND 'H 'H '((2 3 5 7 8 12) (0 3 6 11) (3 7) NIL)
                                     '((1 10) (4 5 8 10 12) (6 11) (3 6 8))
                                     '((4 6) (1) (0 1 2 9 10 12) (2 4 7)) 
                                     '((0 9 11) (2 7 9) (4 5 8) (9 10 11))))
      '(1 ((2 3 5 7 8 12) (0 3 6 11) (7) NIL)
          ((1 10) (4 5 8 10 12) (11) (3 6 8))
          ((4 6) (1) (0 1 2 10 12) (2 4 7)) 
          ((0 9 11) (2 7 9) (5 8) (9 10 11)))
      equal)

(test (as-summary (PLAY-ROUND 'C 'H '(NIL NIL (2 4 9 10) NIL) 
                                    '((10) (6) NIL (2 3))
                                    '((7) NIL (3 5 7) NIL)
                                    '(NIL NIL (11) (4 7 10))))
      '(-1 (NIL NIL (4 9 10) NIL) 
           (() (6) NIL (2 3))
           (() NIL (3 5 7) NIL)
           (NIL NIL () (4 7 10)))
      equal)

(defun untrump-balance (trump declarer left dummy right &optional (winers 0) (loosers 0))
    (if (not (or (nth (suitno trump) left)
                 (nth (suitno trump) right)))
        (list loosers winers)
        (let-from! (as-summary (play-round trump trump declarer left dummy right))
                   (score ndec nleft ndummy nright)
            (untrump-balance trump ndec nleft ndummy nright (if (< score 0) winers (+ winers 1))
                                                          (if (> score 0) loosers (+ loosers 1))))))

(test (untrump-balance 'S '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))
                          '((2 3 11 12) (1 2 4 11) (6 9) (0 1 3))
                          '((4 8 10) (0 6 7 8) (1 10) (2 5 6 7 11))
                          '((5 9) (9 10) (0 2 4 8 11) (4 9 10)))
      '(0 3)
      equal)

(test (untrump-balance 'C '((0 9 11) (9) (8 11) (9 10 11)) 
                        '((6) () (0 1 2 9 10) (2 4 7))
                        '((2 3 5 7 8 12) (11) (7) ())
                        '((1 10) (5 8 10) (6) (3 6 8)))
      '(0 2)
      equal)

(test (untrump-balance 'H '((4 6) (1) (0 1 2 9 10 12) (1 2 4 7))
                        '((2 3 5 7 8 12) (0 3 6 11) (3 7) (0))
                        '((1 10) (4 5 8 10 12) (4 6) (3 6 8 12))
                        '((0 9 11) (2 7 9) (5 8 11) (5 9 10 11)))
      '(0 3)
      equal)

;; More probable EW spade contract is very sad when it comes to untrump:
;;N: ♣ KJ2 ♦ J94 ♥ K107 ♠ KQJ7
;;E: ♣ 86 ♦ 3 ♥ AQJ432 ♠ 9643
;;S: ♣ A109754 ♦ K852 ♥ 95 ♠ 2
;;W: ♣ Q3 ♦ AQ1076 ♥ 86 ♠ A1085

(test (untrump-balance 'S '((4 6) (1) (0 1 2 9 10 12) (1 2 4 7))
                        '((2 3 5 7 8 12) (0 3 6 11) (3 7) (0))
                        '((1 10) (4 5 8 10 12) (4 6) (3 6 8 12))
                        '((0 9 11) (2 7 9) (5 8 11) (5 9 10 11)))
      '(3 1)
      equal)

(defclass cache ()
    ((action :initarg :! :reader action)
     (hits :initform (make-hash-table :test #'equal))))

(defmethod run ((self cache) &rest args)
    (with-slots (action hits) self
        (let ((hit (gethash args hits)))
            (if hit hit
                (let ((ans (apply action (cons self args))))
                    (if (and (not ans) (not (equal args '(nil nil nil nil))))
                        (format t "~A => NIL~%" args))
                    (setf (gethash args hits) ans)
                    ans)))))

(defun play-deal (trump declarer left dummy right)
    (let ((cache (make-instance 'cache :! 
                    (lambda (cache dec l dum r)
                        (fold (lambda (acc suit)
                                  (let ((trick (play-round suit trump dec l dum r)))
                                     (if trick (let-from! (apply #'run (cons cache (remaining trick)))
                                                          (ns nt)
                                                  (let ((newscore (if (> (score trick) 0)
                                                                      (+ (score trick) ns)
                                                                      ns)))
                                                    (if (or (not acc) (> newscore (car acc)))
                                                      (list newscore
                                                            (cons (cons (winner trick)(cards trick) ) nt))
                                                      acc)))
                                         (list 0 nil))))
                               nil
                               '(c d h s))))))
            (run cache declarer left dummy right)))

(defun simdeal (deal)
    (print-deal deal '(n e s w))
    (let-from! (apply 'play-deal (cons 'h deal))
               (winners tricks)
        (format t "winners: ~A~%" winners)
        (loop for trick in tricks
              do (format t "~{~A ~}/~A~%" (mapcar #'cardstr (cdr trick)) (car trick)))))

(defun dump-chosen (fn test printer)
    (lambda (&rest args)
        (let ((ans (apply fn args)))
            (if (funcall test ans)
                (if printer (funcall printer args ans)
                    (format t "~A(~A) -> ~A" fn args ans)))
            ans)))

(defun shortest-side-suits (trump declarer left dummy right)
    (declare (ignore trump))
    (declare (ignore left))
    (declare (ignore dummy))
    (declare (ignore right))
    (subseq (sort (mapcar #'length declarer) #'<) 0 2))

(defun best-tricks (trump n e s w)
    (min (first (play-deal trump n e s w)) (- 13 (immediate-loosers trump n e s w))))

(defclass mc-case ()
    ((props :initarg :=)
     (dealgen :initarg :!)
     (trump :initarg :trump :initform nil)
     (data :initform nil :initarg :data)))

(defmethod print-object ((self mc-case) out)
    (with-slots (props dealgen trump) self
        (format out "MC-CASE{~A->~A @ ~A}" dealgen props trump)))

(defmethod sim ((self mc-case) runs)
    (with-slots (props dealgen trump data) self
        (loop for i from 0 to runs
              do (let ((deal (mapcar #'hand (eval dealgen))))
                    (setf data (cons (cons  deal
                                            (mapcar (lambda (prop)
                                                       (apply prop (cons trump deal)))
                                                    props))
                                     data)))))
    self)

(defmethod add-prop ((self mc-case) &rest new-props)
    (with-slots (props trump data) self
        (loop for row in data
              do (setf (cdr (last row)) (mapcar (lambda (prop)
                                                    (apply prop (cons trump (car row))))
                                                new-props)))
        (setf (cdr (last props)) props)))

(defmethod choose ((self mc-case) exp)
    (with-slots (props data) self
        (let ((pred (sublis (loop for i from 0
                                     for sym in (cons 'hand props)
                                     collect `(,sym . (nth ,i row)))
                    exp)))
            (filter (eval `(lambda (row) ,pred)) data))))

(defmethod select ((self mc-case) &key fields filter)
    (with-slots (data props) self
        (let ((source (if filter (filter filter data)
                        data)))
            (if fields (let ((col-ids (mapcar (lambda (sym)
                                                 (position sym (cons 'hand props))) 
                                              fields)))
                           (loop for row in source
                                 collect (loop for c in col-ids
                                               collect (nth c row))))
                       (mapcar #'cdr source)))))

(defmethod show-deals ((self mc-case) filter)
    (loop for deal in (choose sim filter)
      do (print-deal (car deal))))

(defmethod save ((self mc-case) filename)
    (with-open-file (out filename
                        :direction :output
                        :if-exists :supersede  
                        :if-does-not-exist :create)
        (with-slots (props dealgen trump data) self
            (let ((*print-pretty* nil))
                (prin1 (list props dealgen trump) out)
                (loop for row in data
                      do (prin1 row out))))))

(defun load-mc-case (filename &key fallback-params sim-count)
   (with-open-file (s filename :direction :input :if-does-not-exist nil)
      (if s
        (let-from! (read s nil :eof)
                   (props dealgen trump)
           (let ((data (loop for row = (read s nil :eof)
                             until (eq row :eof)
                             collect row)))
              (make-instance 'mc-case :! dealgen := props :trump trump :data data)))
        (if fallback-params
            (let ((obj (apply #'make-instance (cons 'mc-case fallback-params))))
                (if sim-count (sim obj sim-count)
                              obj))))))
    
(defun partner-clubs (trump &rest hands)
    (declare (ignore trump))
    (labels ((self (hand &optional acc)
                (letcar hand
                    (if (not head) (reverse acc)
                        (let ((high (- head 9)))
                          (if (>= high 0) (self tail (cons (nth high '(J Q K A)) acc))
                                          (self nil (cons (length hand) acc))))))))
        (self (reverse (seektree '(0 0) hands)))))

(defparameter sim (load-mc-case "sim.dat"
                                :sim-count 10
                                :fallback-params '(:= (partner-clubs untrump-balance best-tricks)
                                                   :! (reorder (gen-partner-deal
                                                                     '((9 8 3 2) (A 7 5) (A 9 7 5) (A 10))
                                                                     12 '(H :hcp 3 :hcp-test >= :len 5))
                                                                     '(1 2 0 3))
                                                   :trump h)))

;;(print-hist (histogram (mapcar #'flatten (select sim :fields '(untrump-balance) ))))
;;(print-hist (histogram (mapcar #'flatten (select sim :fields '(best-tricks) ))))

;;(print-hist (histogram (select sim :fields '(partner-clubs) )))

;;(select sim :fields '(hand))
;;(show-deals sim '(= best-tricks 12))

;;(simdeal ' (((11) (0 4 8 11) (0 1 2 8 11) (4 7 11))
;;     ((2 3 5 8 12) (2 7 10) (4 10) (1 3 5))
;;     ((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))
;;     ((4 9 10) (1 6 9) (6 9) (0 2 6 9 10))))

(defun line-hcp (trump &rest hands)
    (declare (ignore trump))
    (apply #'+ (mapcar #'rank-hcp (flatten (append (first hands) (third hands))))))

(defparameter simhcp (load-mc-case "simhcp.dat"
                                   :sim-count 10000
                                   :fallback-params '(:! (deal-all 13 (all-cards))
                                                      := (line-hcp best-tricks))))

;;(print-hist (histogram (select simhcp)))

(defun hist-plot (histogram)
    (flet ((rcode (funname histnode)
        (let ((sorthist (sort histnode (lambda (a b) (< (seektree '(0 0) a) (seektree '(0 0) b))))))
            (format T "hcp = c(~{~a~^,~}); freq = c(~{~,8f~^,~}); ~a(hcp, freq, type='b');~%"
                (mapcar (f* #'first #'first) sorthist)
                (mapcar (f* #'second #'float) sorthist)
                funname))))
      (rcode "plot" histogram)))

;;(hist-plot (histogram (select simhcp :fields '(line-hcp))))
