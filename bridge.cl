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

(defun car* (x)
    (if (listp x) (car x) x))

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

;; ===========================
;; Cards and printing them
;; ===========================

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
(defun print-deal (hands &optional (names '(n e s w)))
    (loop for name in names
          for hand in hands
          do (format t "~A: ~A~%" name 
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

(defun valid-hcp-sums (value &optional supply)
    (filter (lambda (lst) (and (= (apply #'+ (loop for x in lst
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

(test (valid-hcp-sums 13 '(4 1 0 4))
      '((1 0 0 3) (3 1 0 2))
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

(defun permut-weight (supply permut)
    (apply #'* (loop for s in supply
                     for p in permut
                     collect (k-permutation s p))))
    
(test (permut-weight '(4 4 4 4) '(1 2 0 0)) 48 eq)
(test (permut-weight '(1 2 0 0) '(1 2 0 0)) 2 eq)
(test (permut-weight '(0 0 0 0) '(0 3 0 0)) 0 eq)
(test (permut-weight '(0 1 0 0) '(0 3 0 0)) 0 eq)
    
(defun permut-for-kind (kind target division &key (max 13))
    (cond ((eq kind 'hcp) (let* ((figures (cdr division))
                                 (fig-supply (mapcar #'length figures))
                                 (fig-permuts (valid-hcp-sums target fig-supply))
                                 (fig-permut (randcar-weights 
                                                (zip-weight (curry #'permut-weight fig-supply)
                                                            fig-permuts))))
                              (cons (- max (apply #'+ fig-permut)) fig-permut)))
          ((eq kind 'distribution) (if (= (apply #'+ target) max) target
                                       (error 'wrong-distribution := target)))))

(defun card-weight (hcp-dist suit-dist card)
    (let ((hcpa (or (nth (card-hcp card) hcp-dist) 1))
          (suita (or (nth (card-suitno card) suit-dist) 1)))
        (* (if (> hcpa 0) (/ 1 hcpa) 0)
           (if (> suita 0) (/ 1 suita) 0))))

(defmacro -= (form amount)
    `(setf ,form (- ,form ,amount)))

(defun take-many (source getter amount &optional acc)
    (if (= amount 0) (list acc source)
        (let-from! (funcall getter source) (card rest)
            (take-many rest getter (- amount 1) (cons card acc)))))

(defun gen-hand* (hcp distrib &optional cards)
    (if (not cards) (setf cards (all-cards)))
    (let* ((division (cards-by #'card-hcp cards))
           (hcp-target (if hcp (permut-for-kind 'hcp hcp division)))
           (highcards (apply #'append (cdr division))))
        (flet ((draw (cardset amount)
            (take-many cardset
                       (lambda (lst) 
                         (let ((idx (wrand (mapcar (curry #'card-weight hcp-target distrib)
                                                   lst))))
                           (let-from! (take idx lst) (card rest)
                               (-= (nth (card-hcp card) hcp-target) 1)
                               (-= (nth (card-suitno card) distrib) 1)
                               (list card rest))))
                       amount)))
            (zip-deals (list (draw highcards (apply #'+ (cdr hcp-target)))
                             (draw (car division) (car hcp-target)))))))
                         
(hand (car (gen-hand* 15 '(4 3 3 3))))

(defun gen-hand (target kind &key cards (max 13))
    (let* ((division (cards-by (classifier-for-kind kind) 
                               (or cards (all-cards))))
           (permut (permut-for-kind kind target division :max max)))
        (if permut
            (zip-deals (loop for class in division
                             for amount in permut
                             collect (deal amount class))))))

(let-from! (gen-hand 11 'hcp) (rest n)
    (print-deal (mapcar #'hand (deal-all 13 rest (list n)))))

(let-from! (gen-hand '(5 2 2 4) 'distribution) (rest n)
    (print-deal (mapcar #'hand (deal-all 13 rest (list n)))))

;======================================
; Monte-carlo deals & reporting stats
;======================================

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
    (merge-by #'histmerge
        (histogram
            (apply #'append
                (loop for i from 0 to volume
                      collect (mapcar (apply #'f-cons measures)
                                      (mapcar #'hand
                                              (cond (first-hand (let-from! (eval first-hand) (rest)
                                                                    (deal-all 13 rest)))
                                                    (deal (eval deal))
                                                    (t (deal-all 13 (all-cards)))))))))))

(mc-case '(suit-strength suit-distribution) :first-hand '(gen-hand 18 'hcp))

(mc-case (fit-n-distrib 0) :first-hand '(gen-hand '(5 3 3 2) 'distribution))

(defun gen-suit (cards suit &key hcp len)
    (if hcp (gen-hand hcp 'hcp :max len :cards (remove-if (lambda (c) (not (eq suit (car c)))) cards))
            (deal len (remove-if (lambda (c) (not (eq suit (car c)))) cards)))) 
    
(defun gen-partner-deal (base hcp &rest rules)
    (let ((starter (second (remove-cards (unhand base) (all-cards)))))
         (let ((from-rules (zip-deals (loop for rule in rules
                                            collect (apply #'gen-suit (cons starter rule))))))
              (let ((beyond-rules (gen-hand hcp 'hcp 
                                            :max (- 13 (length (second from-rules)))
                                            :cards (remove-if (lambda (c) (find (first c) 
                                                                          (mapcar #'first rules)))
                                                              starter))))
                   (let ((partner (zip-deals (list beyond-rules from-rules))))
                       (append (list (unhand base)(second partner))
                               (deal-all 13 (first partner))))))))

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
        (if played (loop for s in '(c d h s)
                         for cards in hand
                         collect (if (eq suit s) (cdr cards)
                                                 cards)))))

(defun play-high (suit hand)
    (let ((played (car (nth (suitno suit) hand))))
        (if played (loop for s in '(c d h s)
                         for cards in hand
                         collect (if (eq suit s) (subseq cards 0 (- (length cards) 1))
                                                 cards)))))

(test (play-low 'S '((1 2 10) (4 5 6) NIL (0 2 11 12)))
      '((1 2 10) (4 5 6) NIL (2 11 12))
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
        (if void (list (play-low void leader)
                       (play-low void left)
                       (play-low trump partner)
                       (play-low void right)))))
         
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

(defun choose-high-card (trump leader left partner right)
    (let* ((high (find-suit (lambda (suit l op1 p op2)
                                (and l op1 op2 suit
                                     (let ((lc (lastcar l))
                                           (c1 (lastcar op1))
                                           (c2 (lastcar op2))
                                           (pc (lastcar p)))
                                         (or (and (> lc c1) (> lc c2))
                                             (and pc (> pc c1) (> pc c2))))))
                            leader left partner right)))
        (if high (if (let ((lc (lastcar (nth (suitno high) leader)))
                           (pc (lastcar (nth (suitno high) partner))))
                         (or (not pc) (> lc pc)))
                     (list (play-high high leader)
                           (play-low high left)
                           (or (play-low high partner) ;partner may drop non-trump
                               (play-low (find-if #'non-nil (loop for suit in '(c d h s)
                                                                  for suitno from 0 to 3
                                                                  collect (and (not (eq suit trump))
                                                                               (nth suitno partner)
                                                                               suit)))
                                         partner))
                           (play-low high right))
                     (list (play-low high leader)
                           (play-low high left)
                           (play-high high partner)
                           (play-low high right))))))

(test (choose-high-card 'H '((7 12) (1 2 4 11) (6 9) (0 1 3))
                           '((6 11) (3 5 12) (3 5 7 12) (8 12))
                           '(() (0 6 7 8) (1 10) (2 5 6 7 11))
                           '((10) (9 10) (0 2 4 8 11) (4 9 10)))
        '(((7) (1 2 4 11) (6 9) (0 1 3))
          ((11) (3 5 12) (3 5 7 12) (8 12))
          (() (6 7 8) (1 10) (2 5 6 7 11))
          (() (9 10) (0 2 4 8 11) (4 9 10)))
        equal)

(defun immediate-loosers (trump declarer left dummy right &optional (taken 0))
    (let-from! (or (choose-void trump left dummy right declarer)
                   (choose-high-card trump left dummy right declarer))
               (left-left left-dummy left-right left-declarer)
        (if (not left-left) (list taken declarer left dummy right)
            (apply #'immediate-loosers (list trump left-declarer left-left
                                             left-dummy left-right (+ taken 1))))))

(test (immediate-loosers 'H '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))
                            '((2 3 11 12) (1 2 4 11) (6 9) (0 1 3))
                            '((4 8 10) (0 6 7 8) (1 10) (2 5 6 7 11))
                            '((5 9) (9 10) (0 2 4 8 11) (4 9 10)))
      '(3 ((7) (3 5 12) (3 5 7 12) (8 12))
          ((3) (1 2 4 11) (6 9) (0 1 3))
          (() (0 6 7 8) (1 10) (2 5 6 7 11))
          (() (9 10) (2 4 8 11) (4 9 10)))
      equal)

(let-from! (mapcar #'hand (gen-partner-deal '((9 8 3 2) (A 7 5) (A 9 7 5) (10 A))
                             9 '(H :hcp 3 :len 5)))
        (me partner left right)
    (print-deal (list me left partner right) '(declarer left partner right))
    (format t "IL: ~A~%" (car (immediate-loosers 'H partner left me right))))

(defun dump-chosen (fn test printer)
    (lambda (&rest args)
        (let ((ans (apply fn args)))
            (if (funcall test ans)
                (if printer (funcall printer args ans)
                    (format t "~A(~A) -> ~A" fn args ans)))
            ans)))

(defun deal-mc (trump hands &key (volume 25000) eval-fn)
    (histogram
        (loop for i from 0 to volume
              collect (car (apply eval-fn (cons trump (mapcar #'hand (eval hands))))))))

(deal-mc 'h '(reorder (gen-partner-deal '((9 8 3 2) (A 7 5) (A 9 7 5) (10 A))
                                        9 '(H :hcp 3 :len 5))
                      '(1 2 0 3))
            :eval-fn (dump-chosen #'immediate-loosers
                                 (f* #'first (curry #'= 0))
                                 (lambda (args ans)
                                     (if (not (find 12 (car (car (cdr args)))))
                                         (print-deal (cdr args) '(p left me right))))))
