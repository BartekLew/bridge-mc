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

;; =============================
;; GENERAL UTILS
;; =============================

(defun id (x) x)

(defun car* (x)
    (if (and (listp x) (= (length x) 1)) (car x) x))

(defun car? (x)
    (if (listp x) (car x) x))

(defun list-lead (len lst)
    (if (<= (length lst) len) lst 
                              (subseq lst 0 len)))
(defun ensure-list (x)
    (if (listp x) x (list x)))

(defmacro test (form result test)
  `(let ((ans ,form))
     (if (not (,test ans ,result))
       (format t "TEST FAILED: ~A~%   != ~A~%" ans ,result))))

(defmacro mk-file-based-test (file &body body)
    `(with-open-file (out ,file :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
        ,@body))

(defmacro file-based-test (file &body body)
    `(with-open-file (in ,file :direction :input)
        (flet ((test-next (val)
                  (let ((ref (read in)))
                    (if (not (equal ref val))
                        (format t "TEST ~A failed:~%  ~A~%  not equal to:~%  ~A~%"
                                  ,file val ref)))))
           ,@body)))

(defun splitstr (sep str &optional max-len acc)
    (if (and max-len (= max-len 0))
        (reverse (cons str acc))
        (let ((pos (search sep str)))
            (if pos (splitstr sep (subseq str (+ pos (length sep)))
                                  (if max-len (- max-len 1))
                                  (cons (subseq str 0 pos) acc))
                    (reverse (cons str acc))))))
    
(test (splitstr " : " "foo : bar : baz")
      '("foo" "bar" "baz")
      equal)
    
(test (splitstr ":" "foo : bar : baz" 1)
      '("foo " " bar : baz")
      equal)

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

(defmacro lambda* (args &body body)
    `(lambda (arg)
        (let-from* arg ,args
            ,@body)))

(test (apply (lambda* (foo bar) (+ foo bar)) '((1 2 3 4)))
      3 eq)

(defmacro letcar (lst &body body)
    `(let ((head (car ,lst))
           (tail (cdr ,lst)))
        ,@body))
    
(defmacro peek (body)
    `(let ((ans ,body))
        (format T "> ~S~%  = ~S~%" ',body ans)
        ans))
        
(defmacro peek-if (test body)
    `(let ((ans ,body))
        (if ,test (format T "> ~A~%  = ~A~%" ',body ans))
        ans))
        
(defun pass-if (test value)
    (if (funcall test value) value))

(defmacro measure-time (body)
    `(let ((start-time (get-universal-time))
           (ans ,body))
         (format t "TIME(~A) = ~A~%" ',body (- (get-universal-time) start-time))
         ans))

(defvar *time-profs* (make-hash-table :test 'equal))
(defmacro profile-time (body &optional name)
    `(let* ((start-time (get-universal-time))
            (ans ,body)
            (duration (- (get-universal-time) start-time))
            (so-far (gethash ',(or name body) *time-profs*)))
        (setf (gethash ',(or name body) *time-profs*) (if so-far (+ so-far duration)
                                                       duration))
        ans))
        
(defun show-time-profs ()
    (loop for key being the hash-keys of *time-profs*
          collect (list key (gethash key *time-profs*))))

(defun reset-time-profs ()
    (setf *time-profs* (make-hash-table :test 'equal)))

(defun filter (pred lst &optional acc)
    (if (not lst) (reverse acc)
        (letcar lst
            (filter pred tail (if (funcall pred head) (cons head acc) acc)))))

(defun curry (fun &rest base-args)
    (lambda (&rest args)
        (apply fun (append base-args args))))

(defmacro curry* (fun arg-sym args-list)
    `(lambda (,@arg-sym)
        (apply ,fun (list ,@args-list))))

(defun f* (&rest funs)
    (lambda (&rest args)
        (let ((ans (if args (apply (car funs) args))))
            (loop for f in (cdr funs)
                  do (setf ans (if ans (funcall f ans))))
            ans)))

(defun f** (&rest funs)
    (lambda (&rest args)
        (let ((ans (if args (apply (car funs) args))))
            (loop for f in (cdr funs)
                  do (setf ans (funcall f ans)))
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

(defun divlist (sectests lst &key (map #'id))
    (labels ((self (remaining &optional acc)
                (if remaining (let ((secid (position-if (curry* #'funcall (tst) (tst (car remaining))) 
                                                        sectests)))
                                 (self (cdr remaining)
                                       (push-pos acc (or secid (length sectests))
                                                 (funcall map (car remaining)))))
                              acc)))
        (self lst (loop for i from 0 to (length sectests)
                        collect nil))))
                
(test (divlist (list (curry #'< 10) (curry #'> 5))
               '(5 10 15 20 11 1 0 3 17 20))
      '((20 17 11 20 15) (3 0 1) (10 5))
      equal)

(test (divlist (list (curry #'< 10) (curry #'> 5))
               '(5 10 15 20 11 1 0 3 17 20)
               :map (curry #'* 2))
      '((40 34 22 40 30) (6 0 2) (20 10))
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

(defun list-prod (&rest lists)
    (labels ((self (acc lists)
                (cond ((not lists) (mapcar #'reverse acc))
                      ((not (car lists)) nil)
                      ((not acc) (self (mapcar #'list (car lists))
                                       (cdr lists)))
                      (t (self (loop for l in acc
                                     append (loop for x in (car lists)
                                                  collect (cons x l)))
                               (cdr lists))))))
       (self nil lists)))

(test (list-prod '(1 2 3 4) '(a b c d))
      '((1 A) (1 B) (1 C) (1 D)
        (2 A) (2 B) (2 C) (2 D) 
        (3 A) (3 B) (3 C) (3 D) 
        (4 A) (4 B) (4 C) (4 D))
      equal)

(test (list-prod '(1 2 3) '(3 4) '() '(5 6))
      nil equal)

(defun list- (a b &key (test #'eq))
    (filter (f** (curry* #'position (x) (x b :test test)) #'not)
            a))

(test (list- '(foo 2 3 (3 5) (100 22)) '(foo fo (5 4) (3 5)) :test #'equal)
      '(2 3 (100 22))
      equal)

(test (list- '(1 2 3 4) '(4 3 2 1))
      nil
      equal)

(test (list- '(foo 2 3 (3 5) (100 22)) '(foo fo (5 4) (3 5)))
      '(2 3 (3 5) (100 22))
      equal)

(defun fold (fn val lst)
    (if (not lst) val
        (fold fn (apply fn (list val (car lst))) (cdr lst))))

(defun unique (lst &key (cmp #'<))
    (reverse (fold (lambda (acc val)
                (if (equal (car acc) val)
                    acc (cons val acc)))
                nil
                (sort lst cmp))))

(defun longest (&rest lists)
    (fold (lambda (longest next)
             (if (> (length next) (length longest))
                next longest))
          nil
          lists))

(defun andf (&rest funs)
    (lambda (&rest args)
        (fold (lambda (acc fn)
                 (if acc (apply fn args)))
              T
              funs)))

(defun orf (&rest funs)
    (lambda (&rest args)
        (fold (lambda (a v)
                 (or a (apply v args)))
              nil
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
    (if lst
        (let ((a (mod (- amount) (length lst))))
            (if (= a 0) lst
                (append (subseq lst a) (subseq lst 0 a))))))

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

(defmacro += (var val)
    `(setf ,var (+ ,var ,val)))

(defun mean (lst)
    (/ (apply #'+ lst) (length lst)))

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

(defun suitno (suitsym) 
    (if (symbolp suitsym)
        (position suitsym '(c d h s))
        suitsym))

; Comparison function for sorting cards
(defun suit<> (a b)
    (let ((an (suitno a))
          (bn (suitno b)))
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
                  (let ((sn (suitno s)))
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

(defun factorial (n &optional (acc 1))
    (if (<= n 1) acc (factorial (- n 1) (* acc n))))

(defun combinations (supply amount)
    (/ (apply #'* (loop for i from (+ 1 (- supply amount)) to supply collect i))
       (factorial amount)))

(test (combinations 4 2) 6 eq)
(test (combinations 9 4) 126 eq)

(defun add-zeros (len lst)
    (let ((to-add (- len (length lst))))
        (if (> to-add 0) (append lst 
                                 (loop for i from 1 to to-add 
                                       collect 0))
                         lst)))

(defun permut-weight (supply permut)
    (apply #'* (loop for p in permut
                     for s in (add-zeros (length permut) supply)
                     collect (combinations s p))))
    
(test (permut-weight '(4 4 4 4) '(1 2 0 0)) 24 eq)
(test (permut-weight '(1 2 0 0) '(1 2 0 0)) 1 eq)
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

(defun suit-breakdown (cards)
    (divlist (loop for suit in '(c d h)
                   collect (f* #'first (curry #'eq suit)))
             cards))

(defun hcp-breakdown (cards)
    (divlist (loop for hcp-val from 0 to 3
                   collect (f* #'card-hcp (curry #'eq hcp-val)))
             cards))

(defun supply (cards)
    (mapcar (f** #'hcp-breakdown (curry #'mapcar #'length))
            (suit-breakdown cards)))
                
(defun valtest-fun (def)
    (cond ((numberp def) (curry #'= def))
          ((functionp def) def)
          ((not def) #'id)
          ((not (listp def)) #'id)
          ((not (first def)) (curry #'>= (second def)))
          ((not (second def)) (curry #'<= (first def)))
          (t (andf (curry #'<= (first def))
                   (curry #'>= (second def))))))

(defun suit-permuts (supply &key hcp length)
    (let ((len-test (if length (valtest-fun length)))
          (hcp-test (if hcp (valtest-fun hcp))))
        (filter (lambda (permut)
                    (and (or (not len-test) (funcall len-test (apply #'+ permut)))
                         (or (not hcp-test) (funcall hcp-test (apply #'+ (loop for hcp from 1 to 4
                                                   for amount in (cdr permut)
                                                   collect (* hcp amount)))))))
                (apply #'list-prod (mapcar (lambda (x) (loop for i from 0 to x
                                                         collect i))
                                           supply)))))

(defun phcp (permut)
    (apply #'+ (loop for hcp from 1 to 4
                     for rank in (cdr permut)
                     collect (* hcp rank))))

(defun repeat (count element)
    (loop for i from 1 to count
          collect element))

(defun even-sublists (size lst &optional acc)
    (cond ((not lst) acc)
          ((<= (length lst) size) (append acc (list lst)))
          (t (even-sublists size (nthcdr size lst)
                                 (append acc (list (subseq lst 0 size)))))))

(test (even-sublists 3 '(1 2 3 4 5 6 7 8 9 10))
      '((1 2 3) (4 5 6) (7 8 9) (10))
      equal)

(defun permut-hcp (permut)
    (apply #'+ (loop for i from 0
                     for x in permut
                     collect (* x (+ (mod i 4) 1)))))

(defun hc-permuts (supply &key hcp suitdef)
    (let* ((hc-supply (apply #'append (mapcar #'cdr supply)))
           (one-pos (mapcar #'first (filter (f* #'second (curry #'eq 1))
                                            (zip-id hc-supply))))
           (hcp (if hcp (valtest-fun hcp)))
           (suithcp (mapcar (lambda (sd)
                               (let ((range (third sd)))
                                     (cond ((not range) nil)
                                            ((listp range) (lambda (suit)
                                                             (let ((hcp (permut-hcp suit)))
                                                                (and (>= hcp (or (first range) 0))
                                                                     (<= hcp (or (second range) 10))))))
                                            (t (lambda (suit) (= (permut-hcp suit) range))))))
                            suitdef))
           (todo-stack (if one-pos `((nil ,one-pos))))
               (ans nil))
        (loop while todo-stack
              do (destructuring-bind (acc remaining) (pop todo-stack)
                    (let* ((rem-pos (- (car remaining) (length acc)))
                           (acc* (mapcar (curry #'append acc (repeat rem-pos 0))
                                         (list '(0) '(1)))))

                      (if (cdr remaining)
                          (setf todo-stack (append todo-stack (mapcar (curry* #'list (x)
                                                                              (x (cdr remaining)))
                                                                      acc*)))
                          (setf ans (append ans (mapcar (lambda (x) 
                                                          (even-sublists 4
                                                             (append x (repeat (- (length hc-supply)
                                                                                  (length (car acc*)) ) 0))))
                                                        (if hcp (filter (f* #'permut-hcp hcp) acc*) 
                                                                acc*))))))))
        (if suithcp (filter (lambda (dist)
                                (not (position nil (loop for suit in dist
                                                         for test in suithcp
                                                         collect (or (not test) (funcall test suit))))))
                            ans)
                    ans)))

(test (hc-permuts '((0 0 1 0 1) (5 0 0 1 0)))
      '(((0 0 0 0) (0 0 0 0)) ((0 0 0 0) (0 0 1 0)) ((0 0 0 1) (0 0 0 0)) ((0 0 0 1) (0 0 1 0))
        ((0 1 0 0) (0 0 0 0)) ((0 1 0 0) (0 0 1 0)) ((0 1 0 1) (0 0 0 0)) ((0 1 0 1) (0 0 1 0)))  
      equal)

(test (hc-permuts '((0 0 1 0 1) (5 0 0 1 0)) :suitdef '((nil nil (0 4))))
      '(((0 0 0 0) (0 0 0 0)) ((0 0 0 0) (0 0 1 0)) ((0 0 0 1) (0 0 0 0)) ((0 0 0 1) (0 0 1 0))
        ((0 1 0 0) (0 0 0 0)) ((0 1 0 0) (0 0 1 0)))  
      equal)

(test (hc-permuts '((0 1 1 1 1) (5 0 0 1 1)) :hcp '(4 6))
      '(((0 0 0 0) (0 0 0 1)) ((0 0 0 1) (0 0 0 0)) ((0 0 1 0) (0 0 1 0))
        ((0 1 0 0) (0 0 0 1)) ((0 1 0 0) (0 0 1 0)) ((0 1 0 1) (0 0 0 0))
        ((0 1 1 0) (0 0 0 0)) ((1 0 0 0) (0 0 0 1)) ((1 0 0 0) (0 0 1 0))
        ((1 0 0 1) (0 0 0 0)) ((1 0 1 0) (0 0 0 0)) ((1 1 0 0) (0 0 1 0))
        ((1 1 1 0) (0 0 0 0)))
      equal)

(test (hc-permuts '((0 0 0 0 0) (0 0 0 0 0)))
      nil
      equal)

(test (length (hc-permuts '((5 1 0 0 1) (2 1 1 0 1) (3 0 0 1 0) (4 1 1 1 1))))
      1024 eq)

(defun possible-lc-lens (hc-permut lc-supply suit-lengths)
    (let* ((base-lens (mapcar (curry #'apply #'+) hc-permut))
           (hc-count (apply #'+ base-lens))
           (lc-ranges (loop for target in suit-lengths
                            for supply in lc-supply
                            for baselen in base-lens
                            collect (let-from! (if (listp target) target
                                                   (list target target))
                                               (min max)
                                        (filter (curry #'<= 0)
                                                (loop for i from (- (or min 0) baselen)
                                                            to   (- (or max supply) baselen)
                                                      collect i))))))
         (filter (f* (curry #'apply #'+) (curry #'eq (- 13 hc-count)))
                 (apply #'list-prod lc-ranges))))

(test (possible-lc-lens '((1 0 1 0) (1 0 0 0) (0 1 0 0) (1 1 1 1))
                        '(4 4 4 4)
                        '(3 (0 3) (1 2) (4 6)))
      '((1 1 1 2) (1 2 0 2) (1 2 1 1))
      equal)

(test (possible-lc-lens '((1 0 1 0) (1 0 0 0) (0 1 0 0) (1 1 1 1))
                        '(4 4 4 4)
                        '(1 (0 3) (1 2) (4 6)))
      nil
      equal)

(defclass distgen ()
    ((lc-supply)
     (permuts)
     (len)
     (cur)
     (total-weight :reader total-weight)))

(defgeneric next (distgen))

(defmethod initialize-instance ((this distgen) &key (cards (all-cards)) hcp c d h s)
    (let ((supply (supply cards)))
        (with-slots (lc-supply len cur permuts total-weight) this
            (setf lc-supply (mapcar #'first supply))
            (setf permuts (hc-permuts supply :hcp hcp :suitdef (list c d h s)))
            (setf len (list c d h s))
            (setf cur nil)
            (setf total-weight (apply #'+ (loop for x = (next this)
                                                while x
                                                collect (first x)))))))

(defmethod next ((self distgen))
    (with-slots (cur permuts len lc-supply) self
        (if (not cur) (setf cur (copy-list permuts))
                      (setf cur (cdr cur)))
        (let ((permut (car cur)))
            (if permut (list (apply #'+ (mapcar (curry #'permut-weight lc-supply)
                                                (possible-lc-lens permut lc-supply len)))
                             permut)))))

(file-based-test "test/distgen-test.dat"
  (let ((it (make-instance 'distgen :c '(5 nil) :d '(0 4) :h '(0 1) :hcp '(10 15) )))
    (loop for x = (next it)
          while x
          do (test-next x))))

(defun dist-from-hc (hc-distrib lc-supply len)
    (let* ((permuts (possible-lc-lens hc-distrib lc-supply len))
           (opts (zip (mapcar (curry #'permut-weight lc-supply) permuts)
                     permuts)))
      (loop for hc in hc-distrib
            for lc in (randcar-weights opts)
                collect (cons lc hc))))

(defmethod rand-dist ((self distgen) &key dist)
    (with-slots (lc-supply len cur) self
        (dist-from-hc (or dist (car cur)) lc-supply len)))

(defun distrib-hand (distrib cards)
    (let ((breakdown (apply #'append (mapcar #'hcp-breakdown
                                            (suit-breakdown cards)))))
       (mapcar (curry #'apply #'append)
               (apply #'zip (loop for amount in (apply #'append distrib)
                                  for supply in breakdown
                                  collect (deal amount supply))))))

(defun mkhand (x &optional id) (make-instance 'hand := x :id id))

(defmethod rand-hand ((self distgen) cards)
    (let ((x (random (total-weight self))))
        (setf (slot-value self 'cur) nil)
        (loop for opt = (next self)
                  while (> x (first opt))
              do (setf x (- x (first opt)))))
    (let ((hand (distrib-hand (rand-dist self) cards)))
        (list (first hand)
              (mkhand (hand (second hand))))))

(defmacro fold-param-lists (initial defs params expression &key (default 'acc))
    (if (> (length params) 1)
        `(fold (lambda (acc val)
                  (destructuring-bind (&key ,@params &allow-other-keys) val
                     (loop for val in (list ,@params)
                           for acc in acc
                           collect (if (not val) ,default ,expression))))
           ,initial
           ,defs)
        `(fold (lambda (acc val)
                  (destructuring-bind (&key ,@params &allow-other-keys) val
                      (if (not ,@params) acc
                        ,expression)))
           ,initial
           ,defs)))

(defun -? (a b)
    (cond ((not a) b)
          ((not b) a)
          (t (- a b))))

(defun hands-in-supply (supply)
    (/ (apply #'+ (mapcar (curry #'apply #'+) supply)) 13))

(define-condition bad-range (error)
    ((down :initarg :down :reader down)
     (up :initarg :up :reader up)
     (msg :initarg :msg :reader msg)
     (val :initarg :val :initform nil :reader val))
    (:report (lambda (obj out)
                (if (val obj)
                    (format out "~A: val '~A' in <~A-~A>"
                            (or (msg obj) "out of range")
                                (val obj) (down obj)
                                (up obj))
                    (format out "~A: <~A-~A>"
                            (or (msg obj) "out of range")
                            (down obj)
                            (up obj))))))
                            

(defun infer-hcp-range (supply base-params &rest other-defs)
    (let* ((total-hcp (permut-hcp (apply #'append (mapcar #'cdr supply))))
           (min-hcp (if (= (hands-in-supply supply) (+ (length other-defs) 1))
                        (fold-param-lists total-hcp other-defs
                           (hcp) (if (= acc 0) acc
                                               (max (-? acc (if (listp hcp) (second hcp) hcp)) 0)))
                        0))
           (max-hcp (fold-param-lists total-hcp other-defs
                        (hcp) (-? acc (if (listp hcp) (first hcp) hcp)))))
        (destructuring-bind (&key hcp &allow-other-keys) base-params
            (let ((up-limit (min max-hcp total-hcp)))
                (cond ((not hcp) (if (or (< up-limit total-hcp)
                                         (> min-hcp 0)) 
                                     `(:hcp (,min-hcp ,up-limit))))
                      ((listp hcp) (let ((down (max (or (first hcp) 0) min-hcp))
                                         (up (if (second hcp) (min (second hcp) up-limit)
                                                              up-limit)))
                                      (if (> down up) 
                                          (error 'bad-range :msg "Can't infer correct limit!"
                                                            :down down :up up))
                                      `(:hcp (,down
                                              ,up))))
                      (t (if (or (> hcp up-limit)
                                 (< hcp min-hcp))
                             (error 'bad-range :msg "Can't infer correct limit!"
                                               :down min-hcp :up max-hcp :val hcp))
                         `(:hcp ,hcp)))))))

(test (infer-hcp-range (supply (all-cards)) '(:hcp (11 15) :c (6 nil) :h (0 2))
                                            '(:hcp (12 15))
                                            '(:hcp (15 17)))
      '(:hcp (11 13))
      equal)

(test (infer-hcp-range '((5 1 1 1 1) (8 0 1 1 1) (7 1 1 0 0) (6 0 0 0 1))
                       '(:h (7 nil))
                       '(:hcp (10 17)))
      '(:hcp (0 16))
      equal)

(test (infer-hcp-range '((5 1 1 1 1) (8 0 1 1 1) (7 1 1 0 0) (6 0 0 0 1))
                       '(:hcp (8 nil) :h (7 nil))
                       '(:hcp (10 17)))
      '(:hcp (8 16))
      equal)

(test (infer-hcp-range (supply (all-cards)) nil
                                            '(:c (5 7) :d (0 2)))
      nil
      equal)

(test (infer-hcp-range '((9 1 1 1 1)) '(:hcp (4 6)) '(:hcp 6))
      '(:hcp (4 4))
      equal)

(test (infer-hcp-range '((6 1 1 1 1) (9 0 0 1 0) (9 0 0 0 0) (6 1 1 1 1))
                        '(:hcp (0 11)) '(:hcp (0 10)) '(:hcp (0 10)))
      '(:hcp (3 11))
      equal)
    
(defun infer-suit-ranges (supply base-params &rest other-defs)
    (let* ((suitlen (mapcar (curry #'apply #'+) supply))
           (min-len (if (= (hands-in-supply supply) (+ (length other-defs) 1))
                        (fold-param-lists suitlen other-defs
                            (c d h s) (if (= acc 0) acc
                                                    (-? acc (if (listp val) (second val) val)))
                            :default 0)
                        (loop for _ in suitlen collect 0)))
           (max-len (fold-param-lists suitlen other-defs
                        (c d h s) (-? acc (if (listp val) (first val) val))))
           (suit-hcp-defs (loop for suit in '(:c :d :h :s)
                                collect (mapcar (f* (curry* #'getf (def) (def suit))
                                                    #'ensure-list
                                                    #'third
                                                    (curry #'list :hcp))
                                                other-defs))))
        (destructuring-bind (&key c d h s &allow-other-keys) base-params
            (loop for slen in suitlen
                  for maxlen in max-len
                  for minlen in min-len
                  for suit-hcp in suit-hcp-defs
                  for suit-supply in supply
                  for key in '(:c :d :h :s)
                  for def in (list c d h s)
                  append (let ((up-limit (min maxlen slen)))
                              (cond ((not def) (if (< up-limit slen) `(,key (,minlen ,up-limit))))
                                    ((listp def) (let ((down (if (first def) (max (first def) minlen)
                                                                             minlen))
                                                       (up (if (second def) (min (second def) up-limit)
                                                                            up-limit))
                                                       (hcp (if (listp def) (third def))))
                                                    (if (> down up-limit) 
                                                        (error 'bad-range :msg "Can't infer correct limit!"
                                                               :down down :up up))
                                                    `(,key (,down ,up
                                                            ,@(cdr (apply #'infer-hcp-range
                                                                         `((,suit-supply) (:hcp ,hcp) ,@suit-hcp)))))))
                                    (t (if (or (> def up-limit)
                                               (< def minlen))
                                           (error 'bad-range :msg "Can't infer correct limit!"
                                                             :down minlen :up up-limit :val def)
                                           `(,key ,def)))))))))

(test (infer-suit-ranges (supply (all-cards)) '(:hcp (11 15) :c (6 nil) :h (0 2))
                                              '(:hcp (12 15) :c (3 5) :d (1 5))
                                              '(:hcp (15 17)))
      '(:c (6 10) :d (0 12) :h (0 2))
      equal)

(test (infer-suit-ranges '((5 1 1 1 1) (8 0 1 1 1) (7 1 1 0 0) (6 0 0 0 1))
                         '(:hcp (8 nil) :h (7 nil))
                         '(:hcp (10 17) :h (1 5)))
      '(:h (7 8))
      equal)

(test (infer-suit-ranges '((5 1 1 1 1) (8 0 1 1 1) (6 0 1 1 1) (6 0 0 0 1))
                         '(:hcp (8 nil) :h (7 nil (5 nil)))
                         '(:hcp (10 17) :h (1 5)))
      '(:h (7 8 (5 9)))
      equal)

(test (infer-suit-ranges '((5 1 1 1 1) (8 0 1 1 1) (6 0 1 1 1) (6 0 0 0 1))
                         '(:hcp (8 nil) :h (7 nil (5 nil)))
                         '(:hcp (10 17) :h (1 5 (4 nil))))
      '(:h (7 8 (5 5)))
      equal)

(test (infer-suit-ranges (supply (all-cards)) '(:hcp (11 15) :h (0 2))
                                              '(:hcp (12 15) :c (3 5) :d (1 5))
                                              '(:hcp (15 17) :c 1)
                                              '(:c 3))
      '(:c (4 6) :d (0 12) :h (0 2))
      equal)

(defun infer-distgen-params (supply base others)
   (append (apply #'infer-hcp-range `(,supply ,base ,@others))
           (apply #'infer-suit-ranges `(,supply ,base ,@others))))

(defclass deal ()
    ((defs :initarg :~ :initform nil :reader defs)
     (const-hands :initarg := :initform nil :reader const-cands)
     (rootgen :initform nil)))

(defmethod build ((this deal))
    (with-slots (defs const-hands rootgen) this
        (let ((cards (if const-hands (second (remove-cards (apply #'append (mapcar #'unhand const-hands))
                                                               (all-cards)))
                                     (all-cards))))
            (if (not rootgen)
                (setf rootgen (apply #'make-instance `(distgen ,@(infer-distgen-params (supply cards)
                                                                                       (car defs)
                                                                                       (cdr defs))
                                                               :cards ,cards))))
            (labels ((self (rest defs acc)
                       (if (and defs (> (length rest) 13))
                           (let-from! (rand-hand (apply #'make-instance 
                                                        (append (list 'distgen :cards rest)
                                                                (infer-distgen-params (supply rest)
                                                                                      (car defs)
                                                                                      (cdr defs))))
                                                 rest)
                                      (rest* hand)
                              (self rest* (cdr defs) (append acc (list hand))))
                           (list rest acc))))
                (let-from! (rand-hand rootgen cards) (rest hand)
                   (let-from! (self rest (cdr defs) (list hand))
                              (rest* hands)
                       (if rest* (append (mapcar (f* #'unhand #'hand #'mkhand) const-hands)
                                         hands 
                                         (mapcar (f* #'hand #'mkhand) (deal-all 13 rest*)))
                                 hands)))))))
          
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
    (sort (mapcar #'length hand) #'>))

(defun fit-n-distrib (trump)
    (list (lambda (hand)
             (length (nth trump hand)))
          (lambda (hand)
             (sort (mapcar #'length (second (take trump hand))) #'<))))

(defun f-cons (&rest functions)
    (lambda (&rest input)
        (mapcar (lambda (f) (apply f input))
                functions)))

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

(defmethod initialize-instance ((self trick) &key suit trump high winner remaining)
    (setf (slot-value self 'high) (if (listp high) (or high (list (normsuit suit) -1))
                                                   (list (normsuit suit) high)))
    (setf (slot-value self 'suit) (normsuit suit))
    (setf (slot-value self 'trump) (normsuit trump))
    (setf (slot-value self 'winner) (or winner -1))
    (setf (slot-value self 'cards) nil)
    (setf (slot-value self 'remaining) remaining))

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
        (and (>= winner 0)
             (= (mod (length remaining) 2)
                (mod winner 2)))))

(defmethod roll-trick ((self trick) n)
    (with-slots (winner cards remaining) self
        (setf winner (mod (+ winner n) 4))
        (setf cards (roll n cards))
        (setf remaining (roll n remaining)))
    self)

(defmethod score ((self trick))
    (if (= (mod (winner self) 2) 0) 1 -1))

(defmacro make-trick (suit trump &body actions)
    `(let ((trick (make-instance 'trick :suit ,suit :trump ,trump)))
        ,@actions
        trick))

(defclass hand ()
    ((suits :reader suits)
     (id :reader handid)))

(defmethod initialize-instance ((self hand) &key = id)
    (setf (slot-value self 'id) (or id (format nil "~x" (random 255))))
    (setf (slot-value self 'suits) (mapcar (lambda (x) (sort x #'<))
                                           =)))

(defun str2suitno (x)
    (position x '("♣" "♦" "♥" "♠") :test #'equal))

(defun str2hand (str)
    (labels ((str2ranks (str &optional acc)
                (if (= (length str) 0)
                    acc
                    (let ((h (char str 0)))
                      (cond ((eq h #\1) (str2ranks (subseq str 2) (cons 8 acc)))
                            ((eq h #\space) acc)
                            ((str2ranks (subseq str 1)
                                        (cons (position h '(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\1 #\J #\Q #\K #\A)) acc)))))))
            (suits (fields &optional (acc '(nil nil nil nil)))
                (cond ((not fields) acc)
                      ((str2suitno (second fields)) 
                            (setf (nth (str2suitno (first fields)) acc) nil)
                            (suits (cdr fields) acc))
                      (t (let ((suitno (str2suitno (car fields))))
                            (setf (nth suitno acc) (str2ranks (second fields)))
                            (suits (nthcdr 2 fields) acc)))))
            (read-suits (suits &optional handid) 
                (mkhand (suits (splitstr " " suits)) handid)))
      (let-from! (splitstr ": " str 1)
                 (a b)
        (if b (read-suits b a) (read-suits a)))))
         
(test (suits (str2hand "N: ♣ KJ96 ♦ A8 ♥ AJ83 ♠ AK9"))
      '((4 7 9 11) (6 12) (1 6 9 12) (7 11 12))
      equal)

(test (handid (str2hand "N: ♣ KJ96 ♦ A8 ♥ AJ83 ♠ AK9"))
      "N"
      equal)

(defmethod pretty-tree ((this hand))
    (loop for s in (suits this)
          collect (mapcar (curry* #'nth (x) (x '(2 3 4 5 6 7 8 9 10 J Q K A)))
                          (reverse s))))
(defun str2deal (str)
    (loop for handstr in (splitstr (format nil "~%") str)
          collect (str2hand handstr)))

(test (mapcar #'suits (str2deal "N: ♣ KJ96 ♦ A8 ♥ AJ83 ♠ AK9
E: ♣ Q5 ♦ J9762 ♥ Q ♠ J6543
S: ♣ 432 ♦ 1043 ♥ K10954 ♠ 87
W: ♣ A1087 ♦ KQ5 ♥ 762 ♠ Q102"))
    '(((4 7 9 11) (6 12) (1 6 9 12) (7 11 12)) ((3 10) (0 4 5 7 9) (10) (1 2 3 4 9))
      ((0 1 2) (1 2 8) (2 3 7 8 11) (5 6)) ((5 6 8 12) (3 10 11) (0 4 5) (0 8 10)))
    equal)

(test (mapcar #'handid (str2deal "N: ♣ KJ96 ♦ A8 ♥ AJ83 ♠ AK9
E: ♣ Q5 ♦ J9762 ♥ Q ♠ J6543
S: ♣ 432 ♦ 1043 ♥ K10954 ♠ 87
W: ♣ A1087 ♦ KQ5 ♥ 762 ♠ Q102"))
      '("N" "E" "S" "W")
      equal)
      
(defmethod print-object ((self hand) out)
    (format out "HAND<~A>" (print-hand (suits self))))

(defmethod hand-suit ((self hand) suit)
    (nth (suitno suit) (suits self)))

(defmethod beats? ((a hand) (b hand) suit trump)
    (let ((acards (nth (suitno suit) (suits a)))
          (bcards (nth (suitno suit) (suits b)))
          (atrumps (and trump (nth (suitno trump) (suits a))))
          (btrumps (and trump (nth (suitno trump) (suits b)))))
       (cond ((and acards bcards) (> (apply #'max acards) (apply #'max bcards)))
             ((and (not acards) (not bcards) atrumps btrumps)
                  (> (apply #'max atrumps) (apply #'max btrumps)))
             ((and (not acards) atrumps) T)
             (T nil))))

(defmethod play ((self hand) suit (chooser function))
    (with-slots (suits id) self 
        (let ((played (nth suit suits)))
            (if played (let ((ans (funcall chooser played)))
                               (if ans(list (list suit (car ans))
                                            (make-instance 'hand := (append (subseq suits 0 suit)
                                                                            (cons (second ans)
                                                                                  (subseq suits (+ suit 1))))
                                                                 :id id))))))))

(defmethod as-list ((self trick))
    (cons (car (last (cards self))) (mapcar #'suits (remaining self))))

(defmethod as-summary ((self trick))
    (cons (score self) (mapcar #'suits (remaining self))))

(defmethod weak-suit ((self hand) trick)
    (with-slots (trump remaining) trick
        (first (fold (lambda (acc val)
                        (let-from* val (suit cards)
                           (let-from* acc (accsuit accards)
                               (cond ((not cards) acc)
                                     ((not acc) val)
                                     ((eq suit trump) acc)
                                     ((eq accsuit trump) val)
                                     ((and (not (find-if (curry #'< 8) cards))
                                           (not (find-if (curry #'< 8) accards)))
                                          (if (< (length cards) (length accards))
                                              val acc))
                                     ((not (find-if (curry #'< 8) cards)) val)
                                     ((not (find-if (curry #'< 8) accards)) acc)
                                     (t (let* ((rem-a (apply #'append (mapcar (curry* #'hand-suit (x) (x suit))
                                                                              remaining)))
                                               (rem-b (apply #'append (mapcar (curry* #'hand-suit (x) (x accsuit))
                                                                              remaining)))
                                               (buffer-a (- (length cards)
                                                            (length (filter (curry #'< (apply #'max cards))
                                                                            rem-a))))
                                               (buffer-b (- (length accards)
                                                            (length (filter(curry #'< (apply #'max accards))
                                                                                    rem-b)))))
                                            (cond ((= buffer-a buffer-b)
                                                      (if (< (car cards) (car accards))
                                                          val acc))
                                                  ((or (and (> buffer-a 0)
                                                            (> buffer-b 0))
                                                       (and (< buffer-a 0)
                                                            (< buffer-b 0)))
                                                      (if (> buffer-a buffer-b) val acc))
                                                  ((> buffer-a 0) val)
                                                  (t acc))))))))
                     nil
                     (zip-id (suits self))))))

(test (weak-suit (make-instance 'hand := '((2 3) (1 2 4 11) (6 9) (0 1 3)))
                 (make-instance 'trick :remaining (list (mkhand '((6 7) (3 5 12) (3 5 7 12) (8 12)))
                                                        (mkhand '((2 3) (1 2 4 11) (6 9) (0 1 3)))
                                                        (mkhand '(NIL (0 6 7 8) (1 10) (2 5 6 7 11)))
                                                        (mkhand '((10) (9 10) (0 2 4 8 11) (4 9 10))))))
      0 eq)

(test (weak-suit (make-instance 'hand := '(() (4 11) () (1 10)))
                 (make-instance 'trick :remaining (list (mkhand '(() (5 12) () (0 8)))
                                                        (mkhand '(() (4 11) () (1 10)))
                                                        (mkhand '(NIL (7 8) () (6 7)))
                                                        (mkhand '(() (9 10) () (3 9))))))
      3 eq)

(defun lowest-card (cards)
    (list (car cards) (cdr cards)))

(defun closest-card (card cards &optional acc)
    (cond ((not cards) (if acc (take (- (length acc) 1) acc)))
          ((>= (car cards) card) (list (car cards) (append acc (cdr cards))))
          (t (closest-card card (cdr cards) (append acc (list (car cards)))))))
 
(test (closest-card 1 '(4 11))
      '(4 (11))
      equal)

(test (closest-card 5 '(4 11))
      '(11 (4))
      equal)

(test (closest-card 12 '(4 11))
      '(11 (4))
      equal)

(defun beat-or-low (trick hand &key use-highest)
    (with-slots (suit high trump) trick
      (let-from* high (hsuit hrank)
        (let-from! (cond ((and hsuit (eq hsuit trump) (not (eq suit trump)))
                            (or (play hand suit #'lowest-card)
                                (if (not (winnerp? trick))
                                    (play hand trump (lambda (cards)
                                                        (let ((ans (position-if (curry #'< hrank) cards)))
                                                           (if ans (take ans cards))))))
                                (play hand (weak-suit hand trick) #'lowest-card)
                                (if trump (play hand trump #'lowest-card))))

                         ((and hsuit (eq hsuit suit))
                            (or (play hand suit (lambda (cards)
                                                    (or (and (not (winnerp? trick))
                                                             (take-higher cards hrank
                                                                          :use-highest use-highest))
                                                        (lowest-card cards))))
                                (and trump (play hand trump #'lowest-card))
                                (play hand (weak-suit hand trick) #'lowest-card)
                                (if trump (play hand trump #'lowest-card))))

                         (t (or (play hand suit (lambda (cards)
                                                    (or (take-higher cards -1
                                                                     :use-highest use-highest)
                                                        (lowest-card cards))))
                                (and trump (play hand trump #'lowest-card))
                                (play hand (weak-suit hand trick) #'lowest-card)
                                (if trump (play hand trump #'lowest-card)))))
                   (high remaining)
              (if remaining (play trick high remaining))))))

(test (as-list (beat-or-low (make-instance 'trick :suit 'H :trump nil :high '(2 7) :winner 1 
                                     :remaining (mapcar #'str2hand '("N: ♣  ♦ Q4 ♥ A74 ♠ AJ1076432" 
                                                                     "E: ♣ KQ ♦ K63 ♥ KQJ52 ♠ KQ5" 
                                                                     "S: ♣ AJ1098763 ♦ AJ8 ♥ 109 ♠")))
                            (str2hand "N: ♣  ♦ Q4 ♥ A74 ♠ AJ1076432")))
         '((2 2) (NIL (2 10) (2 5 12) (0 1 2 4 5 8 9 12))
                 ((10 11) (1 4 11) (0 3 9 10 11) (3 10 11))
                 ((1 4 5 6 7 8 9 12) (6 9 12) (7 8) NIL)
                 (NIL (2 10) (5 12) (0 1 2 4 5 8 9 12)))
      equal)

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

(defun finesse-if-possible (trick h1 h2)
    (with-slots (high suit trump) trick
        (let-from! (and (not (winnerp? trick)) high) (hsuit hrank)
            (let-from! (cond ((and hsuit (eq hsuit trump) (not (eq trump suit)))
                                (or (play h1 suit #'lowest-card)
                                    (if (and (nth suit (suits h2)) (not (eq suit trump)))
                                        (play h1 trump (curry #'take-match (curry #'< hrank)))
                                        (let ((h2-max (or (apply #'max? (nth trump (suits h2))) -1)))
                                            (play h1 trump (curry #'take-match (andf (curry #'< hrank)
                                                                                     (curry #'< h2-max))))))
                                    (play h1 (weak-suit h1 trick) #'lowest-card)))
                             ((and hsuit (eq hsuit suit))
                                (or (if (nth suit (suits h2))
                                        (let ((h2-max (apply #'max? (nth suit (suits h2)))))
                                           (or (play h1 suit (curry #'take-match (andf (curry #'< h2-max)
                                                                                       (curry #'< hrank))))
                                               (play h1 suit (curry #'take-match (curry #'< hrank)))))
                                        (play h1 suit (curry #'take-match (curry #'< hrank))))
                                    (play h1 suit #'lowest-card)

                                    (if (and trump (not (nth suit (suits h2)))
                                             (nth trump (suits h2)))
                                        (let ((h2-max (apply #'max (nth trump (suits h2)))))
                                            (play h1 trump (curry #'take-match (curry #'< h2-max))))
                                        (if (and trump (not (winnerp? trick)))
                                            (play h1 trump #'lowest-card)))

                                    (play h1 (weak-suit h1 trick) #'lowest-card)))
                             (t (or (if (nth suit (suits h2))
                                        (let ((h2-max (apply #'max (nth suit (suits h2)))))
                                           (play h1 suit (curry #'take-match (curry #'< h2-max)))))
                                    (play h1 suit #'lowest-card)

                                    (if (and trump (not (nth suit (suits h2)))
                                             (nth trump (suits h2)))
                                        (let ((h2-max (apply #'max (nth trump (suits h2)))))
                                            (play h1 trump (curry #'take-match (curry #'< h2-max))))
                                        (if (and trump (not (winnerp? trick)))
                                            (play h1 trump #'lowest-card)))

                                    (play h1 (weak-suit h1 trick) #'lowest-card))))
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

(test (as-list (finesse-if-possible (make-instance 'trick :suit 'D :trump 'C :high 11 :winner 0)
                                    (make-instance 'hand := '((1 10) () (4 6) (3 6 8 12)))
                                    (make-instance 'hand := '((0 9 11) (9 10) (5 8 11) (5 9 10 11)))))
      '((2 4) ((1 10) () (6) (3 6 8 12)))
      equal)

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
           ((2 3 5 7 8 12) (0 3 6 11) (7) NIL)
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

(defclass cache ()
    ((action :initarg :! :reader action)
     (hits :initform (make-hash-table :test #'equal))))

(defmethod run ((self cache) &rest args)
    (with-slots (action hits) self
        (let ((hit (gethash args hits)))
            (if hit hit
                (let ((ans (apply action (cons self args))))
                    (setf (gethash args hits) ans)
                    ans)))))

(defclass outcome ()
   ((tricks :initarg :tricks :initform nil :reader tricks)
    (winners :initarg :winners :initform nil :reader winners)
    (winner-nos :initarg :winner-nos :initform nil :reader winner-nos)
    (score :initform '(0 0) :initarg :score :reader score)
    (roll :initform 0 :initarg :roll)
    (remaining :initarg :remaining :reader remaining)))

(defmethod print-object ((self outcome) out)
    (format out "OUTCOME<~A => ~A / ~A>" (mapcar (curry #'mapcar #'cardstr) (tricks self)) 
                                         (remaining self) (score self)))

(defmethod new-outcome (trick)
    (make-instance 'outcome :tricks (list (cards trick))
                            :winners (list (nth (winner trick) (cards trick)))
                            :winner-nos (list (winner trick))
                            :score (if (eq (mod (winner trick) 2) 0) '(1 0) '(0 1))
                            :roll (winner trick)
                            :remaining (roll (- (winner trick)) (remaining trick))))

(defmethod add-outcome ((self outcome) (other outcome))
    (with-slots (tricks score winners winner-nos roll remaining) self
        (setf tricks (append tricks (tricks other)))
        (setf winners (append winners (winners other)))
        (setf winner-nos (append winner-nos (winner-nos other)))
        (setf score (loop for a in score
                          for b in (if (eq (mod roll 2) 1) (reverse (score other))
                                                           (score other))
                          collect (+ a b)))
        (setf roll (slot-value other 'roll))
        (setf remaining (remaining other)))
    self)

(defmethod add-outcome* ((self outcome) (other outcome))
    (with-slots (tricks score winners winner-nos roll remaining) self
        (make-instance 'outcome :tricks (append tricks (tricks other))
                                :winners (append winners (winners other))
                                :winner-nos (append winner-nos (winner-nos other))
                                :score (loop for a in score
                                             for b in (if (eq (mod roll 2) 1) (reverse (score other))
                                                                              (score other))
                                             collect (+ a b))
                                :roll (mod (+ roll (slot-value other 'roll)) 4)
                                :remaining (remaining other))))

(defmethod won? ((self outcome))
    (if (winner-nos self)
        (= (mod (car (winner-nos self)) 2) 0)))

(defmethod do-next ((self outcome) if-won &optional if-lost)
    (with-slots (winner-nos remaining) self
       (if winner-nos
           (if (won? self)
               (add-outcome self (apply if-won remaining))
               (if if-lost (add-outcome self (apply if-lost remaining)))))))

(defun best-outcome (&rest outcomes)
    (fold (lambda (acc val)
             (if (or (not acc) (and val (< (car (score acc)) (car (score val)))))
                 val acc))
          (car outcomes)
          (cdr outcomes)))

(defun best-outcome* (roll &rest outcomes)
    (let ((current (mod roll 2)))
        (fold (lambda (acc val)
                 (if (or (not acc) (and val (< (nth current (score acc)) 
                                               (nth current (score val)))))
                     val acc))
              (car outcomes)
              (cdr outcomes))))

(defun simple-trick (trump suit a b c d &key use-highest)
    (if (> (length (hand-suit a suit)) 0)
        (new-outcome (make-trick suit trump
                        (beat-or-low trick a :use-highest use-highest)
                        (if (beats? d c suit trump) (beat-or-low trick b)
                                                    (finesse-if-possible trick b c))
                        (finesse-if-possible trick c d)
                        (beat-or-low trick d)))
        (make-instance 'outcome :remaining (list a b c d))))
        
(defun suit-tricks (trump suit a b c d)
    (labels ((play-through (&rest hands)
                (apply #'best-outcome
                    (mapcar (curry #'apply 
                                   (lambda (top hands)
                                        (or (do-next (apply #'simple-trick
                                                            `(,trump ,suit ,@hands :use-highest ,top))
                                                     #'play-through #'play-through)
                                            (make-instance 'outcome :remaining hands))))
                            (list-prod '(T NIL) (list hands (roll 2 hands)))))))
        (fold (lambda (acc trick)
                 (if (first trick) (cons (cons (second trick) (car acc)) (cdr acc))
                                   `(nil (,(second trick) ,@(car acc)) ,@(cdr acc))))
              nil
              (with-slots (winners winner-nos) (play-through a b c d)
                  (reverse (loop for player in winner-nos
                                 for card in winners
                                 collect (list (= (mod player 2) 0) card)))))))

(defclass suit-value ()
    ((suit :reader suit)
     (trump :reader trump)
     (tricks :reader tricks)
     (summary :reader summary)))

(defmethod initialize-instance ((this suit-value) &key trump suit deal)
    (setf (slot-value this 'suit) suit)
    (setf (slot-value this 'trump) trump)
    (let ((tricks (apply #'suit-tricks `(,trump ,suit ,@deal))))
        (setf (slot-value this 'tricks) tricks)
        (setf (slot-value this 'summary) (mapcar #'length tricks))))

(defmethod print-object ((self suit-value) out)
    (format out "SUIT-VALUE<~A/~A : ~A>" 
                (suit self) (trump self)
                (mapcar (curry #'mapcar #'cardstr) (tricks self))))
                
(defmethod ours ((this suit-value))
    (with-slots (summary) this
        (apply #'+ (loop for i from 0 to (- (length summary) 1) by 2
                         collect (nth i summary)))))

(defmethod theirs ((this suit-value))
    (with-slots (summary) this
        (apply #'+ (loop for i from 1 to (- (length summary) 1) by 2
                         collect (nth i summary)))))
                         
(defmethod better ((self suit-value) other)
    (if (and other (> (ours other) (ours self))) other self))

(defclass deal-plan ()
    ((deal)
    (trump :reader trump)
    (iwins :reader iwins)
    (ilosers :reader ilosers)
    (suits :reader suits)))

(defmethod initialize-instance ((this deal-plan) &key = trump)
    (with-slots (deal) this
        (setf deal =)
        (setf (slot-value this 'trump) trump)
        (let ((suits (sort (filter (f* #'ours (curry #'< 0)) 
                                   (loop for suit in '(c d h s)
                                         collect (better (make-instance 'suit-value :suit suit :deal =)
                                                         (if trump (make-instance 'suit-value :trump trump 
                                                                                              :suit suit 
                                                                                              :deal =)))))
                           (lambda (a b) (> (ours a) (ours b))))))
          (setf (slot-value this 'iwins) (apply #'+ (mapcar (f* #'summary #'first) suits)))
          (setf (slot-value this 'ilosers) (apply #'+ (mapcar #'second 
                                                      (filter (f* #'first (curry #'= 0)) 
                                                              (mapcar #'summary suits)))))
          (setf (slot-value this 'suits)
                (if trump (append (filter #'trump suits)
                                  (filter (lambda (x) (eq (suit x) trump))
                                          suits)
                                  (filter (lambda (x) (not (or (eq trump (suit x)) (trump x)))) suits))
                          suits)))))

(defclass play-route ()
    ((from :initarg :from :initform nil :reader from)
     (to :initarg :to :reader to)
     (to-suit-void :initform nil :initarg :to-suit-void :reader to-suit-void)
     (tricks :initarg :tricks :reader tricks)))

(defmethod print-object ((self play-route) out)
    (with-slots (from to to-suit-void tricks) self
        (format out "PLAY-ROUTE<~A->~A @ ~A: ~A" from to to-suit-void
                (mapcar (curry #'mapcar #'cardstr) tricks))))

(defmethod reproduce ((route play-route) hands trump)
    (with-slots (tricks) route
        (if (not tricks) nil
            (let* ((ref-trick (let ((base (pop tricks)))
                                 (if (find (seektree '(0 1) base)
                                           (hand-suit (first hands) (seektree '(0 0) base)))
                                     base (roll 2 base))))
                   (suit (seektree '(0 0) ref-trick))
                   (first-card (play (first hands) suit (curry #'closest-card (seektree  '(0 1) ref-trick))))
                   (ans (if first-card
                            (new-outcome (make-trick suit trump
                                           (apply #'play (cons trick first-card))
                                           (if (beats? (fourth hands) (third hands)  
                                                       suit trump)
                                               (beat-or-low trick (second hands))
                                               (finesse-if-possible trick (second hands) 
                                                                          (third hands)))
                                           (apply #'play
                                                  (cons trick 
                                                        (or (play (third hands)
                                                                  (seektree '(2 0) ref-trick)
                                                                  (curry #'closest-card (seektree  '(2 1) ref-trick)))
                                                            (play (third hands)
                                                                  (weak-suit (third hands) trick)
                                                                  #'lowest-card))))
                                           (beat-or-low trick (fourth hands)))))))
               (if (and ans (won? ans)) ans (reproduce route hands trump))))))
                
(defmethod rem-break-div ((self play-route) trump cards)
    (with-slots (tricks from to to-suit-void) self
        (let ((break-cards (if (not from) (mapcar #'third tricks)))
              (remove-cards (cond ((not from) (mapcar (f* #'first #'list) tricks))
                                  ((eq from to) (mapcar (f* #'first #'list) tricks))
                                  (t (mapcar (curry* #'reorder (lst) (lst '(0 2))) tricks)))))
            (divlist (list #'third #'second) 
                    (mapcar (lambda (card) 
                              (let* ((break-id (position card break-cards :test #'equal))
                                     (rem-id (position-if (curry* #'position (rc) (card rc :test #'equal))
                                                          remove-cards)))
                                 (list card break-id rem-id)))
                            cards)
                    :map (lambda* (a b c)
                            (list a (or b c)))))))

(defmethod del-at ((self play-route) ids)
    (with-slots (from to to-suit-void tricks) self
        (make-instance 'play-route :from from :to to 
                       :to-suit-void to-suit-void
                       :tricks (loop for tr in tricks
                                     for tid from 0
                                     append (if (position tid ids) nil (list tr))))))
                       
(defmethod add-at ((self play-route) new-tricks)
    (with-slots (from to to-suit-void tricks) self
        (make-instance 'play-route :from from :to to 
                       :to-suit-void to-suit-void
                       :tricks (append tricks new-tricks))))

(defclass play-com ()
    ((zero-id :initarg :zero-id :reader zero-id)
     (trump :initarg :trump :reader trump)
     (active :initarg :active :reader active)
     (pending :initarg :pending :reader pending)
     (guards :initarg :guards :initform nil :reader guards)))
 
(defmethod print-object ((self play-com) out)
    (with-slots (active pending guards) self
        (format out "PLAY-COM<~A /~%~A/~%~A>" active pending guards)))

       
(defun mk-play-com (trump routes guards a c)
    (destructuring-bind (empty active pending)
                        (divlist (list (lambda (route) (not (tricks route)))
                                       (lambda (route)
                                         (with-slots (to-suit-void) route
                                         (or (not to-suit-void)
                                             (not (and (hand-suit a to-suit-void)
                                                       (hand-suit c to-suit-void)))))))
                                 routes)
        (declare(ignore empty))
        (make-instance 'play-com :zero-id (handid a) :trump trump 
                                 :active active :pending pending
                                 :guards guards)))
                                                              
(defmethod mk-route ((com play-com) a b c d)
    (flet ((player-routes (player)
            (sort (filter (orf (lambda (x) (not (from x)))
                               (f* #'from (curry #'eq player)))
                          (active com))
                  (lambda (a b)
                    (cond ((eq (from a) (to a)) T)
                          ((eq (from b) (to b)) nil)
                          ((from a) T)
                          (T nil))))))
        (labels ((self (acc player-routes)
                    (if (not (car player-routes)) acc
                        (let ((ans (reproduce (seektree '(0 0) player-routes) (remaining acc) (trump com))))
                            (cond ((not ans) (self acc
                                                   (list (cdr (car player-routes)) 
                                                         (second player-routes))))
                                  ((eq (car (winner-nos ans)) 0)
                                             (self (add-outcome acc ans) player-routes))
                                  (T (self (add-outcome acc ans) (roll 1 player-routes))))))))
            (self (make-instance 'outcome :remaining (list a b c d))
                  (list (player-routes 0)
                        (player-routes 2))))))
                                                   
(defun play-routes (trump a b c d &key (suits '(c d h s)))
    (labels ((suit-tops (suit &rest hands)
                (or (apply #'best-outcome (mapcar (lambda* (top hands)
                                                  (do-next (apply #'simple-trick
                                                                  `(,trump ,suit ,@hands :use-highest ,top))
                                                           (curry #'suit-tops suit)))
                                                (list-prod '(T nil) (list hands (roll 2 hands)))))
                    (make-instance 'outcome :remaining hands)))
             (guards (suit &rest hands)
                (let* ((top-cards (sort (loop for hand in (mapcar (curry* #'hand-suit (hand) (hand suit))
                                                                  hands)
                                              for handid from 0
                                              append (mapcar (f* #'list (curry #'cons handid))
                                                             hand))
                                        (lambda (a b) (> (second a) (second b)))))
                       (last-guard (position-if (f* #'car (curry* #'mod (x) (x 2)) (curry #'= 1))
                                                top-cards)))
                  (cond ((not last-guard) (mapcar #'second top-cards))
                        ((= last-guard 0) nil)
                        (t (mapcar (curry #'second) (subseq top-cards 0 last-guard))))))
             (trick-type (&key left winnerno my? partners?)
                (let ((conds `(,@(if left '((not (eq (seektree '(0 0) cards)
                                                     (seektree '(2 0) cards)))))
                               ,@(if winnerno `((= winner ,winnerno)))
                               ,@(cond (my? '(my))
                                       (partners? '((not my))))))
                      (ignores `(,@(if (not left) '(cards))
                                 ,@(if (not winnerno) '(winner))
                                 ,@(if (not (or my? partners?)) '(my)))))
                   (eval `(lambda* (cards winner my)
                             ,@(mapcar (lambda (sym) `(declare (ignore ,sym)))
                                       ignores)
                             ,@(if (> (length conds) 1) `((and ,@conds)) conds)))))
             (void-vertices (from to tricks)
                (filter (curry #'tricks)
                        (loop for suit in suits
                              for suit-tricks in (divlist (mapcar (lambda (s) 
                                                                     (f* #'car #'car (curry #'eq s)))
                                                                  '(0 1 2))
                                                          tricks)
                              collect (make-instance 'play-route :from from :to to
                                                                 :to-suit-void suit
                                                                 :tricks suit-tricks)))))
      (destructuring-bind (guards my-left partner-left 
                           my-trumped partner-trumped 
                           my-lead partner-lead 
                           my partner)
            (divlist (list (trick-type :winnerno 1)
                           (trick-type :left T :winnerno 0 :my? T)
                           (trick-type :left T :winnerno 0 :partners? T)
                           (trick-type :left T :winnerno 2 :partners? T)
                           (trick-type :left T :winnerno 2 :my? T)
                           (trick-type :winnerno 2 :partners? T)
                           (trick-type :winnerno 2 :my? T)
                           (trick-type :my? T))
                     (loop for suit in suits
                           append (let* ((tops (suit-tops suit a b c d))
                                         (guards (apply #'guards (cons suit (roll 1 (remaining tops))))))
                                    (append (zip (tricks tops)
                                                 (winner-nos tops)
                                                 (loop for winner in (winners tops)
                                                       collect (find (second winner)
                                                                     (hand-suit a (first winner)))))
                                            (if guards `(((,(suitno suit) ,guards) 1))))))
                     :map #'car)
            (mk-play-com trump
                         (append (list (make-instance 'play-route :to 0 :tricks my)
                                       (make-instance 'play-route :to 2 :tricks partner)
                                       (make-instance 'play-route :from 0 :to 2 :tricks my-lead)
                                       (make-instance 'play-route :from 2 :to 0 :tricks partner-lead))
                                 (void-vertices 0 0 my-left)
                                 (void-vertices 0 2 my-trumped)
                                 (void-vertices 2 2 partner-left)
                                 (void-vertices 2 0 partner-trumped))
                         guards
                         a c))))
                                                                                         
(defun insert-route (route routes)
    (let ((p (position-if (lambda (r) (and (eq (from r) (from route))
                                           (eq (to r) (to route))
                                           (eq (to-suit-void r) (to-suit-void r))))
                          routes)))
      (if p (append (subseq routes 0 p)
                    (list (add-at (nth p routes) (tricks route)))
                    (subseq routes (+ p 1)))
            (cons route routes))))

(defun append-routes (a &rest other)
    (fold (lambda (acc val) (insert-route val acc))
          a
          (apply #'append other)))

(defmethod first-pass ((self play-com) trick)
    (fold (lambda (acc route)
             (destructuring-bind (routes remaining) acc
                (if remaining (destructuring-bind (remove break absent)
                                                  (rem-break-div route (trump self) remaining)
                                 (let* ((ids (mapcar #'second (append remove break)))
                                        (updated (if ids (del-at route ids) route))
                                        (break-ids (list- (mapcar #'second break)
                                                          (mapcar #'second remove)
                                                          :test #'equal))
                                        (other (if break-ids
                                                   (insert-route (make-instance 'play-route 
                                                                        :from (to route)
                                                                        :to (to route)
                                                                        :tricks (reorder (tricks route)
                                                                                         break-ids))
                                                                       routes)
                                                   routes)))
                                    (list (if (tricks updated) (cons updated other) other)
                                          (mapcar #'first absent))))
                              (list (cons route routes) nil))))
          (list nil trick)
          (active self)))

(defmethod second-pass ((self play-com) hands)
    (divlist (list (lambda (route) 
                     (with-slots (to to-suit-void) route
                        (not (nth (suitno to-suit-void) (suits (nth to hands)))))))
             (pending self)))

(defmethod broken-guards ((self play-com) tricks)
    (with-slots (guards) self
        (destructuring-bind (remaining broken) 
                (fold (lambda (acc val)
                        (destructuring-bind (remaining broken) acc
                            (destructuring-bind (suit ranks) val
                                (let* ((played-ranks (mapcar #'second
                                                             (filter (f* #'first (curry #'eq suit))
                                                                     tricks)))
                                       (new-ranks (list- ranks played-ranks)))
                                    (if new-ranks
                                        (list (cons (list suit new-ranks) remaining) broken)
                                        (list remaining (cons suit broken)))))))
                       
                '(nil nil)
                guards)
            (setf guards remaining)
            broken)))

(defmethod normalized-hands ((self play-com) outcome)
    (let ((offset (position-if (f* #'handid (curry #'eq (zero-id self)))
                               (remaining outcome))))
       (roll offset (remaining outcome))))

(defmethod normalize-hands ((self play-com) outcome)
    (flet ((norm-routes (offset routes)
             (loop for route in routes
                   collect (with-slots (from to to-suit-void tricks) route
                                (make-instance 'play-route :from (if from (mod (+ from offset) 4))
                                                           :to (mod (+ to offset) 4)
                                                           :to-suit-void to-suit-void
                                                           :tricks tricks)))))
        (with-slots (active pending zero-id) self
            (let ((offset (position-if (f* #'handid (curry #'eq (zero-id self)))
                                       (remaining outcome))))
                (if (> offset 0)
                    (make-instance 'play-com :zero-id (handid (first (remaining outcome)))
                                             :trump (trump self)
                                             :active (norm-routes offset active)
                                             :pending (norm-routes offset pending))
                    self)))))

(defmethod refresh-suits ((self play-com) suits hands)
    (if suits (apply #'play-routes `(,(trump self) ,@hands :suits ,suits))))
    
(defmethod after-tricks ((self play-com) outcome)
    (let ((new-self (normalize-hands self outcome))
          (raw-cards (apply #'append (tricks outcome))))
        (destructuring-bind (base-routes skipped-cards)
                            (first-pass new-self raw-cards)
            (destructuring-bind (new-active new-pending) 
                                (second-pass new-self (remaining outcome))
                 (let ((refreshed (refresh-suits new-self (broken-guards new-self skipped-cards) 
                                                 (remaining outcome))))
                   (make-instance 'play-com :zero-id (zero-id new-self)
                                  :trump (trump self)
                                  :active (append-routes (reverse base-routes) 
                                                         new-active 
                                                         (if refreshed (active refreshed)))
                                  :pending (append-routes new-pending 
                                                          (pending new-self)
                                                          (if refreshed (pending refreshed)))
                                  :guards (append (guards new-self) (if refreshed (guards refreshed)))))))))

(test (let ((a (make-instance 'hand := '((0 5 10 11 12) () (0 1 4 9 10) (0 1 6))))
            (b (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11))))
            (c (make-instance 'hand := '((1 6 7) (3 5 8 12) (3 5 7 12) (8 12))))
            (d (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5)))))
        (tricks (mk-route (play-routes 'h a b c d) a b c d)))
      '(((3 6) (3 7) (3 8) (3 2)) ((1 12) (1 1) (3 0) (1 0))
        ((1 8) (1 10) (2 0) (1 6)) ((2 10) (2 6) (2 12) (2 2))
        ((1 5) (1 7) (2 1) (1 9)) ((3 1) (3 3) (3 12) (3 4))
        ((1 3) (1 4) (2 4) (1 11)) ((0 11) (0 2) (0 6) (0 4))
        ((0 12) (0 3) (0 1) (0 8)))
      equal)

(test (let* ((a (make-instance 'hand := '((0 5 10 11 12) () (0 1 4 9 10) (0 1 6))))
             (b (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11))))
             (c (make-instance 'hand := '((1 6 7) (3 5 8 12) (3 5 7 12) (8 12))))
             (d (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5))))
             (routes (play-routes 'h a b c d))
             (trick (do-next (simple-trick 'h 's a b c d) 
                             (curry #'simple-trick 'h 's))))
          (tricks (apply #'mk-route (cons (after-tricks routes trick) (remaining trick)))))
      '(((1 12) (1 1) (3 6) (1 0)) ((1 8) (1 10) (2 0) (1 6))
        ((2 10) (2 6) (2 12) (2 2)) ((1 5) (1 7) (2 1) (1 9))
        ((0 11) (0 2) (0 6) (0 4)) ((0 12) (0 3) (0 1) (0 8)))
      equal)

;; TODO:
;;> (APPLY #'MK-ROUTE (CONS (PEEK (AFTER-TRICKS ROUTES TRICK)) (REMAINING TRICK)))
;;  = OUTCOME<((♦2 ♥2 ♦6 ♠2) (♦4 ♠3 ♦J ♥5)) => (HAND<♣ J54 ♦  ♥ 8 ♠ KQJ95>
;;                                              HAND<♣ Q983 ♦  ♥ A97 ♠ A10>
;;                                              HAND<♣ 106 ♦ 9 ♥ K104 ♠ 764>
;;                                              HAND<♣ AK72 ♦  ♥ QJ63 ♠ 8>) / (2
;; One missing trick in this test…
(test (let* ((a (make-instance 'hand := '((0 5 11 12) (3 5) (1 4 9 10) (0 1 6))))
             (b (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11))))
             (c (make-instance 'hand := '((1 6 7 10) (8 12) (0 3 5 7 12) (8 12))))
             (d (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5))))
             (routes (play-routes nil b c d a))
             (trick (do-next (simple-trick nil 'd a b c d) 
                             (curry #'simple-trick nil 'd))))
          (tricks (apply #'mk-route (cons (after-tricks routes trick) (remaining trick)))))
      '(((1 0) (2 0) (1 4) (3 0)) ((1 2) (3 1) (1 9) (2 3)))
      equal)

(defun immediate-tricks (cache trump a b c d &key (suits '(c d h s)))
    (fold (lambda (acc card)
            (best-outcome 
                acc
                (do-next (simple-trick trump (car card) a b c d :use-highest (second card))
                         (curry* 'run (a b c d) (cache trump a b c d :suits suits)))))
          (make-instance 'outcome :remaining (list a b c d))
          (loop for suit in suits
                for cards in (mapcar (curry #'hand-suit a) suits)
                append (mapcar (curry #'list suit)
                               (cond ((eq (suitno suit) (suitno trump))
                                         (if cards (let ((btrumps (hand-suit b trump))
                                                         (dtrumps (hand-suit d trump)))
                                                      (if (or btrumps dtrumps)
                                                          (if (> (length cards) 1) (list nil t) (list nil))))))
                                     (cards
                                        (if (< (length cards) 2) (list nil)
                                                                 (list nil T))))))))

(test (tricks (run (make-instance 'cache :! #'immediate-tricks) nil 
                   (make-instance 'hand := '((5 10 11 12) (8) (0 1 4 9 10) (0 1 6)))
                   (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11)))
                   (make-instance 'hand := '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12)))
                   (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5)))
                   :suits '(c)))
      '(((0 12) (0 2) (0 0) (0 4)) ((0 11) (0 3) (0 1) (0 8))
        ((0 10) (0 9) (0 6) (3 2)) ((0 5) (2 6) (0 7) (3 4)))
      equal)

(test (tricks (run (make-instance 'cache :! #'immediate-tricks) nil 
                   (make-instance 'hand := '((5 10 11 12) (8) (0 1 4 9 10) (0 1 6)))
                   (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11)))
                   (make-instance 'hand := '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12)))
                   (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5)))
                   :suits '(c h)))
      '(((0 12) (0 2) (0 0) (0 4)) ((0 11) (0 3) (0 1) (0 8))
        ((0 10) (0 9) (0 6) (3 2)) ((0 5) (2 6) (0 7) (3 4))
        ((2 12) (2 2) (2 0) (3 3)))
      equal)

(defun find-opp-weakness (trump a b c d)
    (let ((plan (make-instance 'deal-plan := (list a b c d) :trump trump)))
        (mapcar (lambda (suit)
                    (list (suitno (suit suit))
                          (- (ours suit) (theirs suit))
                          (mapcar #'second
                                (filter (f* #'first (curry #'eq (suitno (suit suit))))
                                        (loop for i from 0 to (- (length (tricks suit)) 1) by 2
                                            append (nth i (tricks suit)))))
                          (mapcar #'second
                                (filter (f* #'first (curry #'eq (suitno (suit suit))))
                                        (loop for i from 1 to (- (length (tricks suit)) 1) by 2
                                              append (nth i (tricks suit)))))))
                                
                (suits plan))))

(test (find-opp-weakness nil (make-instance 'hand := '((5 10 11 12) (8) (0 1 4 9 10) (0 1 6)))
                             (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11)))
                             (make-instance 'hand := '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12)))
                             (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5))))
      '((0 4 (12 11 10 7) NIL) (2 3 (12 9 5 1) (11)) (3 2 (8 12) NIL) (1 -3 (12) (11 6 4 10)))
      equal)

(test (find-opp-weakness nil (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11)))
                             (make-instance 'hand := '((5 10 11 12) (8) (0 1 4 9 10) (0 1 6)))
                             (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5)))
                             (make-instance 'hand := '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))))
      '((1 3 (7 10 9 2) (12)) (3 3 (9 10 7 3) (12)) (2 -3 (8) (12 10 7 1)))
      equal)

(test (let ((a (make-instance 'hand := '((0 1 2 3 10 11 12) (0) (0 1 12) (0 1))))
            (b (make-instance 'hand := '((7 8 9) (1 5 6 7 12) (8 9 10) (5 6))))
            (c (make-instance 'hand := '(nil (2 3 9 10) (2 3 4 11) (2 3 4 10 11))))
            (d (make-instance 'hand := '((4 5 6) (4 8 11) (5 6 7) (7 8 9 12)))))
         (find-opp-weakness 'C b c d a))
      '((1 3 (12 11 5 6) (10)) (3 2 (12 7 8) (11)) (2 1 (8 9) (12)))
      equal)

(defun take-highest-that (test cards)
    (let ((pos (position-if test (reverse cards))))
        (if pos (take (- (length cards) pos 1) cards)
                (take 0 cards))))

(defun hunt-card (trump card a b c d)
    (let ((suit (car card)))
        (new-outcome (cond ((position (second card) (hand-suit b suit))
                                (make-trick suit trump
                                    (beat-or-low trick a)
                                    (beat-or-low trick b :use-highest T)
                                    (beat-or-low trick c)
                                    (beat-or-low trick d)))
                           ((position (second card) (hand-suit d suit))
                                (if (beats? b c suit trump)
                                    (make-trick suit trump
                                        (beat-or-low trick a :use-highest T)
                                        (beat-or-low trick b)
                                        (beat-or-low trick c)
                                        (beat-or-low trick d))
                                    (make-trick suit trump
                                        (beat-or-low trick a)
                                        (beat-or-low trick b)
                                        (beat-or-low trick c :use-highest T)
                                        (beat-or-low trick d))))
                           (t (make-trick suit trump
                                    (beat-or-low trick a)
                                    (beat-or-low trick b)
                                    (beat-or-low trick c)
                                    (beat-or-low trick d)))))))

(test (tricks (hunt-card nil '(2 11) (make-instance 'hand := '((5 10 11 12) (8) (0 1 4 9 10) (0 1 6)))
                                     (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11)))
                                     (make-instance 'hand := '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12)))
                                     (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5)))))
      '(((2 10) (2 6) (2 3) (2 11)))
      equal)

(defclass play-strategy ()
   ((soonest :reader soonest)
    (establish :reader establish)
    (trump :reader trump)
    (routes :reader routes)))

(defmethod initialize-instance ((self play-strategy) &key trump hands soonest establish routes)
    (if (not hands)
        (progn (setf (slot-value self 'soonest) soonest)
               (setf (slot-value self 'establish) establish)
               (setf (slot-value self 'trump) trump)
               (setf (slot-value self 'routes) routes))
        (with-slots (soonest establish routes) self
          (let ((targets (apply #'find-opp-weakness (cons trump hands))))
            (setf (slot-value self 'trump) trump)
            (setf soonest (mapcar #'first (filter (andf (f* #'second (curry #'= 0))
                                                        (lambda (x) (not (third x))))
                                                  targets)))
            (setf establish (filter (f* #'second (curry #'< 0)) targets))
            (setf routes (apply #'play-routes (cons trump hands)))))))

(defmethod after-tricks ((self play-strategy) outcome)
    (with-slots (soonest establish trump routes) self
        (make-instance 'play-strategy :soonest soonest :establish establish
                                      :trump trump :routes (after-tricks routes outcome))))

(defmethod play-strategy ((strategy play-strategy) cache a b c d)
    (with-slots (soonest establish trump) strategy
        (or (let ((suits (filter (curry #'hand-suit a) soonest)))
                  (pass-if #'tricks (run cache trump a b c d :suits (list-lead 2 suits))))
            (let* ((target (car (filter (f* #'first 
                                            (lambda (suit) (and (hand-suit a suit)
                                                                (or (not (eq suit (suitno trump)))
                                                                         (hand-suit b suit)
                                                                         (hand-suit d suit))
                                                                (or (not trump) (eq (suitno trump) suit)
                                                                    (and (hand-suit b suit) 
                                                                         (hand-suit d suit))))))
                                         establish)))
                   (suit (car target)))
                (if suit 
                    (or (pass-if #'tricks (immediate-tricks cache trump a b c d :suits (cons suit soonest)))
                        (let ((rank (and (fourth target) (pop (fourth target)))))
                            (if rank (hunt-card trump (list suit rank) a b c d)
                                     (simple-trick trump suit a b c d))))))
            (make-instance 'outcome :remaining (list a b c d)))))

(test (let* ((a (make-instance 'hand := '((5 10 11 12) (8) (0 1 4 9 10) (0 1 6))))
             (b (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11))))
             (c (make-instance 'hand := '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))))
             (d (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5))))
             (cache (make-instance 'cache :! #'immediate-tricks))
             (strategy (make-instance 'play-strategy :trump nil :hands (list a b c d))))
          (tricks (do-next (play-strategy strategy cache a b c d) 
                           (curry #'play-strategy strategy cache))))
      '(((0 12) (0 2) (0 0) (0 4)) ((0 11) (0 3) (0 1) (0 8))
        ((0 10) (0 9) (0 6) (3 2)) ((0 5) (2 6) (0 7) (3 4))
        ((2 3) (2 11) (2 0) (3 3)))
      equal)

(test (let* ((a (make-instance 'hand := '((5 10 11 12) (8) (0 1 4 9 10) (0 1 6))))
             (b (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11))))
             (c (make-instance 'hand := '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))))
             (d (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5))))
             (cache (make-instance 'cache :! #'immediate-tricks))
             (as (make-instance 'play-strategy :trump nil :hands (list a b c d)))
             (bs (make-instance 'play-strategy :trump nil :hands (list b c d a))))
          (tricks (do-next (play-strategy bs cache b c d a)
                           (lambda (a b c d) (make-instance 'outcome :remaining (list a b c d)))
                           (curry #'play-strategy as cache))))
      '(((1 0) (1 12) (1 1) (1 8)) ((0 0) (0 4) (0 10) (0 2))
        ((0 12) (0 3) (0 1) (0 8)) ((0 11) (0 9) (0 6) (3 2))
        ((0 5) (2 6) (0 7) (3 4)))
      equal)

(test (let* ((a (make-instance 'hand := '((5 10 11 12) (8) (0 1 4 9 10) (0 1 6))))
             (b (make-instance 'hand := '((2 3 9) (0 6 9 11) (6) (3 7 9 10 11))))
             (c (make-instance 'hand := '((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))))
             (d (make-instance 'hand := '((4 8) (1 2 4 7 10) (2 8 11) (2 4 5))))
             (cache (make-instance 'cache :! #'immediate-tricks))
             (as (make-instance 'play-strategy :trump nil :hands (list a b c d)))
             (bs (make-instance 'play-strategy :trump nil :hands (list b c d a))))
         (let ((ans1 (do-next (play-strategy bs cache b c d a)
                              (lambda (a b c d) (make-instance 'outcome :remaining (list a b c d)))
                              (curry #'play-strategy as cache))))
            (append (tricks ans1) (tricks (apply #'play-strategy  `(,as ,cache ,@(remaining ans1)))))))
      '(((1 0) (1 12) (1 1) (1 8)) ((0 0) (0 4) (0 10) (0 2))
        ((0 12) (0 3) (0 1) (0 8)) ((0 11) (0 9) (0 6) (3 2))
        ((0 5) (2 6) (0 7) (3 4)) ((2 3) (2 11) (2 0) (3 3)))
      equal)

(defclass deal-play ()
    ((hands)(trump)(passive)
     (cache :reader cache)
     (active :reader active)
     (outcome :reader outcome)))

(defmethod initialize-instance ((this deal-play) &key hands trump cache active passive outcome)
    (setf (slot-value this 'hands) (if active hands (roll -1 hands)))
    (setf (slot-value this 'trump) trump)
    (setf (slot-value this 'outcome) outcome)
    (setf (slot-value this 'cache) (or cache (make-instance 'cache :! #'immediate-tricks)))
    (setf (slot-value this 'active) (or active (make-instance 'play-strategy :trump trump 
                                                              :hands (roll -1 hands))))
    (setf (slot-value this 'passive) (or passive (make-instance 'play-strategy 
                                                              :trump trump :hands hands))))

(defmethod after-action ((this deal-play) action)
    (with-slots (hands outcome trump cache active passive) this
        (let ((ans (apply (cond ((functionp action) action)
                                ((eq action 'play-strategy) (curry #'play-strategy active cache))
                                ((eq action 'mk-route) (curry #'mk-route (routes active)))
                                (t (curry #'simple-trick trump action)))
                          hands)))
            (if (tricks ans)
                (let ((new-a (after-tricks active ans))
                      (new-p (after-tricks passive ans)))
                     (make-instance 'deal-play
                                    :hands (remaining ans)
                                    :trump trump
                                    :outcome (if outcome (add-outcome* outcome ans) ans)
                                    :active (if (won? ans) new-a new-p)
                                    :passive (if (won? ans) new-p new-a)))))))
    
(defmethod play-outcome ((this deal-play))
    (with-slots (hands trump cache active passive outcome) this
        (let ((any-suit (position-if #'id (suits (first hands))))
              (roll (if outcome (slot-value outcome 'roll) 0)))
            (if any-suit (let ((options (loop for action in (list (curry #'play-strategy active cache)
                                                                  (curry #'mk-route (routes active))
                                                                  (curry #'simple-trick trump any-suit))
                                              collect (let ((ans (after-action this action)))
                                                         (if ans (play-outcome ans))))))
                              (apply (curry #'best-outcome* roll) options))
                         (outcome this)))))

(defun play-deal (trump declarer left dummy right)
    (play-outcome (make-instance 'deal-play :hands (list declarer left dummy right) :trump trump)))

(test (tricks (peek (play-deal nil (str2hand "♣ AKQ7 ♦ 10 ♥ QJ632 ♠ 832")
                                   (str2hand "♣ J54 ♦ KJ82 ♥ 8 ♠ KQJ95")
                                   (str2hand "♣ 9832 ♦ A75 ♥ A975 ♠ A10")
                                   (str2hand "♣ 106 ♦ Q9643 ♥ K104 ♠ 764"))))
      '(((1 0) (1 12) (1 1) (1 8)) ((0 0) (0 4) (0 10) (0 2))
        ((0 12) (0 3) (0 1) (0 8)) ((0 11) (0 9) (0 6) (3 2))
        ((0 5) (2 6) (0 7) (3 4)) ((1 3) (1 4) (3 0) (1 6))
        ((1 9) (1 5) (1 2) (3 1)) ((1 11) (2 3) (1 7) (3 6))
        ((3 3) (3 8) (3 5) (2 0)) ((2 5) (2 11) (2 1) (3 7))
        ((1 10) (2 4) (3 9) (2 7)) ((2 2) (2 9) (3 10) (2 12))
        ((3 12) (2 8) (2 10) (3 11)))
      equal)

(let ((dp (make-instance 'deal-play :hands (list (str2hand "♣ AKQ7 ♦ 10 ♥ QJ632 ♠ 832")
                                   (str2hand "♣ J54 ♦ KJ82 ♥ 8 ♠ KQJ95")
                                   (str2hand "♣ 9832 ♦ A75 ♥ A975 ♠ A10")
                                   (str2hand "♣ 106 ♦ Q9643 ♥ K104 ♠ 764")))))
   (play-outcome (fold #'after-action
         dp 
         '(play-strategy play-strategy play-strategy play-strategy))))

(defun suit-hcp (suit)
    (apply #'+ (mapcar #'rank-hcp suit)))

; Test if infering minimum values work properly
(let ((dg (make-instance 'deal :~ '((:hcp (0 11) :c (2 5) :d (2 5) :h (2 5) :s (2 5))
                                    (:hcp (0 11) :c (2 5) :d (2 5) :h (2 5) :s (2 5))
                                    (:hcp (0 11) :c (2 5) :d (2 5) :h (2 5) :s (2 5)))
                               := '(((A J 7) (A K 5 4) (6 5 4 3) (6 4))))))
    (test (filter (lambda (h)
                    (let ((hcp (apply #'+ (mapcar #'suit-hcp (suits h))))
                          (suitlens (mapcar #'length (suits h))))
                        (or (> hcp 11) 
                            (filter #'id (loop for len in suitlens
                                               collect (or (< len 2) (> len 5)))))))
                                                    
                  (loop for i from 1 to 20
                        append (cdr (build dg))))
          nil eq))
 
(defun simdeal (deal &key trump)
    (print-deal (mapcar #'suits deal) (mapcar #'handid deal))
    (let ((outcome (apply 'play-deal (cons trump deal))))
        (format t "winners: ~A~%" (car (score outcome)))
        (loop for trick in (tricks outcome)
              do (format t "~{~A ~}~%" (mapcar #'cardstr trick)))))

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
    (second (score (play-deal trump  (mkhand n) (mkhand e) (mkhand s) (mkhand w)))))

(defun best-tricks* (_ n e s w)
    (declare (ignore _))
    (let ((trump (first  (fold (lambda (acc v) (if (or (not acc) (>= (second v) (second acc))) v acc))
                               nil
                            (loop for i from 0 to 3
                                  for st in '(c d h s)
                                  collect (list st (+ (length (nth i n)) (length (nth i s)))))))))
        (let ((in-suit (second (score (play-deal trump  (mkhand n) (mkhand e) (mkhand s) (mkhand w)))))
              (non-trump (second (score (play-deal nil  (mkhand n) (mkhand e) (mkhand s) (mkhand w))))))
           (list trump in-suit non-trump))))

(defun contract-score (suit tricks &key vulnerable double level)
    (if (not level) (setf level (max (- tricks 6) 1)))
    (let ((result (- tricks level 6)))
        (if (>= result 0)
            (let* ((ppl (if (find suit '(c d)) 20 30))
                   (base (* (+ (if (not suit) 10 0)
                               (* ppl level))
                            (if double 2 1)))
                   (bonus (+ (cond ((= level 7) (if vulnerable 1500 1000))
                                   ((= level 6) (if vulnerable 750 500))
                                   ((>= base 100) (if vulnerable 500 300))
                                   (t 50))
                             (if double (+ (if vulnerable 100 50)
                                           (* (if vulnerable 200 100) result))
                                        (* ppl result)))))
               (+ base bonus))
            (if double (if vulnerable (+ -200 (* 300 (+ result 1)))
                                      (cond ((= result -1) -100)
                                            ((= result -2) -300)
                                            ((= result -3) -500)
                                            (t (+ -800 (* 300 (+ result 3))))))
                       (* result (if vulnerable 100 50))))))

(defun best-outcomes (_ n e s w)
    (declare (ignore _))
    (let* ((outcomes (loop for roll from 0 to 1
                           collect (let-from! (apply (curry #'best-tricks* nil) (roll roll (list  n e s w)))
                                              (suit suit-tricks nt-tricks)
                                      (let ((suit-score (contract-score suit suit-tricks))
                                            (nt-score (contract-score nil nt-tricks)))
                                      (if (> nt-score suit-score)
                                          (list nil nt-tricks)
                                          (list suit suit-tricks)))))))
       (apply #'append outcomes)))
                                    
(defun trump-length (trump n e s w)
    (declare (ignore e w))
    (apply #'+ (mapcar (f** (curry #'nth (suitno trump)) #'length)
                       (list n s))))

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
              do (let ((deal (mapcar #'suits (eval dealgen))))
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
                                     for sym in (cons 'hands props)
                                     collect `(,sym . (nth ,i row)))
                        exp)))
            (filter (eval `(lambda (row) ,pred)) data))))

(defmethod select ((self mc-case) &key fields filter)
    (with-slots (data props) self
        (let ((source (if filter (choose self filter)
                        data)))
            (if fields (let ((col-ids (mapcar (lambda (sym)
                                                 (position sym (cons 'hands props))) 
                                              fields)))
                           (loop for row in source
                                 collect (loop for c in col-ids
                                               collect (nth c row))))
                       (mapcar #'cdr source)))))

(defmethod show-deals ((self mc-case) filter)
    (loop for deal in (choose self filter)
      do (print-deal (car deal))))

(defmethod save ((self mc-case) filename)
    (with-open-file (out filename
                        :direction :output
                        :if-exists :supersede  
                        :if-does-not-exist :create)
        (with-slots (props dealgen trump data) self
            (let ((*print-pretty* nil))
                (loop for row in data
                      do (prin1 row out))))))

(defun load-mc-case (filename &key fallback-params sim-count)
   (with-open-file (s filename :direction :input :if-does-not-exist nil)
      (if s
        (let ((data (loop for row = (read s nil :eof)
                             until (eq row :eof)
                             collect row)))
              (apply #'make-instance `(mc-case ,@fallback-params :data ,data)))
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

(defun line-hcp (trump &rest hands)
    (declare (ignore trump))
    (apply #'+ (mapcar #'rank-hcp (flatten (append (first hands) (third hands))))))


(defun hist-plot (histogram &key (cmd "plot") params (out T))
    (flet ((rcode (funname histnode)
        (let ((sorthist (sort histnode (lambda (a b) (< (car* (car a)) (car* (car b)))))))
            (format out "hcp = c(~{~a~^,~}); freq = c(~{~,8f~^,~}); ~a(hcp, freq, type='b'~A);~%"
                (mapcar (f* #'first #'car*) sorthist)
                (mapcar (f* #'second #'float) sorthist)
                funname
                (if params (format nil ",~A" params)
                    "")))))
      (rcode cmd histogram)))

(defun best-suit (a b)
    (let ((longest (fold (lambda (acc val)
                            (if (>= (length (second val)) (length (second acc))) val acc))
                         nil
                         (loop for suit in '(c d h s)
                               for as in a
                               for bs in b
                               collect (list suit (append as bs))))))
       (car longest)))
       
