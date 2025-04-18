(defmacro peek (body)
    `(progn (format t "PEEK ~A~%" ',body)
            (let ((ans ,body))
                (format t ">~A~%" ans)
                ans)))

(defmacro let-from (lst syms &body body)
    `(let (,@(loop for sym in syms
                   for i from 0
                   collect (list sym `(nth ,i ,lst))))
        ,@body))

(defmacro let-from! (form syms &body body)
    `(let ((lst (eval ',form)))
        (let (,@(loop for sym in syms
                   for i from 0
                   collect (list sym `(nth ,i lst))))
            ,@body)))

(defclass Deal-Progress ()
  ((have :initarg := :accessor have)
   (left :initarg :/ :accessor left)
   (dropped :initform nil :accessor dropped)))

(defmethod print-object ((this Deal-Progress) out)
    (format out "DealProgress<~A/~A/~A>" (have this) (left this) (dropped this)))

(defun cartesian (a b)
    (apply 'append (loop for x in a
                         collect (loop for y in b
                                       collect (list x y)))))

(defvar *suits* '(c d h s))
(defvar *ranks* '(2 3 4 5 6 7 8 9 10 j q k a))

(defun start-deal ()
    (make-instance 'Deal-Progress := '() :/ (cartesian *suits* *ranks*)))
    
(define-condition wrong-seed (error)
    ((n :initarg := :reader n)
     (lim :initarg :/ :reader lim)))

(defmethod deal ((this Deal-Progress) seed)
    (let ((optc (length (left this))))
        (if (>= seed optc)
            (error 'wrong-seed := seed :/ optc))
            
        (let ((before (subseq (left this) 0 seed))
              (after (subseq (left this) (min optc (+ seed 1)) optc))
              (choice (nth seed (left this))))
            (with-slots (dropped left have) this
                (setf dropped (append dropped before))
                (setf left after)
                (setf have (reverse (cons choice have)))
                choice))))

(defmethod finish-hand ((this Deal-Progress))
    (with-slots (dropped left have) this
        (let ((ans have))
            (setf left (append dropped left))
            (setf have nil)
            (setf dropped nil)
            ans)))

(let ((cards (start-deal)))
    (loop for i from 1 to 4
          collect (progn (loop for j from 12 downto 0
                               do (deal cards (random (peek (- (length (left cards)) j)))))
                         (finish-hand cards))))

(defclass Range ()
    ((low :initarg := :reader low)
     (high :initarg :< :reader high)))

(defmethod print-object ((this Range) out)
    (with-slots (low high) this
        (format out "[~a..~a]" low high)))

(defmethod args ((this Range))
    (let ((x (low this)))
        (lambda ()
            (if (< x (high this))
                (let ((ans x))
                    (setf x (+ x 1))
                    ans)))))

(defmethod while ((this Range) f)
    (let ((iter (args this)))
        (labels ((self ()
                    (let ((x (funcall iter)))
                        (if x (progn (funcall f x)
                                     (self))))))
            (self))))
                                     
(while (make-instance 'Range := 0 :< 10) (lambda (x) (format t "~a~%" x)))
        
(defclass Goal ()
    ((job :initarg :!)
     (domain :initarg :=)))

(defmethod print-object ((this Goal) out)
    (with-slots (job domain) this
        (format out "Goal<~A <- ~A>" job domain)))

(defmethod while ((this Goal) f)
    (with-slots (job domain) this
        (labels ((dolevel (feed job)
            (let ((hd (first feed))
                  (tl (rest feed)))
                (while (eval (second hd))
                       (lambda (val)
                           (if tl (dolevel (subst val (first hd) tl)
                                           (subst val (first hd) job))
                               (funcall f (eval (subst val (first hd) job)))))))))
            (dolevel domain job))))

(defun println (x) 
    (format t "~a~%" x))

(while (make-instance 'Goal
            :! '(list a b c)
            := '((a (make-instance 'Range := 0 :< 50))
                 (b (make-instance 'Range := 0 :< (- 52 a)))
                 (c (make-instance 'Range := 0 :< (- 52 b a)))))
       #'println)
