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
            (error 'wrong-seed := seed optc))
            
        (let ((before (subseq (left this) 0 seed))
              (after (subseq (left this) (min optc (+ seed 1)) optc))
              (choice (nth seed (left this))))
            (with-slots (dropped left have) this
                (setf dropped (append dropped before))
                (setf left after)
                (setf have (reverse (cons choice have)))
                choice))))

(let ((x (start-deal)))
    (deal x 2)
    (deal x 7)
    x)
