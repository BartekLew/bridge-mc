(load "~/quicklisp/setup.lisp")
(ql:quickload '(:clack :clack-cors :jonathan :flexi-streams))
(load "bridge.cl")

(defun read-body (env)
  (let ((body (getf env :raw-body)))
    (etypecase body
      (string body)
      (stream (let ((stream (flexi-streams:make-flexi-stream body :external-format :utf-8)))
                (with-output-to-string (out)
                    (loop for line = (read-line stream nil)
                          while line
                          do (write-line line out))))))))

(defun suitstr (suit)
    (if (not suit) "NT"
        (let ((suitno (if (numberp suit) suit (position suit '(C D H S)))))
            (nth suitno '("♣" "♦" "♥" "♠")))))

(defun sim-axis (hands)
    (let ((trump (apply #'best-suit (mapcar #'suits (reorder hands '(0 2))))))
        (fold (lambda (acc val)
                (let-from* val (suit roll outcome)
                   (if (or (not acc)
                           (> (second (score outcome))
                              (third acc)))
                       (list suit (handid (nth roll hands)) (second (score outcome)) outcome)
                       acc)))
              nil
              (mapcar (lambda* (suit roll)
                         (list suit roll (apply #'play-deal (cons suit (roll roll hands)))))
                      (list-prod `(nil ,trump) '(0 2))))))

(defun suitcmp (a b)
    (> (or (suitno a) 4)
       (or (suitno b) 4)))

(defun sim-deal (req)
    (let* ((hands (mapcar #'str2hand (getf req :|hands|)))
           (results (list (sim-axis hands) (sim-axis (roll 1 hands))))
           (winner (let ((t1 (seektree '(0 2) results))
                         (t2 (seektree '(1 2) results)))
                      (cond ((> t1 t2) 0)
                            ((= t1 t2) (if (suitcmp (seektree '(0 0) results)
                                                    (seektree '(1 0) results))
                                           0 1))
                            (t 1)))))
       (let-from! (nth winner results) (wsuit wdec wtricks woutcome)
          (declare (ignore wdec woutcome))
          (let-from! (second (roll winner results)) (dsuit ddec dtricks doutcome)
             (declare (ignore ddec doutcome))
             (let* ((wscore (contract-score wsuit wtricks))
                    (dscore (contract-score dsuit dtricks :level (if (suitcmp dsuit wsuit)
                                                                    (- wtricks 6)
                                                                    (- wtricks 5))
                                                          :double (>= wscore 300))))
               `(:score (,wscore ,dscore)
                 :power ,(mapcar (lambda* (suit declarer tricks outcome)
                                    `(:trump ,(suitstr suit)
                                      :declarer ,declarer
                                      :score ,tricks
                                      :tricks ,(mapcar (curry #'mapcar #'cardstr) (tricks outcome))))
                                 results)))))))
        
(defun app (env)
  "Simple API endpoint example."
  (let ((path (getf env :path-info)))
    (cond
      ((eq (getf env :request-method) :OPTIONS)
       `(200 (:content-type "text/plain")
             ("")))
      ((string= path "/api/simdeal")
       `(200 (:content-type "application/json")
             (,(jonathan:to-json (sim-deal (jonathan:parse (read-body env)))))))
      (t
       `(404 (:content-type "text/plain") ("Not found"))))))


(defun wrap-errors (app)
  (lambda (env)
    (let ((backtrace nil)
          (error-message nil))
      (restart-case
          (handler-bind
              ((error
                (lambda (e)
                  ;; Capture error message
                  (setf error-message (princ-to-string e))
                  ;; Capture backtrace if SBCL
                  (when (find-package 'sb-debug)
                    (setf backtrace
                          (with-output-to-string (s)
                            (ignore-errors
                              (funcall (find-symbol "PRINT-BACKTRACE" "SB-DEBUG")
                                       :count 30 :stream s)))))
                  ;; Abort normal execution
                  (invoke-restart 'abort))))
                ;; Call the app normally
                (funcall app env))
        ;; Abort restart clause
        (abort ()
          ;; Print error to stdout
          (format t "~&[ERROR] ~A~%" error-message)
          (when backtrace
            (format t "~&[TRACEBACK]~%~A~%" backtrace))
          (force-output)
          ;; Return 500 JSON response
          `(500 (:content-type "application/json")
                (,(jonathan:to-json
                   `(:error ,error-message)))))))))


;; Wrap app manually with CORS middleware
(defparameter *wrapped-app*
  (wrap-errors
    (clack-cors:make-cors-middleware
        #'app
        :allowed-origin "*"
        :allowed-methods "GET, POST, OPTIONS"
        :allowed-headers "Content-Type")))

;; Start server
(defparameter *server*
    (clack:clackup *wrapped-app* :server :woo :port 5000))

(defun restart-server ()
    (clack:stop *server*)
    (setf *server* (clack:clackup *wrapped-app* :server :woo :port 5000)))
