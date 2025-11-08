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

(defun sim-deal (req)
    (let* ((hands (mapcar #'str2hand (getf req :|hands|))))
        (mapcar (lambda* (suit declarer outcome)
                    `(:trump ,(suitstr suit)
                      :declarer ,declarer
                      :score ,(second (score outcome))
                      :tricks ,(mapcar (curry #'mapcar #'cardstr) (tricks outcome))))
                (loop for axis from 0 to 1
                      collect (let ((trump (apply #'best-suit
                                                  (mapcar #'suits (reorder hands 
                                                                           (mapcar (curry #'+ axis) 
                                                                                   '(0 2)))))))
                                (fold (lambda (acc val)
                                        (let-from* val (suit roll outcome)
                                            (if (or (not acc) val
                                                    (> (second (score outcome))
                                                       (second (score (third acc)))))
                                                (list suit (handid (nth (+ roll axis) hands)) outcome)
                                                acc)))
                                        nil
                                        (mapcar (lambda* (suit roll)
                                                    (list suit roll (apply #'play-deal 
                                                                           (cons suit (roll (+ axis roll) 
                                                                                      hands)))))
                                                (list-prod `(nil ,trump) '(0 2)))))))))
        
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
    (handler-case
        (funcall app env)
      (error (e)
        (format t "~&[ERROR] ~A~%" e)
        (when (find-package 'sb-debug)
          (format t "~&[TRACEBACK]~%")
          ;; print 20 frames; you can adjust number
          (ignore-errors (sb-debug:print-backtrace
                                :count 20
                                :stream *standard-output*)))
        (force-output)
        `(500 (:content-type "application/json")
              (,(jonathan:to-json
                 `(:error ,(princ-to-string e)))))))))

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
