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

(defun sim-deal (req)
    (let* ((hands (mapcar #'str2hand (getf req :|hands|)))
           (trump (or (getf req :trump) 
                      (apply #'best-suit (mapcar #'suits (reorder hands '(0 2))))))
           (outcome (apply #'play-deal (cons trump hands))))
      `(:trump ,trump
        :score ,(second (score outcome))
        :tricks ,(mapcar (curry #'mapcar #'cardstr) (tricks outcome)))))
        
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
