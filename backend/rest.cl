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

(load "~/quicklisp/setup.lisp")
(ql:quickload '(:clack :clack-cors :jonathan :flexi-streams))
(load "bridge.cl")

(defclass table ()
    ((users :initform '() :reader users)
     (lock :initform nil)
     (cond :initform nil)
     (hands :initform '(nil nil nil nil))))

(defvar *table* (make-instance 'table))

(defmethod sitting-resp ((table table))
    (with-slots (users) table
        (if (= (length users) 4) `(:status ok
                                   :ready ,users)
                                 `(:status ok
                                   :waiting ,users))))

(defmethod sit ((table table) name)
    (with-slots (users lock cond) table
        (if (not lock) (setf lock (bt:make-lock)))
        (if (not cond) (setf cond (bt:make-condition-variable)))

        (let ((ans (or (find name users :test #'equal)
                       (and (< (length users) 4)
                            (setf users (cons name users))
                            (or (when (= (length users) 4)
                                      (setf users (second (deal 4 users)))
                                      (bt:with-lock-held (lock)
                                        (sb-thread:condition-broadcast cond)))
                                 T)))))
           (if ans (sitting-resp table) 
                   '(:status rejected)))))

(defmethod setting ((table table))
    (with-slots (users lock cond) table
        (if (not lock) (setf lock (bt:make-lock)))
        (if (not cond) (setf cond (bt:make-condition-variable)))

        (bt:with-lock-held (lock)
            (if (not (= (length users) 4)) (bt:condition-wait cond lock :timeout 20))
            (sitting-resp table))))

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

(defun simple-endpoint (env fields function &key validator)
    (let* ((request (peek (jonathan:parse (read-body env))))
           (args (loop for field in fields
                       collect (getf request field)))
           (error (or (not validator) (apply validator args))))
      (if error `(400 (:content-type "application/json")
                      (,(jonathan:to-json `(:error ,error))))
                `(200 (:content-type "application/json")
                      (,(jonathan:to-json (apply function args)))))))
       
(defun string-validator (&key length)
    (lambda (x)
        (if (or (not (stringp x))
                (and length (< (length x) 4)))
           (format nil "Bad user name: ~S" x))))

(defun app (env)
  "Simple API endpoint example."
  (let ((path (getf env :path-info)))
    (cond
      ((eq (getf env :request-method) :OPTIONS)
       `(200 (:content-type "text/plain")
             ("")))
      ((string= path "/api/sit")
          (simple-endpoint env '(:|user|) (curry #'sit *table*)
                           :validator (string-validator :length 4)))
      ((string= path "/api/setting")
       `(200 (:content-type "application/json")
        (,(jonathan:to-json (setting *table*)))))
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
    (clack:clackup *wrapped-app* :server :hunchentoot :port 5000 :max-connections 20))

(defun restart-server ()
    (clack:stop *server*)
    (setf *server* (clack:clackup *wrapped-app* :server :hunchentoot :port 5000 :max-connections 20)))
