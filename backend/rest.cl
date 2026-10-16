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

(load "bridge.cl")
(load "compscore.cl")
(ql:quickload '(:clack :clack-cors :jonathan :flexi-streams))

(defclass table ()
    ((users :initform '() :reader users)
     (mode :initform 'init)
     (lock :initform nil)
     (cond :initform nil)
     (hands :initform '(nil nil nil nil))
     (auction :initform nil)))

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
                            (bt:with-lock-held (lock)
                                (setf users (cons name users))
                                (when (= (length users) 4)
                                      (setf users (second (deal 4 users))))
                                (sb-thread:condition-broadcast cond)))
                                T)))
           (if ans (sitting-resp table) 
                   '(:status rejected)))))

(defmethod setting ((table table))
    (with-slots (users lock cond) table
        (if (not lock) (setf lock (bt:make-lock)))
        (if (not cond) (setf cond (bt:make-condition-variable)))

        (bt:with-lock-held (lock)
            (if (not (= (length users) 4)) (bt:condition-wait cond lock :timeout 20))
            (sitting-resp table))))

(defmethod for-uid ((table table) user function)
    (with-slots (users) table
        (let ((uid (position user users :test #'equal)))
            (if uid (funcall function uid)
                    `(:status error
                      :message (format nil "User not logged in: ~a" user))))))

(defmethod add-hand ((table table) user hand)
    (with-slots (users hands cond lock) table
        (if (= (length hands) 4)
            (for-uid table user (lambda (uid)
                                    (setf (nth uid hands) (str2hand hand))
                                    (if (not (position nil hands))
                                        (bt:with-lock-held (lock)
                                           (sb-thread:condition-broadcast cond)))
                                    `(:status ok)))
            `(:status error
              :message "Bad state. Not all players are there."))))

(defmethod auction ((table table) user since)
    (with-slots (users auction lock cond) table
       (bt:with-lock-held (lock)
          (for-uid table user 
                   (lambda (uid)
                      (if (and (<= since (length auction))
                          (not (= (mod (length auction) 4) uid)))
                        (bt:condition-wait cond lock :timeout 20))
                      (if (= (mod (length auction) 4) uid)
                          `(:your-bid ,auction)
                          `(:others-bid ,auction)))))))

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

;; Splitting play into short sequences of tricks to make it easier to follow.

; Numbers of tricks in consecutive sequences; the last one takes the rest
(defparameter *sequence-sizes* '(5 5 3))

; Cut points (numbers of tricks played so far) dividing n tricks by sizes
(defun sequence-cuts (sizes n)
    (let ((sum 0))
        (append (loop for size in (butlast sizes)
                      while (< (+ sum size) n)
                      collect (incf sum size))
                (list n))))

(test (sequence-cuts '(5 5 3) 13) '(5 10 13) equal)
(test (sequence-cuts '(5 5 3) 7) '(5 7) equal)

; Tricks of outcome grouped into sequences of *sequence-sizes* tricks, each
; with its own summary. Winners are positions of trick winners relative to
; trick leader. Hands are in order: declarer, LHO, dummy, RHO.
(defun play-sequences (outcome suit hands)
    (loop for start = 0 then end
          for end in (sequence-cuts *sequence-sizes* (length (tricks outcome)))
          collect `(:tricks ,(mapcar (curry #'mapcar #'cardstr)
                                     (subseq (tricks outcome) start end))
                    :winners ,(subseq (winner-nos outcome) start end)
                    :comment ,(format nil "~A"
                                  (apply #'summarize-outcome
                                         (append (list outcome suit) hands
                                                 (list :start start :end end :show-trump nil)))))))

(defun sim-deal (defhands)
    (let* ((hands (if (= (length defhands) 4)
                      defhands (append defhands
                                       (list (make-instance 'hand :remaining defhands)))))
                                        
           (results (list (sim-axis hands) (sim-axis (roll 1 hands)))))
       `(:score ,(deal-score (subseq (first results) 0 3) (subseq (second results) 0 3))
         :power ,(mapcar (lambda* (roll suit declarer tricks outcome)
                            `(:trump ,(suitstr suit)
                              :declarer ,declarer
                              :score ,tricks
                              :sequences ,(play-sequences outcome suit (roll roll hands))))
                         (mapcar #'cons (list 0 1) results)))))

; Scores of the side winning the auction and of the other one competing
; over it, given (suit declarer tricks) of both sides' contracts
(defun deal-score (a b)
    (let ((winner (cond ((> (third a) (third b)) 0)
                        ((= (third a) (third b)) (if (suitcmp (first a) (first b)) 0 1))
                        (t 1))))
       (let-from! (nth winner (list a b)) (wsuit wdec wtricks)
          (declare (ignore wdec))
          (let-from! (nth (- 1 winner) (list a b)) (dsuit ddec dtricks)
             (declare (ignore ddec))
             (let ((wscore (contract-score wsuit wtricks)))
                (list wscore
                      (contract-score dsuit dtricks :level (if (suitcmp dsuit wsuit)
                                                               (- wtricks 6)
                                                               (- wtricks 5))
                                                    :double (>= wscore 300))))))))

(defparameter *sims-dir* (merge-pathnames "sims/" (uiop:getcwd)))

(defun sim-path (id)
    (merge-pathnames (format nil "~A.json" id) *sims-dir*))

(defun valid-sim-id? (id)
    (and (> (length id) 0)
         (every #'alphanumericp id)))

(defun new-sim-id ()
    (format nil "~(~36R~36R~)" (get-universal-time) (random (expt 36 3))))

(defun write-sim (id sim)
    (ensure-directories-exist *sims-dir*)
    (with-open-file (out (sim-path id) :direction :output :if-exists :supersede
                                       :external-format :utf-8)
        (write-string (jonathan:to-json sim) out)))

(defun save-sim (hands result)
    (let ((id (new-sim-id)))
        (write-sim id `(:hands ,hands :result ,result))
        id))

(defun load-sim (id)
    (if (valid-sim-id? id)
        (with-open-file (in (sim-path id) :if-does-not-exist nil :external-format :utf-8)
            (if in (let ((str (make-string (file-length in))))
                      (subseq str 0 (read-sequence str in)))))))

(defun sim-deal-saved (hands raw-hands)
    (let ((result (sim-deal hands)))
        `(:id ,(save-sim raw-hands result) ,@result)))

;; Editing play of saved simulations: tricks are removed from the end of
;; play or added there. Sequences cut or made by the user have no comment;
;; edited axes are marked EDITED.

(define-condition edit-error (simple-error) ())

(defun edit-error (fmt &rest args)
    (error 'edit-error :format-control fmt :format-arguments args))

(defparameter *seats* '("N" "E" "S" "W"))

(defun str2card (str)
    (and (stringp str) (> (length str) 1)
         (let ((suit (str2suitno (subseq str 0 1)))
               (rank (position (subseq str 1) '("2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A")
                               :test #'equal)))
            (and suit rank (list suit rank)))))

; Hands of the deal by seat (N E S W) from hands of saved sim
(defun sim-hands (formulas)
    (let ((hands (mapcar #'str2hand formulas)))
        (mapcar #'suits (if (= (length hands) 4) hands
                            (append hands (list (make-instance 'hand :remaining hands)))))))

; Computed hand has no proper id; it's always the last one (W)
(defun declarer-seat (axis)
    (or (position (getf axis :|DECLARER|) *seats* :test #'equal) 3))

(defun axis-tricks (axis)
    (loop for seq in (getf axis :|SEQUENCES|) append (getf seq :|TRICKS|)))

; Hands left by seat, seat to lead and tricks taken by declarer's side after
; tricks of axis are played
(defun play-state (hands axis)
    (let* ((trump (str2suitno (getf axis :|TRUMP|)))
           (declarer (declarer-seat axis))
           (leader (mod (+ declarer 1) 4))
           (taken 0))
        (loop for trick in (axis-tricks axis)
              for cards = (mapcar #'str2card trick)
              for winner = (mod (+ leader (trick-winner cards trump)) 4)
              do (setf hands (play-trick hands leader cards))
                 (setf leader winner)
                 (if (evenp (- winner declarer)) (incf taken)))
        (values hands leader taken)))

(defun play-done? (hands axis)
    (every #'null (apply #'append (play-state hands axis))))

(defun plain-seq (tricks winners)
    `(:|TRICKS| ,tricks :|WINNERS| ,winners))

; Sequences without the last trick; empty ones at the end are dropped
(defun pop-trick (seqs)
    (let ((seqs (loop with ans = (reverse seqs)
                      while (and ans (not (getf (car ans) :|TRICKS|)))
                      do (pop ans)
                      finally (return (reverse ans)))))
        (if (not seqs) (edit-error "No tricks to remove"))
        (let* ((last-seq (car (last seqs)))
               (tricks (butlast (getf last-seq :|TRICKS|))))
            (append (butlast seqs)
                    (if tricks (list (plain-seq tricks (butlast (getf last-seq :|WINNERS|)))))))))

(defun push-trick (seqs trick winner)
    (let ((last-seq (car (last seqs))))
        (if last-seq
            (append (butlast seqs)
                    (list (plain-seq (append (getf last-seq :|TRICKS|) (list trick))
                                     (append (getf last-seq :|WINNERS|) (list winner)))))
            (list (plain-seq (list trick) (list winner))))))

(defun split-seqs (seqs)
    (if (or (not seqs) (not (getf (car (last seqs)) :|TRICKS|)))
        (edit-error "Next tricks go to a new sequence already"))
    (append seqs (list (plain-seq nil nil))))

; (suit declarer tricks) of axis as deal-score takes it
(defun axis-contract (axis)
    (let ((suit (str2suitno (getf axis :|TRUMP|))))
        (list (and suit (nth suit '(c d h s)))
              (getf axis :|DECLARER|)
              (getf axis :|SCORE|))))

; Applies action (pop, clear, split, play or trick) to play of axis axis-no
; of saved sim id. Returns (:result ...) with the updated result, which gets
; saved, or (:choices ...) of tricks to choose from, sim left intact. Play
; adds a trick taken by card, trick adds the explicit one.
(defun sim-edit (id axis-no action card trick)
    (let* ((sim (jonathan:parse (or (load-sim id) (edit-error "No simulation ~A" id))))
           (result (getf sim :|RESULT|))
           (power (getf result :|POWER|))
           (axis (if (and (integerp axis-no) (< -1 axis-no (length power)))
                     (nth axis-no power)
                     (edit-error "Bad axis ~A" axis-no)))
           (hands (sim-hands (getf sim :|HANDS|)))
           (trump (str2suitno (getf axis :|TRUMP|)))
           (seqs (getf axis :|SEQUENCES|)))
        (multiple-value-bind (left leader) (play-state hands axis)
            (labels ((check-open ()
                        (if (every #'null (apply #'append left))
                            (edit-error "All cards are played")))
                     (push-cards (cards)
                        (push-trick seqs (mapcar #'cardstr cards) (trick-winner cards trump))))
                (let ((new-seqs
                        (cond ((equal action "pop") (pop-trick seqs))
                              ((equal action "clear") nil)
                              ((equal action "split") (check-open) (split-seqs seqs))
                              ((equal action "trick")
                                 (check-open)
                                 (let ((cards (mapcar #'str2card trick)))
                                    (if (not (legal-trick? left leader cards))
                                        (edit-error "Trick ~A can't be played now" trick))
                                    (push-cards cards)))
                              ((equal action "play")
                                 (check-open)
                                 (let* ((c (or (str2card card) (edit-error "Bad card ~A" card)))
                                        (seat (or (position-if (lambda (h) (member (second c) (nth (first c) h)))
                                                               left)
                                                  (edit-error "Card ~A is played already" card))))
                                    (multiple-value-bind (tricks takes) (card-tricks left leader trump seat c)
                                       (cond ((not tricks)
                                                (edit-error "Card ~A can't be played to this trick" card))
                                             ((and takes (= (length tricks) 1))
                                                (push-cards (first tricks)))
                                             (t (return-from sim-edit
                                                   `(:choices ,(mapcar (lambda (tr)
                                                                          `(:trick ,(mapcar #'cardstr tr)
                                                                            :winner ,(trick-winner tr trump)))
                                                                       tricks)
                                                     :takes ,(if takes t :false)
                                                     :leader ,(nth leader *seats*))))))))
                              (t (edit-error "Unknown action ~A" action)))))
                    (setf (getf axis :|SEQUENCES|) new-seqs)
                    (setf (getf axis :|EDITED|) t)
                    (setf (getf axis :|SCORE|) (nth-value 2 (play-state hands axis)))
                    (setf (nth axis-no power) axis)
                    (if (every (curry #'play-done? hands) power)
                        (setf (getf result :|SCORE|) (apply #'deal-score (mapcar #'axis-contract power))))
                    (write-sim id sim)
                    `(:result ,result))))))

(defclass result ()
    ((success :reader ok?)
     (val :reader val)))
 
(defmethod initialize-instance ((this result) &key ok error)
    (with-slots (success val) this
        (setf success (if error nil T))
        (setf val (or error ok))))

(defmethod match ((this result) &key ok error)
    (with-slots (success val) this
        (cond (success (if (functionp ok) (funcall ok val) val))
              (t (if (functionp error) (funcall error val) val)))))

(defmethod list-merger (a b)
    (make-instance 'result :ok (append a (list b))))

(defmethod combine ((this result) (other result) &optional (merger #'list-merger))
    (match this :ok (lambda (val)
                        (match other :ok (curry #'funcall merger val)
                                     :error other))
                :error (lambda (val)
                          (match other :ok this
                                       :error (curry #'format nil "~A~%~A" val)))))

(defun simple-endpoint (env fields function &key parse-args)
    (let* ((request (jonathan:parse (read-body env)))
           (raw-args (loop for field in fields
                           collect (getf request field)))
           (args (if parse-args (fold (curry #'combine)
                                      (make-instance 'result)
                                      (loop for i from 0 below (length raw-args)
                                            for base in raw-args
                                            collect (if (nth i parse-args)
                                                        (funcall (nth i parse-args) base)
                                                        (make-instance 'result :ok base))))
                                (make-instance 'result :ok raw-args))))
      (match args :error (lambda (error)
                            `(400 (:content-type "application/json")
                                  (,(jonathan:to-json `(:error ,error)))))
                  :ok (lambda (args)
                            `(200 (:content-type "application/json")
                                  (,(jonathan:to-json (apply function args))))))))
       
(defun string-validator (&key length)
    (lambda (x)
        (if (or (not (stringp x))
                (and length (< (length x) length)))
           (make-instance 'result :error (format nil "Bad user name: ~S" x))
           (make-instance 'result :ok x))))

(defun hand-parser (handstr)
    (if (stringp handstr)
        (make-instance 'result :ok (str2hand handstr))
        (make-instance 'result :error (format nil "Wrong hand format: ~A" handstr))))

(defvar *user-validator* (string-validator :length 4))

(defun close-list (lst base)
    (if (< (length lst) (length base))
        (append lst (subseq base (length lst)))
        lst))

(defclass jobs ()
    ((cases) (last-id) (lock)))

(defmethod initialize-instance ((this jobs) &key)
    (with-slots (cases last-id lock) this
        (setf cases (make-hash-table))
        (setf last-id -1)
        (setf lock (bt:make-lock))))

(defmethod add-job ((this jobs) mcc)
    (with-slots (cases last-id lock) this
        (bt:with-lock-held (lock)
            (incf last-id)
            (setf (gethash last-id cases) mcc)
            (list last-id mcc))))
     
(defmethod get-case ((this jobs) id)
    (with-slots (cases) this
        (gethash id cases)))

(defmethod cancel ((this jobs))
    (with-slots (cases) this
        (loop for x being the hash-values of cases
              do (loop for thread in (threads x)
                       do (bt:destroy-thread thread)))))

(defmethod syms ((this jobs))
    (loop for key being the hash-keys of (slot-value this 'cases)  collect key))

(defparameter *jobs* (make-instance 'jobs))

(defun normdef (defs)
    (loop for def in defs
          collect (cond ((numberp def) `(,def ,def))
                        ((and (listp def)
                              (numberp (third def)))
                            `(,(first def) ,(second def) (,(third def) ,(third def))))
                        (t def))))

(defun invert-id (ids)
    (mapcar #'first (sort (zip-id ids) (lambda (a b) (< (second a) (second b))))))

(test (invert-id '(2 0 1 3)) '(1 2 0 3) equal)
(test (invert-id '(3 2 0 1)) '(2 3 1 0) equal)

(defun mc-deal (defs measures)
    (let-from! (mapcar #'reverse (divlist (list (f* #'second #'type-of (curry #'eq 'hand)))
                                          (zip-id defs)))
               (consts ranged)
       (let* ((d (make-instance 'deal := (mapcar (f* #'second #'pretty-tree) consts)
                                      :~ (mapcar (f* #'second #'normdef) ranged)))
              (reorder (close-list (invert-id (mapcar #'first (append consts ranged))) '(0 1 2 3))))
         (let-from! (add-job *jobs* (make-instance 'mc-case :! `(reorder (build ,d) ',reorder)
                                                            := (or measures '(best-outcomes))))
                    (id mc)
           (sim mc 500)
           (sleep 5)
           `(:status in-progress :job-id ,id
             :data ,(histogram (mapcar (curry #'apply #'append) (select mc))))))))

(defun hand-or-def-parser (defs)
    (fold #'combine
          (make-instance 'result)
          (mapcar (lambda* (type val)
                     (make-instance 'result :ok (cond ((eq type :|hand|) (str2hand val))
                                                      (t (read-from-string val)))))
                  defs)))

(defun measure-parser (names)
    (if (equal names '("")) (make-instance 'result :ok '(best-outcomes))
        (fold #'combine
            (make-instance 'result)
            (mapcar (lambda (name)
                        (let ((contract (make-instance 'contract := name)))
                            (make-instance 'result :ok
                                (eval `(defun ,(read-from-string (format nil "msr-~A" name)) (_ &rest hands)
                                    (declare (ignore _))
                                    (let ((tricks (apply #'best-tricks 
                                                        (cons ',(suit contract)
                                                            (roll ,(- (position (declarer contract)
                                                                                '(N E S W)))
                                                                    hands)))))
                                        (let ((result (with-score ,contract tricks)))
                                            (list tricks (base-score result)))))))))
                    names))))

(defun measure-sym-parser (names)
    (make-instance 'result :ok (filter (lambda (sym)
                                            (and sym (ignore-errors (eval `(function ,sym)))))
                                       (mapcar (f* (curry #'format nil "msr-~A") #'read-from-string)
                                               names))))

(defun substr= (ref str)
    (and (>= (length str) (length ref))
         (string= ref (subseq str 0 (length ref)))))

(defun json-response (retcode response)
    `(,retcode (:content-type "application-json")
      (,(jonathan:to-json response))))

(defun app (env)
  (let ((path (getf env :path-info)))
    (cond
      ((eq (getf env :request-method) :OPTIONS)
       `(200 (:content-type "text/plain")
             ("")))
      ((string= path "/api/sit")
          (simple-endpoint env '(:|user|) (curry #'sit *table*)
                           :parse-args (list *user-validator*)))
      ((string= path "/api/setting")
       `(200 (:content-type "application/json")
        (,(jonathan:to-json (setting *table*)))))
      ((string= path "/api/add-hand")
          (simple-endpoint env '(:|user| :|hand|) (curry #'add-hand *table*)
                               :parse-args (list *user-validator*)))
      ((string= path "/api/auction")
          (simple-endpoint env '(:|user| :|since|) (curry #'auction *table*)
                               :parse-args (list *user-validator*)))
      ((string= path "/api/simdeal")
          (simple-endpoint env '(:|hands| :|hands|) #'sim-deal-saved
                               :parse-args (list (f* (curry #'mapcar #'hand-parser)
                                                     (curry #'fold #'combine (make-instance 'result)))
                                                 nil)))
      ((string= path "/api/simedit")
       (let ((request (jonathan:parse (read-body env))))
          (handler-case (json-response 200 (sim-edit (getf request :|id|) (getf request :|axis|)
                                                     (getf request :|action|) (getf request :|card|)
                                                     (getf request :|trick|)))
            (edit-error (e) (json-response 400 `(:error ,(princ-to-string e)))))))
      ((substr= "/api/simdeal/" path)
       (let ((saved (load-sim (subseq path (length "/api/simdeal/")))))
          (if saved `(200 (:content-type "application/json") (,saved))
                    (json-response 404 `(:status :not-found)))))
      ((string= path "/api/mc")
          (simple-endpoint env '(:|defs| :|measures|) #'mc-deal'
                               :parse-args (list #'hand-or-def-parser #'measure-parser)))
      ((substr= "/api/mc/" path)
       (let ((job-id (read-from-string (subseq path (length "/api/mc/")))))
          (let ((job (get-case *jobs* job-id)))
             (if job (simple-endpoint env '(:|measures|)
                        (lambda (measures)
                            `(:status ,(if (threads job) :in-progress :success)
                              :data ,(histogram (mapcar (curry #'apply #'append)
                                                    (if measures (select job :fields measures)
                                                                 (select job))))))
                        :parse-args (list #'measure-sym-parser))
                     (json-response 400 `(:status :not-found))))))
              
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
        'app
        :allowed-origin "*"
        :allowed-methods "GET, POST, OPTIONS"
        :allowed-headers "Content-Type")))

;; Start server
(defparameter *server*
    (clack:clackup *wrapped-app* :server :hunchentoot :port 5000 :max-connections 20))

(defun restart-server ()
    (clack:stop *server*)
    (setf *server* (clack:clackup *wrapped-app* :server :hunchentoot :port 5000 :max-connections 20)))
