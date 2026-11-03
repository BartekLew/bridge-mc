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

; (load "bridge.cl") (load "bidding.cl")

; Module: grading.cl — graded bidding (exploratory, beside bidding.cl)
; A bid's rule is a grade: a real number, any scale, computed from the
; hand's measures. Every bid of a scheme is graded and the highest wins;
; pass is a bid like the others. A bid's meaning isn't deduced from its
; rule but measured: deal many hands, see which hands choose the bid.
;
; Grade expressions:
; - a number, or a measure: C D H S (lengths), Cpower .. Spower (suit HCP),
;   Cstop .. Sstop, hcp, losers, balanced (1 or 0)
; - (=~ x model blur): 1 when x is within model, a number or a range (lo hi)
;   with nil for no bound, and blur less for each point outside it, floored
;   at 0. (=~ x center bias blur) is the model (center-bias center+bias).
;   bias and blur are a number or (below above), so the two sides can differ;
;   a negative blur above rewards x beyond the model.
; - (and ...) the mean, (or ...) the maximum, (* ...) the product (a 0 vetoes),
;   (not x) 1 - x
; - comparisons (< <= > >= =) are 1 or 0, + - / are arithmetic

;; Measures of a hand by name, as grade expressions use them
(defun hand-measures (hand)
    (destructuring-bind (lens power losers stops) (assess-hand hand)
        (append (mapcar #'cons '(C D H S) lens)
                (mapcar #'cons '(Cpower Dpower Hpower Spower) power)
                (mapcar #'cons '(Cstop Dstop Hstop Sstop) stops)
                `((hcp . ,(apply #'+ power))
                  (losers . ,losers)
                  (balanced . ,(if (balanced? lens power losers stops) 1 0))))))

;; x against the model lo..hi (nil: no bound), losing blur-below for each
;; point below lo and blur-above for each point above hi, floored at 0
(defun near (x lo hi blur-below blur-above)
    (cond ((and lo (< x lo)) (max 0 (- 1 (* blur-below (- lo x)))))
          ((and hi (> x hi)) (max 0 (- 1 (* blur-above (- x hi)))))
          (t 1)))

(defun grade (rule measures)
    (labels ((g (e)
               (cond ((numberp e) e)
                     ((eq e t) 1)
                     ((null e) 0)
                     ((symbolp e) (let ((m (assoc e measures)))
                                      (if m (cdr m) (error "Unknown measure ~A" e))))
                     (t (let ((args (cdr e)))
                            (case (car e)
                              (and (/ (reduce #'+ (mapcar #'g args)) (length args)))
                              (or (reduce #'max (mapcar #'g args)))
                              (* (reduce #'* (mapcar #'g args)))
                              (not (- 1 (g (first args))))
                              ((+ - /) (apply (car e) (mapcar #'g args)))
                              ((< <= > >= =) (if (apply (car e) (mapcar #'g args)) 1 0))
                              ; (=~ x model blur) or (=~ x center bias blur)
                              (=~ (destructuring-bind (x model a &optional (b nil four)) args
                                      (destructuring-bind (lo hi)
                                              (if four
                                                  (destructuring-bind (below above) (def-range a)
                                                      (list (and below (- model below))
                                                            (and above (+ model above))))
                                                  (def-range model))
                                          (destructuring-bind (blur-below blur-above)
                                                  (def-range (if four b a))
                                              (near (g x) lo hi blur-below blur-above)))))
                              (otherwise (error "Unknown grade operator ~A" (car e)))))))))
        (g rule)))

; HCP =~ 16 <4, 0.3>: 1 on 12-20, 0.7 for 11 and 21, 0 from 8 down
(test (loop for hcp in '(12 16 20 11 21 10 8 3)
            collect (grade '(=~ hcp 16 4 0.3) `((hcp . ,hcp))))
      '(1 1 1 0.7 0.7 0.4 0 0)
      (lambda (a b) (every (lambda (x y) (< (abs (- x y)) 1e-6)) a b)))

; Hearts for 1♥: 3 is no way, 4 half, 5 and 6 fine, 7+ a little less
(test (loop for h from 3 to 8
            collect (grade '(=~ H (5 6) (1/2 1/10)) `((H . ,h))))
      '(0 1/2 1 1 9/10 8/10)
      (lambda (a b) (every (lambda (x y) (< (abs (- x y)) 1e-6)) a b)))

; A negative blur above rewards length beyond the model
(test (grade '(=~ H 5 (0 nil) (1/2 -3/10)) '((H . 7))) 1 eql)
(test (grade '(=~ H (5 nil) (1/2 -3/10)) '((H . 7))) 1 eql)
(test (grade '(=~ H 5 (1/2 -3/10)) '((H . 7))) 8/5 eql)

; and: mean, or: maximum, *: product, not, comparisons
(test (let ((m '((H . 4) (S . 5) (hcp . 12))))
          (list (grade '(and (=~ H 5 1/2) (=~ S 5 1)) m)
                (grade '(or (=~ H 5 1/2) (=~ S 5 1)) m)
                (grade '(* (=~ H 5 1/2) (=~ hcp (15 17) 1)) m)
                (grade '(not (> S H)) m)
                (grade '(/ (+ H S) 3) m)))
      '(3/4 1 0 0 3)
      equal)

;; Tracing a grade: the rule printed with its value after each parenthesis
;; that is evaluated, (=~ hcp (12 nil) 1)=1. The model and blurs of =~ are
;; data, so they are left alone.
(defstruct (traced (:constructor traced (form value))) form value)

(defun grade-str (x)
    (if (integerp x)
        (princ-to-string x)
        (string-right-trim "." (string-right-trim "0" (format nil "~,2F" x)))))

(defmethod print-object ((x traced) stream)
    (write (traced-form x) :stream stream)
    (format stream "=~A" (grade-str (traced-value x))))

(defun trace-rule (rule measures)
    (if (consp rule)
        (traced (cons (car rule)
                      (if (eq (car rule) '=~)
                          (cons (trace-rule (second rule) measures) (cddr rule))
                          (mapcar (lambda (e) (trace-rule e measures)) (cdr rule))))
                (grade rule measures))
        rule))

(defun grade-trace (rule measures)
    (let ((*print-pretty* t) (*print-right-margin* 70) (*print-case* :downcase))
        (prin1-to-string (trace-rule rule measures))))

(test (grade-trace '(* (=~ hcp (12 nil) 1) (or (=~ H 5 1/2) (> S H)))
                   '((hcp . 11) (H . 4) (S . 5)))
      "(* (=~ hcp (12 nil) 1)=0 (or (=~ h 5 1/2)=0.5 (> s h)=1)=1)=0"
      equal)

;; Grades of each bid of a scheme, (bid . rule), and the best of them: the
;; highest, the earlier one on a tie. A nil bid is a pass.
(defun graded-bids (measures scheme)
    (loop for (bid . rule) in scheme
          collect (cons bid (grade rule measures))))

(defun best-graded (measures scheme)
    (car (reduce (lambda (best x) (if (> (cdr x) (cdr best)) x best))
                 (graded-bids measures scheme))))

;; Today's openings (bidding.cl), translated: each bound a =~ with blur 1, so
;; a point outside gives 0, and and-ed by product. They choose as choose-bid
;; does: a fitting bid grades 1, the others 0, pass 1/2. The graded openings
;; in use, with blurs, are in grade-bidding.cfg (graded-scheme).
(defparameter graded-balanced
    '((=~ C (2 5) 1) (=~ D (2 5) 1) (=~ H (2 4) 1) (=~ S (2 4) 1)))

(defparameter crisp-openings
    `(((2 C) . (or (=~ hcp (23 nil) 1)
                   (* (not balanced)
                      (or (* (or (=~ H (5 nil) 1) (=~ S (5 nil) 1))
                             (=~ losers (nil 4) 1))
                          (* (=~ H (nil 4) 1) (=~ S (nil 4) 1)
                             (or (=~ C (5 nil) 1) (=~ D (5 nil) 1))
                             (=~ losers (nil 3) 1))))))
      ((2 NT) . (* ,@graded-balanced (=~ hcp (19 22) 1)))
      ((1 NT) . (* ,@graded-balanced (=~ hcp (15 17) 1)))
      ((1 S) . (* (or (=~ losers (nil 5) 1) (=~ hcp (12 nil) 1))
                  (=~ losers (5 nil) 1) (=~ hcp (nil 22) 1)
                  (=~ S (5 nil) 1)))
      ((1 H) . (* (or (=~ losers (nil 5) 1) (=~ hcp (12 nil) 1))
                  (=~ losers (5 nil) 1) (=~ hcp (nil 22) 1)
                  (=~ H (5 nil) 1) (=~ S (nil 4) 1)))
      ((1 D) . (* (or (=~ losers (nil 5) 1) (=~ hcp (12 nil) 1))
                  (=~ losers (4 nil) 1) (=~ hcp (nil 22) 1)
                  (=~ D (4 nil) 1) (=~ H (nil 4) 1) (=~ S (nil 4) 1)
                  (or (=~ D (5 nil) 1) (> D C) (* (=~ C 4 1) (> Dpower Cpower)))))
      ((1 C) . (* (or (=~ losers (nil 5) 1) (=~ hcp (12 nil) 1))
                  (=~ losers (4 nil) 1) (=~ hcp (nil 22) 1)
                  (=~ C (3 nil) 1) (=~ H (nil 4) 1) (=~ S (nil 4) 1)))
      ((2 D) . (* (=~ hcp (6 nil) 1) (=~ Dpower (5 nil) 1)
                  (or (=~ losers (6 nil) 1) (=~ hcp (nil 10) 1)) (=~ D 6 1)))
      ((2 H) . (* (=~ hcp (6 nil) 1) (=~ Hpower (5 nil) 1)
                  (or (=~ losers (6 nil) 1) (=~ hcp (nil 10) 1)) (=~ H 6 1)))
      ((2 S) . (* (=~ hcp (6 nil) 1) (=~ Spower (5 nil) 1)
                  (or (=~ losers (6 nil) 1) (=~ hcp (nil 10) 1)) (=~ S 6 1)))
      ((3 C) . (* (=~ hcp (6 10) 1) (=~ Cpower (5 nil) 1) (=~ C (7 nil) 1)))
      ((3 D) . (* (=~ hcp (6 10) 1) (=~ Dpower (5 nil) 1) (=~ D (7 nil) 1)))
      ((3 H) . (* (=~ hcp (6 10) 1) (=~ Hpower (5 nil) 1) (=~ H (7 nil) 1)))
      ((3 S) . (* (=~ hcp (6 10) 1) (=~ Spower (5 nil) 1) (=~ S (7 nil) 1)))
      (nil . 1/2)))

;; The scheme file: top-level entries ((bid) . rule), with ; comments
;; between them. Each entry is read as (bid rule start end), start..end
;; being the rule's text in the file, so a rule can be replaced by new text
;; keeping the rest of the file as it is.
(defparameter *scheme-file* "grade-bidding.cfg")

(defun skip-blank (str pos)
    (loop (when (>= pos (length str)) (return pos))
          (case (char str pos)
            ((#\Space #\Tab #\Newline #\Return #\Page) (incf pos))
            (#\; (setf pos (or (position #\Newline str :start pos) (length str))))
            (t (return pos)))))

(defun read-lisp (str start)
    (let ((*package* (find-package :cl-user)) (*read-eval* nil))
        (read-from-string str t nil :start start)))

(defun scheme-entries (str)
    (flet ((expect (char pos what)
             (unless (and (< pos (length str)) (char= char (char str pos)))
                 (error "Expected ~A at ~A: ~A" char what
                        (subseq str pos (min (length str) (+ pos 30)))))))
        (loop with pos = 0
              for start = (skip-blank str pos)
              while (< start (length str))
              collect (progn
                        (expect #\( start "an entry")
                        (multiple-value-bind (bid p) (read-lisp str (1+ start))
                            (let ((dot (skip-blank str p)))
                                (expect #\. dot (format nil "the bid ~A" bid))
                                (let ((rs (skip-blank str (1+ dot))))
                                    (multiple-value-bind (rule re) (read-lisp str rs)
                                        (let ((close (skip-blank str re)))
                                            (expect #\) close (format nil "the end of ~A" bid))
                                            (setf pos (1+ close))
                                            (list bid rule rs re))))))))))

(defun file-string (file)
    (with-open-file (s file :external-format :utf-8)
        (let ((str (make-string (file-length s))))
            (subseq str 0 (read-sequence str s)))))

; The file's entries, read again when it changes: (write-date entries)
(defvar *scheme* nil)
(defvar *scheme-lock* (sb-thread:make-mutex :name "scheme file"))

(defun scheme-file-entries ()
    (sb-thread:with-recursive-lock (*scheme-lock*)
        (let ((date (file-write-date *scheme-file*)))
            (unless (eql date (first *scheme*))
                (setf *scheme* (list date (scheme-entries (file-string *scheme-file*)))))
            (second *scheme*))))

;; The scheme in use, ((bid . rule) ...), and the text of bid's rule in the file
(defun graded-scheme ()
    (mapcar (lambda (e) (cons (first e) (second e))) (scheme-file-entries)))

(defun scheme-rule-text (bid)
    (let ((e (find bid (scheme-file-entries) :key #'first :test #'equal)))
        (and e (subseq (file-string *scheme-file*) (third e) (fourth e)))))

;; A rule given as text: one Lisp form, graded once on some hand to find
;; unknown measures and operators
(defun parse-rule (text)
    (multiple-value-bind (rule end) (read-lisp text 0)
        (when (< (skip-blank text end) (length text))
            (error "Text after the rule: ~A" (subseq text end)))
        (grade rule (hand-measures (str2hand "♠ AKQ2 ♥ 432 ♦ 432 ♣ 432")))
        rule))

;; Saves text as bid's rule in the scheme file, in place of the old text
(defun save-rule (bid text)
    (parse-rule text)
    (sb-thread:with-recursive-lock (*scheme-lock*)
        (let* ((str (file-string *scheme-file*))
               (e (or (find bid (scheme-entries str) :key #'first :test #'equal)
                      (error "No bid ~A in ~A" bid *scheme-file*)))
               (new (concatenate 'string (subseq str 0 (third e))
                                 (string-trim '(#\Space #\Tab #\Newline #\Return) text)
                                 (subseq str (fourth e)))))
            (scheme-entries new)
            (with-open-file (s *scheme-file* :direction :output :if-exists :supersede
                                             :external-format :utf-8)
                (write-string new s))
            (setf *scheme* nil))))

; Entries and their rule texts, with comments between them
(test (let* ((str (format nil "; c~%((1 S) . (and (=~~ S 5 1) ; long~% hcp))~%~%(nil . 1/2)~%"))
             (es (scheme-entries str)))
          (list (mapcar #'first es)
                (mapcar (lambda (e) (subseq str (third e) (fourth e))) es)))
      (list '((1 S) nil) (list (format nil "(and (=~~ S 5 1) ; long~% hcp)") "1/2"))
      equal)

(defun choose-graded (hand &optional (scheme (graded-scheme)))
    (best-graded (hand-measures hand) scheme))

; The translation opens as choose-bid does, on its test hands and random ones
(test (loop for str in '("♣ KJ9753 ♦ 94 ♥ A84 ♠ A9" "♣ Q86 ♦ KJ8 ♥ AQ1094 ♠ Q3"
                         "♣ AJ ♦ K73 ♥ K52 ♠ Q10942" "♣ Q1097642 ♦ Q98 ♥ 103 ♠ 7"
                         "♣ AQ109762 ♦ Q98 ♥ 103 ♠ 7" "♣ K6 ♦ KQJ ♥ AK108 ♠ KQ62"
                         "♣ 72 ♦ KJ862 ♥  ♠ AQ8652" "♣ K103 ♦ 102 ♥ AKJ10862 ♠ J"
                         "♣ AKQ10952 ♦ Q63 ♥ 9 ♠ K2" "♣ A95 ♦ 83 ♥ A4 ♠ 1076543"
                         "♠ 532 ♥ 83 ♦ AQ102 ♣ AQJ2" "♠ 532 ♥ 83 ♦ AQJ2 ♣ AQ102"
                         "♣ AKQJ5 ♦ KQ543 ♥ AQ ♠ 7" "♣ 85 ♦ K10 ♥ AQJ1032 ♠ 943")
            for hand = (str2hand str)
            unless (equal (choose-graded hand crisp-openings) (choose-bid hand))
            collect str)
      nil
      equal)

(test (let ((*random-state* (sb-ext:seed-random-state 11)))
          (loop repeat 50
                append (loop for hand in (random-deal)
                             unless (equal (choose-graded hand crisp-openings) (choose-bid hand))
                             collect (princ-to-string hand))))
      nil
      equal)

;; Hands for a def (one hand's ranges, as normdef gives them): random hands
;; that fit it. Faster than the deal generator, which works out its suit
;; lengths first, and with the true distribution of random hands. nil when
;; none of tries deals fits.
(defun in-range? (x range)
    (destructuring-bind (lo hi) (def-range range)
        (and (or (null lo) (>= x lo)) (or (null hi) (<= x hi)))))

(defun fits-def? (def measures)
    (flet ((m (key) (cdr (assoc key measures))))
        (and (in-range? (m 'hcp) (getf def :hcp))
             (loop for key in '(:c :d :h :s)
                   for len in '(C D H S)
                   for pts in '(Cpower Dpower Hpower Spower)
                   for v = (getf def key)
                   always (if (listp v)
                              (and (in-range? (m len) (subseq v 0 (min 2 (length v))))
                                   (in-range? (m pts) (third v)))
                              (in-range? (m len) v))))))

(defun def-hand (def &optional (tries 100000))
    (loop repeat tries
          do (dolist (hand (random-deal))
                 (when (fits-def? def (hand-measures hand))
                     (return-from def-hand hand)))))

(test (let ((m (hand-measures (str2hand "♠ AK2 ♥ Q432 ♦ 432 ♣ J32"))))
          (list (fits-def? '(:hcp (10 12) :h (4 nil (2 2))) m)
                (fits-def? '(:hcp (10 12) :h (4 nil (3 nil))) m)
                (fits-def? '(:s 3 :c (nil 2)) m)
                (fits-def? '(:hcp (12 14) :s (nil nil (7 7))) (hand-measures (def-hand '(:hcp (12 14) :s (nil nil (7 7))))))))
      '(t nil nil t)
      equal)

;; Measured meanings
; Each bid's meaning is measured over many hands: its share of them, and for
; each measure the probability of each value among the hands choosing it.
; Its ranges, the least and the most value of each, are a def as the deal
; generator takes it (:hcp (lo hi) :c (lo hi (points-lo points-hi)) ...), so
; later bids can deal hands for it.
(defparameter *meaning-measures* '(hcp losers C D H S Cpower Dpower Hpower Spower))

(defun value-probs (values)
    (let ((total (length values)))
        (sort (mapcar (lambda (x) (cons (first x) (/ (second x) total)))
                      (hist-base values))
              #'< :key #'car)))

(defun measure-ranges (measures)
    (flet ((span (key) (loop for m in measures
                             for v = (cdr (assoc key m))
                             minimize v into lo maximize v into hi
                             finally (return (list lo hi)))))
        (append (list :hcp (span 'hcp))
                (loop for key in '(:c :d :h :s)
                      for len in '(C D H S)
                      for pts in '(Cpower Dpower Hpower Spower)
                      append (list key (append (span len) (list (span pts))))))))

(defun meaning-of (bid measures total)
    (list :bid bid
          :count (length measures)
          :share (/ (length measures) total)
          :ranges (measure-ranges measures)
          :probs (loop for key in *meaning-measures*
                       collect (cons key (value-probs (mapcar (lambda (m) (cdr (assoc key m)))
                                                              measures))))))

;; Meanings of a scheme's bids over hands (each a hand), in scheme order,
;; leaving out bids no hand chose
(defun measure-bids (hands &optional (scheme (graded-scheme)))
    (let ((chosen (make-hash-table :test #'equal)))
        (dolist (hand hands)
            (let ((m (hand-measures hand)))
                (push m (gethash (best-graded m scheme) chosen))))
        (loop for (bid) in scheme
              for ms = (gethash bid chosen)
              when ms collect (meaning-of bid ms (length hands)))))

;; Openings measured over the hands of deals random deals
(defun measure-openings (deals &optional (scheme (graded-scheme)))
    (measure-bids (loop repeat deals append (random-deal)) scheme))

(defun print-meanings (meanings)
    (dolist (m meanings)
        (format t "~A ~,2F% (~D)~%  ~S~%"
                (or (getf m :bid) "Pass") (* 100 (getf m :share)) (getf m :count)
                (getf m :ranges))
        (loop for (key . probs) in (getf m :probs)
              do (format t "  ~A:~{ ~{~A:~,1F~}~}~%" key
                         (mapcar (lambda (p) (list (car p) (* 100 (cdr p)))) probs)))))

; 1NT's measured meaning is balanced 15-17, its shares sum to 1, and its
; ranges deal hands
(test (let* ((*random-state* (sb-ext:seed-random-state 5))
             (meanings (measure-openings 300 crisp-openings))
             (nt (find '(1 NT) meanings :key (lambda (m) (getf m :bid)) :test #'equal))
             (ranges (getf nt :ranges)))
          (list (= 1 (reduce #'+ (mapcar (lambda (m) (getf m :share)) meanings)))
                (getf ranges :hcp)
                (every (lambda (k) (let ((r (getf ranges k)))
                                     (and (>= (first r) 2) (<= (second r) 5))))
                       '(:c :d :h :s))
                (= 4 (length (build (make-instance 'deal :~ (list ranges)))))))
      '(t (15 17) t t)
      equal)
