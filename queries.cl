;; Show all hands from simulation
(select sim :fields '(hand))

;; Show deals from given simulation, with expected tricks = 12
(show-deals sim '(= best-tricks 10))

;; TODO: fix case
(simdeal '(((5 10 11 12) (8) (0 1 4 9 10) (0 1 6))
     ((2 3 9) (0 6 9 11) (6) (3 7 9 10 11))
     ((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))
     ((4 8) (1 2 4 7 10) (2 8 11) (2 4 5))))

(measure-time (simdeal '(((1 2 3) (0 12) (1) ())
                         ((10 11 12) (11) (0) (0))
                         ((0) (1) (10 11 12) (1))
                         ((4 5 6) () (2) (11 12)))))

;; TODO: fix this case
(simdeal '(((5 12) (0 7 9) (1 4 6 8 11) (5 9 11))
     ((4 8 10) (4 8) (0 2 10) (0 3 4 7 10))
     ((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))
     ((2 3 9 11) (1 2 6 10 11) (9) (1 2 6))))

(defun simdeal (deal &key trump)
    (print-deal deal '(n e s w))
    (let-from! (apply (curry #'run (make-instance 'cache :! #'immediate-tricks) trump)
                      (mapcar (curry #'make-instance 'hand :=) deal))
               (tricks remaining)
        (format t "winners: ~A~%" (length tricks))
        (loop for trick in tricks
              do (format t "~{~A ~}~%" (mapcar #'cardstr trick)))
        (print-deal (mapcar #'suits remaining))))
                                
;;Print histograms for given fields of particular deal
(print-hist (histogram (mapcar #'flatten (select sim :fields '(untrump-balance) ))))
(print-hist (histogram (mapcar #'flatten (select sim :fields '(best-tricks) ))))
(print-hist (histogram (mapcar #'flatten (select sim :fields '(partner-clubs) ))))

;; Perform simulation for given deal
(simdeal ' (((11) (0 4 8 11) (0 1 2 8 11) (4 7 11))
     ((2 3 5 8 12) (2 7 10) (4 10) (1 3 5))
     ((0 1 6 7) (3 5 12) (3 5 7 12) (8 12))
     ((4 9 10) (1 6 9) (6 9) (0 2 6 9 10))))

;; Print histogram for hcp-tricks relation
(print-hist (histogram (select simhcp)))

;; plot showing how NT trick probabilities change depending on hcp on line
(let ((h (histogram (select simhcp))))
  (hist-plot (nthcdr 2 (nth 0 h)) :params "pch=0, xlim=c(7,13), ylim=c(0,0.5)" :out T)
  (loop for i from 1 to 20
        do (hist-plot (nthcdr 2 (nth i h)) :cmd "lines" :out T
                      :params (format nil "pch=~a" i))))

;; plot showing how average NT tricks change depending on hcp on line
(let* ((h (sort (histogram (select simhcp)) (lambda (a b) (< (car? (first a)) (car? (first b))))))
       (hcps (mapcar (f* #'first #'car?) h))
       (avgs (mapcar (lambda (case)
                        (if (listp (first case))
                            (second (first case))
                            (apply #'+ (mapcar (curry #'apply #'*)
                                               (nthcdr 2 case)))))
             h)))
    (format T "hcps=c(~{~A~^,~}); avgs=c(~{~,7f~^,~}); plot(hcps, avgs, type='b');~%"
              hcps avgs))


;; plot showing how HCP-trick ratio change with HCP
(let* ((h (sort (histogram (select simhcp)) (lambda (a b) (< (car? (first a)) (car? (first b))))))
       (hcps (mapcar (f* #'first #'car?) h))
       (avgs (mapcar (lambda (case)
                        (if (listp (first case))
                            (second (first case))
                            (apply #'+ (mapcar (curry #'apply #'*)
                                               (nthcdr 2 case)))))
             h)))
    (format T "hcp=c(~{~A~^,~}); tph=c(~{~,7f~^,~}); plot(hcps, tph, type='b');~%"
               hcps (loop for hcp in hcps
                          for avg in avgs
                          collect (float (/ avg hcp)))))

(show-deals simhcp '(= line-hcp 25))

(simdeal '(((1 2 3 7 12) (0 8 12) (0 10) (3 4 11))
     ((4 8 11) (5 6 11) (2 3 4 9 11) (1 9))
     ((0 5) (3 4 10) (6 8 12) (0 5 6 10 12))
     ((6 9 10) (1 2 7 9) (1 5 7) (2 7 8))))
