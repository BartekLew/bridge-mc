;; Tests too slow to run each time bridge.cl and training.cl load (rest.cl
;; starting the server): test.sh loads this file after them. Each test is
;; kept as it was, with a note of where it came from.

;; bridge.cl, after distgen's next (about 5 s)
(file-based-test "test/distgen-test.dat"
  (let ((it (make-instance 'distgen :c '(5 nil) :d '(0 4) :h '(0 1) :hcp '(10 15) )))
    (loop for x = (next it)
          while x
          do (test-next x))))

;; bridge.cl, after build (about 50 s)
; Hands are dealt one after another, so the cards a hand takes may leave none
; that fit a later one (here N's honours S's suit points): such deals are
; dealt again
(test (let ((d (make-instance 'deal :~ '((:hcp (15 17) :s (2 4) :h (2 4) :d (2 4) :c (2 4))
                                         nil
                                         (:hcp (8 10) :h (5 nil (3 nil)) :d (4 nil (4 nil)))))))
          (loop repeat 200 always (= (length (build d)) 4)))
      t eq)

;; training.cl, after training-position (about 2 s)
; The computer plays a whole deal legally; with declarer's seats left to the
; player (S declaring) it stops after the opening lead
(test (let ((cards (computer-plays *training-hands* 2 2 nil nil))
            (h *training-hands*))
          (multiple-value-bind (left done) (training-position *training-hands* 2 2 cards)
              (and (= (length done) 13)
                   (every #'null (apply #'append left))
                   (loop for (tr leader) in done
                         always (legal-trick? h leader tr)
                         do (setf h (play-trick h leader tr))))))
      t eq)
