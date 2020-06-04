(ql:quickload "cl-ppcre")
(ql:quickload "iterate")


(defun first-test (code)
  (ppcre:scan ; lol
   ".*?00.*?|.*?11.*?|.*?22.*?|.*?33.*?|.*?44.*?|.*?55.*?|.*?66.*?|.*?77.*?|.*?88.*?|.*?99.*?"
   (write-to-string code)))

(defun second-test (code)
  (let ((prev #\0) (ret 1))
    (loop for c across (write-to-string code)
	  do (if (char< c prev) (setf ret 0))
	  do (setf prev c))
    (eq ret 1)))


(defun check-codes (code)
   (and (first-test code) (second-test code)))


(defun loop-over (start fin)
  (let ((count 0))
    (loop for i from start to fin
	  do (if (check-codes i) (incf count)))
    count))


(format t "First number of codes: ~D~%" (loop-over 206938 679128))


(defun first-test-redux (code) ;; don't like regex now
  (let ((prev #\!) (prev-plus #\!) (prev-plus-plus #\!)
	(passed 0) (str (write-to-string code)));; ! is before digits
     (loop for c across str for i from 0
	   do (if (and (not (char= c prev)) (char= prev prev-plus)
		       (not (char= prev-plus prev-plus-plus)))
		  (setf passed 1)) ;; there's a triple
	   do (setf prev-plus-plus prev-plus)
	   do (setf prev-plus prev)
	   do (setf prev c))

     (or (and (char= (char str 4) (char str 5)) (not (char= (char str 3) (char str 4))))
	 (= 1 passed))))

(defun check-codes-redux (code)
   (and (first-test-redux code) (second-test code)))


(defun loop-over-redux (start fin)
  (let ((count 0))
    (loop for i from start to fin
	  do (if (check-codes-redux i) (incf count)))
    count))


(format t "With extra rule: ~D~%" (loop-over-redux 206938 679128))
