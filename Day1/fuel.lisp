(defun calc-fuel (mass)
  (- (floor (/ mass 3)) 2))


;; solution to part one
(defparameter total-fuel (with-open-file (file #P"input")
  (loop for i from 0
     for line = (read-line file nil nil)
     while line
       sum (calc-fuel (parse-integer line)))))

(format t "Total Fuel Used(initial value): ~D~%" total-fuel)


;; solution to part two
(defun calc-fuel-plus (mass)
  (if (<= mass 5) ;; 5 will become -1, probably a better solution
      0
      (+ (calc-fuel mass) (calc-fuel-plus (calc-fuel mass)))))


(defparameter total-fuel-plus (with-open-file (file #P"input")
  (loop for i from 0
     for line = (read-line file nil nil)
     while line
       sum (calc-fuel-plus (parse-integer line)))))

(format t "Total Fuel Used(extra calculation): ~D~%" total-fuel-plus)
