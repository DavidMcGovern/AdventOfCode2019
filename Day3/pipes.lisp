;; read from file into lists
(ql:quickload :split-sequence)

;; get lists out of the input
(defun get-lists (file-name)
  (with-open-file (file (pathname file-name))
    (loop for i from 0
      for line = (read-line file nil nil) while line
       collect (split-sequence:split-sequence #\, line))))

(defun generate-coords (list)
  (let ((new-list '()) (current-x 0) (current-y 0) (num-steps 0))
    (loop for item in list
       do (let ((command (char item 0)) (number-of-repetitions (parse-integer (subseq item 1))))
	    (dotimes (j number-of-repetitions)
	      (case command
		(#\U (incf current-x))
		(#\D (decf current-x))
		(#\L (decf current-y))
		(#\R (incf current-y)))
	      (incf num-steps)
	      (setq new-list (append new-list (list (list current-x current-y num-steps)))))))
    new-list))

;; put the two paths through generate-coords
(defun paths-to-coords (paths)
  (list (generate-coords (nth 0 paths)) (generate-coords (nth 1 paths))))


(defun get-intersections (coords)
  (let ((out '()))
    (dolist (1st-coord (nth 0 coords))
      (dolist (2nd-coord (nth 1 coords))
	(if (and (equal (nth 0 1st-coord) (nth 0 2nd-coord))
		 (equal (nth 1 1st-coord) (nth 1 2nd-coord)))
		 (push (list (nth 0 1st-coord) (nth 1 2nd-coord)
			     (nth 2 1st-coord) (nth 2 2nd-coord)) out))))
    out))


(defparameter *intersection-list*
  (print (get-intersections (paths-to-coords (get-lists "input"))))) ; saving it to save like 10mins


;;; Solution to part one
;; first manhattan distance funcs
;; takes form (x y), only calculates from 0
(defun manhattan-distance (coord)
  (+ (abs (nth 0 coord)) (abs (nth 1 coord))))


(defun shortest-manhattan-distance (coords)
  (let ((shortest-seen (nth 0 coords)))
    (dolist (coord coords)
      (if (< (manhattan-distance coord) (manhattan-distance shortest-seen))
	  (setf shortest-seen coord)))
    shortest-seen))


(format t "Shortest Manhattan Distance: ~D~%"
	 (manhattan-distance (shortest-manhattan-distance *intersection-list*)))

;;; Part 2
(defun total-steps (coord)
  (+ (nth 2 coord) (nth 3 coord)))


(defun least-steps (coords)
  (let ((shortest-seen (nth 0 coords)))
    (dolist (coord coords)
      (if (< (total-steps coord) (total-steps shortest-seen))
	  (setf shortest-seen coord)))
    shortest-seen))

(format t "Least Number of Steps: ~D~%"
	 (total-steps (least-steps *intersection-list*)))
