;; read from file into lists
(ql:quickload "cl-ppcre")

;; get tree out of the input
;;
;; returns something like (((input-qty1 input-str1) (input-qty2 input-str2))
;;                         (output-qty output-str))
;; need to use nth 0/1 since I'm not a cons wizard
(defun get-tree (file-name)
  (with-open-file (file (pathname file-name))
    (loop for i from 0
      for line = (read-line file nil nil) while line
       collect (let ((in-and-out (cl-ppcre:split " => " line)) (input '()))
		 (list (dolist (element (cl-ppcre:split ", " (nth 0 in-and-out))
				input)
				(push (cl-ppcre:split " "  element) input))
		       (cl-ppcre:split " " (nth 1 in-and-out)))))))

;; take tree and make a hash table
(defun tree-to-reaction-table (tree)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (reaction tree table)
      (setf (gethash (nth 1 (nth 1 reaction)) table) reaction))))

(defun tree-to-qty-table (tree) ; init'd to 0
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "ORE" table) 0)
    (dolist (reaction tree table)
      (setf (gethash (nth 1 (nth 1 reaction)) table) 0))))

(defun eval-reaction (output reaction-table reqs-table)
  (let* ((resulting-output
	 (parse-integer (nth 0 (nth 1 (gethash output reaction-table)))))
	 (multiplier (ceiling
		      (/ (gethash output reqs-table) resulting-output))))
    (loop for input in (nth 0 (gethash output reaction-table))
       do (setf (gethash (nth 1 input) reqs-table)
		(+ (gethash (nth 1 input) reqs-table)
		   (* multiplier (parse-integer (nth 0 input))))))
    (setf (gethash output reqs-table)
	  (- (gethash output reqs-table)
	     (* multiplier resulting-output)))))

(defun resolve-reqs (reaction-table reqs-table)
  (loop for k being the hash-keys in reqs-table using (hash-value v)
	do (if (not (equal "ORE" k))
	       (if (> v 0)
		       (progn
			 (eval-reaction k reaction-table reqs-table)
			 (resolve-reqs reaction-table reqs-table)))))
  (gethash "ORE" reqs-table))

(defun run-task-one (fuel-qty) ; hardcoded to get one fuel, prints output of
			; resolve-reqs
  (let ((input (get-tree "input")))
    (resolve-reqs (tree-to-reaction-table input)
		    (let ((reqs-table (tree-to-qty-table input)))
			  (setf (gethash "FUEL" reqs-table) fuel-qty)
			  reqs-table))))

(print (run-task-one 1))

;; I'm gonna be super lazy about this, still faster than bruteforcing properly
(defun run-task-two ()
  (let ((n 0))
    (loop while (< (run-task-one n) 1000000000000)
       do (setf n (+ n 1000000)))
    (setf n (- n 1000000))
    (loop while (< (run-task-one n) 1000000000000)
       do (setf n (+ n 100000)))
    (setf n (- n 100000))
    (loop while (< (run-task-one n) 1000000000000)
       do (setf n (+ n 10000)))
    (setf n (- n 10000))
    (loop while (< (run-task-one n) 1000000000000)
       do (setf n (+ n 1000)))
    (setf n (- n 1000))
    (loop while (< (run-task-one n) 1000000000000)
       do (setf n (+ n 100)))
    (setf n (- n 100))
    (loop while (< (run-task-one n) 1000000000000)
       do (setf n (+ n 10)))
    (setf n (- n 10))
    (loop while (< (run-task-one n) 1000000000000)
       do (setf n (+ n 1)))
    (setf n (- n 1))
  n))



(print (run-task-two))
