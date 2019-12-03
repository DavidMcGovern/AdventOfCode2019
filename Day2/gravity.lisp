;; read from file into list
(ql:quickload :split-sequence)

(defun get-list (file-name)
  (mapcar #'parse-integer (with-open-file (file (pathname file-name))
	 (split-sequence:split-sequence #\, (read-line file nil nil)))))

;;; actual logic
;; apply function
(defun apply-function (function index list)
  (loop for i from 0 for item in list
     collect (if (= i (nth (+ index 3) list))
		 (funcall function (nth (nth (+ index 1) list) list)
			           (nth (nth (+ index 2) list) list))
		 item)))

;; one iteration
(defun eval-instruction (index list)
  (case (nth index list)
    (1 (apply-function #'+ index list))
    (2 (apply-function #'* index list))
    (99 nil)
    (otherwise (print "error bad OPCODE"))))

;; loop it
(defun exec-program (index list)
  (let ((new-list (eval-instruction index list)))
    (if (not new-list) list
	(exec-program (+ index 4) new-list))))

;;; set up program
(defun insert-inputs (list noun verb)
  (loop for i from 0 for item in list
     collect (if (= i 1) noun (if (= i 2) verb item))))

;; jam exec-program and insert-inputs together
(defun run-with-inputs (filename noun verb)
  (exec-program 0 (insert-inputs (get-list filename) noun verb)))

;; part 1
(format t "0th index of 1202 program: ~D~%" (nth 0 (run-with-inputs "input" 12 02)))

;; part two (just gonna bruteforce it tbh)
(dotimes (noun 100) (dotimes (verb 100)
		      (if (= 19690720 (nth 0 (run-with-inputs "input" noun verb)))
			  (format t "Part 2 solved with noun: ~D, verb: ~D~%"
				  noun verb))))
;; took 6.16 seconds :P
