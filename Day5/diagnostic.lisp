;; read from file into list
(ql:quickload :split-sequence)



(defun get-list (file-name)
  (mapcar #'parse-integer (with-open-file (file (pathname file-name))
	 (split-sequence:split-sequence #\, (read-line file nil nil)))))

(defun process-params-two-args (function index list params)
  (let ((list (map 'list #'digit-char-p (format (format t "a ~3,'0d" params) "~5,'0d" params))))
    (values 0 0 0)))

;;; actual logic
;; apply function
(defun two-args (function index list params)
  (multiple-value-bind (first second third)
      (process-params-two-args function index list params)

    (loop for i from 0 for item in list
       collect (if (= i (nth (+ index 3) list))
                   (funcall function (nth (nth (+ index 1) list) list)
                            (nth (nth (+ index 2) list) list))
                   item))))


(defun get-input ()
  1)

(defun input-func (index list)
  (loop for i from 0 for item in list
     collect (if (= i (nth (+ index 1) list))
                 (get-input)
                 item)))

(defun output-func (index list params)
  (print params)
  (if params
      (print index)
      (print (nth (+ index 1) list)))
  list)


;; one iteration
(defun eval-instruction (index list)
  (let* ((params (floor (nth index list) 100))
         (opcode (- (nth index list) params)))
    (print opcode)
    (print params)
    (print "b")
    (case opcode
      (1 (two-args #'+ index list params))
      (2 (two-args #'* index list params))
      (3 (input-func index list))
      (4 (output-func index list params))
      (99 nil)
      (otherwise (print "error bad OPCODE")
                 (return-from eval-instruction list)))))

;; loop it
(defun exec-program (index list)
  (let ((new-list (eval-instruction index list)))
    (if (not new-list) list
	(exec-program (+ index (case (nth index list)
                                 (1 4) (2 4)
                                 (3 2) (4 2)
                                 (otherwise 1)))
                      new-list))))

;;; set up program
(defun insert-inputs (list noun verb)
  (loop for i from 0 for item in list
     collect (if (= i 1) noun (if (= i 2) verb item))))

;; jam exec-program and insert-inputs together
(defun run-with-inputs (filename noun verb)
  (exec-program 0 (insert-inputs (get-list filename) noun verb)))

;; part 1
(exec-program 0 (get-list "input"))
