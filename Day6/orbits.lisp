(ql:quickload :iterate)

;; get map out of the input
(defun get-map (file-name)
  (with-open-file (file (pathname file-name))
    (loop for i from 0
      for line = (read-line file nil nil) while line
       collect (split-sequence:split-sequence #\) line))))

;; calc how many orbits each planet has
(defun path-to-com (map planet)
  (loop for orbit in map
     sum (if (string= planet (nth 1 orbit))
             (if (string= "COM" (nth 0 orbit))
                 1
                 (+ 1 (path-to-com map (nth 0 orbit))))
             0)))

;; cover the tree
(defun sweep-tree (map planet)
  (loop for orbit in map
     sum (if (string= planet (nth 0 orbit))
             (+ (path-to-com map (nth 1 orbit))
                (sweep-tree map (nth 1 orbit)))
             0)))


(print (path-to-com (get-map "testinput") "L"))

(print (sweep-tree (get-map "testinput") "COM"))

(print (sweep-tree (get-map "input") "COM"))

(defun has-descendant (map child ancestor)
  (loop for orbit in map
     sum (if (and (string= ancestor (nth 0 orbit))
                  (or
                   (/= 0 (has-descendant map child (nth 1 orbit)))
                   (string= child (nth 1 orbit))))
             1
             0)))

(defun deepest-common-ancestor (map B1 B2)
  (iter:iterate (iter:for orbit in map)
                (iter:finding (nth 1 orbit)
                      iter:maximizing (* (has-descendant map B1 (nth 1 orbit))
                                         (has-descendant map B2 (nth 1 orbit))
                                         (path-to-com map (nth 1 orbit))))))

(print (has-descendant (get-map "testinput") "YOU" "C"))



(print (deepest-common-ancestor (get-map "testinput") "YOU" "SAN"))


;; drop the com, 1 less dist for part 2 reasons
(defun path-to (map planet root)
  (loop for orbit in map
     sum (if (string= planet (nth 1 orbit))
             (if (string= root (nth 0 orbit))
                 0
                 (+ 1 (path-to map (nth 0 orbit) root)))
             0)))

(print (path-to (get-map "testinput") "SAN" "D"))

(defun distance-between (map B1 B2)
  (let ((root (deepest-common-ancestor map B1 B2)))
    (+ (path-to map B1 root) (path-to map B2 root))))
   ;; a newline right about ^ broke it

(print (distance-between (get-map "testinput") "YOU" "SAN")) ; 4 = yes

(print (deepest-common-ancestor (get-map "input") "YOU" "SAN"))

(print (path-to (get-map "input") "YOU" "9SV")) ;; 167
(print (path-to (get-map "input") "SAN" "9SV")) ;; 179
                                               ;  =346


(print (distance-between (get-map "input") "YOU" "SAN")) ;; running way too long
;;; 17.56 seconds lmao





    ;;;; I wish I had my algorithms course done
