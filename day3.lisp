; R8,U5,L5,D3, right 8, up 5, left 5, down 3
; U7,R6,D4,L4
; Distance is 6 because the nearest crossover of the 
; lines is closer in manhattan distance.

; example 2 
; R75,D30,R83,U83,L12,D49,R71,U7,L72
; U62,R66,U55,R34,D71,R55,D58,R83
; Distance is 159

; example 3
; R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
; U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
; Distance is 135

(defun get-coordinates (sequence)
       (butlast 
         (reduce (lambda (coordinates step)
                   (let ((x (caar coordinates))
                         (y (cdar coordinates))
                         (step-cnt (parse-integer (subseq step 1))))
                     (append (case (elt step 0)
                               (#\U (loop for i from (+ step-cnt y) downto (+ y 1)
                                          collect (cons x i)))
                               (#\R (loop for i from (+ step-cnt x) downto (+ x 1)
                                          collect (cons i y)))
                               (#\D (loop for i from (- y step-cnt) upto (- y 1)
                                          collect (cons x i)))
                               (#\L (loop for i from (- x step-cnt) upto (- x 1)
                                          collect (cons i y))))
                             coordinates)))
                 sequence
                 :initial-value
                 (list (cons 0 0)))))

(defun smallest-manhattan (in1 in2)
  (loop for x in (mapcar (lambda (coords)
                           (+ (abs (car coords))
                              (abs (cdr coords)))) 
                         (intersection (get-coordinates in1)
                                       (get-coordinates in2)
                                       :test #'equalp))
        minimize x))

(assert (eql (smallest-manhattan '("R8" "U5" "L5" "D3")
                                 '("U7" "R6" "D4" "L4"))
             6))

(assert (eql (smallest-manhattan '("R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72")
                                 '("U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"))
            159))

(defun intersection-steps (in1 in2)
  (let* ((seq1 (get-coordinates in1))
         (seq2 (get-coordinates in2)))
    (loop for coord1 in seq1
          for steps1 from 1
          nconcing (loop for coord2 in seq2
                         for steps2 from 1
                         when (equalp coord1 coord2)
                         collect (list steps1 steps2)))))

(assert (eql (intersection-steps '("R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72")
                                 '("U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"))
            159))

(assert (eql (intersection-steps '("R8" "U5" "L5" "D3")
                                 '("U7" "R6" "D4" "L4"))
             6))

        
(defun load-coordinates ()
  (let* ((lines (uiop:read-file-lines "day3.input"))
         (in1 (first lines))
         (in2 (second lines))
         (seq1 (cl-ppcre:split "," in1))
         (seq2 (cl-ppcre:split "," in2)))
    (values seq1 seq2)))

(multiple-value-bind (seq1 seq2) (load-coordinates)
  (get-coordinates))
  ;(smallest-manhattan seq1 seq2))






