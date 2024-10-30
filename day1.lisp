(defparameter +filename+ "day1.input")


(defun calc-fuel (fuel)
  (- (floor (/ fuel 3))
     2))

(defun required-fuel (fuel)
  (labels ((internal (fuel acc-fuel)
                     (let ((req-f (calc-fuel fuel)))
                       (if (<= req-f 0)
                           acc-fuel
                           (internal req-f (+ acc-fuel req-f))))))
    (internal fuel 0)))

(loop for line in (uiop:read-file-lines +filename+)
     sum (required-fuel (parse-integer line))) 

