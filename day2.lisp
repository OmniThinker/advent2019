; 1  Add, next 2 integers are positions of numbers that should be added, 
; 3rd is what position to store the value of sumation
; 2 Same as 1 except it multiplies
; 99 Means the program is finished and should stop
; When operation is done (except for 99) move 4 positions
            
(defparameter +filename+ "day2.input")

(defun process! (operations index)
  (let ((op-code (elt operations index)))
    (unless (equal op-code 99)
      (let* ((i-inp1 (elt operations (+ index 1)))
             (inp1 (elt operations i-inp1))
             (i-inp2 (elt operations (+ index 2)))
             (inp2 (elt operations i-inp2))
             (i-out (elt operations (+ index 3))))
        (case op-code
              (1 (setf (elt operations i-out)
                       (+ inp1 inp2)))
              (2 (setf (elt operations i-out)
                       (* inp1 inp2)))
              (otherwise (error "Something went wrong")))))))

(defun process-operations! (operations)
  (let ((res t))
       (loop for i from 0 below (length operations)
             by 4
             until (not res)
             do (setf res (process! operations i)))
    operations))


(defparameter *operations* (map 'vector
                                #'parse-integer 
                                (cl-ppcre:split "," (uiop:read-file-string +filename+))))

(assert (equalp (process-operations! #(1 9 10 3 2 3 11 0 99 30 40 50))
                #(3500 9 10 70 2 3 11 0 99 30 40 50)))
(assert (equalp (process-operations! #(1 0 0 0 99))
                #(2 0 0 0 99)))
(assert (equalp (process-operations! #(2 3 0 3 99))
                #(2 3 0 6 99)))
(assert (equalp (process-operations! #(2 4 4 5 99 0))
                #(2 4 4 5 99 9801)))
(assert (equalp (process-operations! #(1 1 1 4 99 5 6 0 99))
                #(30 1 1 4 2 5 6 0 99)))

(defparameter +magic-output+ 19690720)

(defun find-magic-numbers (operations)
  (let* ((res-lst (loop for noun from 0 upto 99
                        nconcing (loop for verb from 0 upto 99
                                       for x = (let ((cp (alexandria:copy-array operations)))
                                                  (progn
                                                    (setf (elt cp 1) noun)
                                                    (setf (elt cp 2) verb)
                                                    (process-operations! cp)
                                                    (list (elt cp 0) noun verb)))
                                       collect x)))
         (filtered (remove-if-not (lambda (x) 
                                    (eql (first x)
                                         +magic-output+))
                                 res-lst)))
    (if (not (eql (length filtered)
                  1))
        (error "Found more than one")
        (values (cadar filtered)
                (caddar filtered)))))


(multiple-value-bind (noun verb) (find-magic-numbers *operations*)
  (+ (* 100 noun) verb)) 





