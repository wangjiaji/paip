(defun sum-squares (seq)
  (let ((sum 0))
    (dotimes (i (length seq))
      (incf sum (square (elt seq i))))
    sum))

(defun square (x)
  (* x x))

(defun sum-squares (vect)
  (declare (type (simple-array fixnum *) vect)
	   (inline square)
	   (optimize (speed 3) (safety 0)))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i (length seq))
      (declare (fixnum i))
      (incf sum (the fixnum (square (svref vect i)))))
    sum))

(defmacro defun* (fn-name arg-list &body body)
  (if (and (member '&key arg-list) (not (member '&rest arg-list)))
      (let ((no-key-fn-name (symbol fn-name '*no-key))
	    (args (mapcar #'first-or-self (set-difference arg-list lambda-list-keywords))))
	`(progn
	  (proclaim '(inline ,fn-name))
	  (defun ,no-key-fn-name ,args
	    ,@body)
	  (defun ,fn-name ,arg-list
	    (,no-key-fn-name ,@args))))
      `(defun ,fn-name ,arg-list
	 ,@body)))

