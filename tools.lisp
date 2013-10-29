(defun memo (fn name key test)
  "Return a memo function of fn"
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	(let ((k (funcall key args)))
	  (multiple-value-bind (val found-p)
	      (gethash k table)
	    (if found-p
		val
		(setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with the memoized one"
  (setf (symbol-function fn-name)
	(memo (symbol-function fn-name) fn-name key test)))

(defun clear-memoize (fn-name)
  (let ((table (get fn-name 'memo)))
    (when table
      (clrhash table))))

(defun rule-lhs (rule)
  (first rule))

(defun rule-rhs (rule)
  (cddr rule))

(defun one-of (choices)
  (list (random-elt choices)))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun length=1 (seq)
  (and (consp seq) (null (rest seq))))

(defun compile-rule (rule)
  "Translate a grammar rule into a Lisp function definition"
  (let ((rhs (rule-rhs rule)))
    `(defun ,(rule-lhs rule) ()
       ,(cond ((every #'atom rhs) `(one-of ',rhs))
	      ((length=1 rhs)
	       (build-code (first rhs)))
	      (t `(case (random ,(length rhs))
		    ,@(build-cases 0 rhs)))))))

(defun build-cases (number choices)
  (when choices
    (cons (list number (build-code (first choices)))
	  (build-cases (1+ number) (rest choices)))))

(defun build-code (choice)
  "Append together multiple constituents"
  (cond ((null choice) nil)
	((atom choice) (list choice))
	(t `(append ,@(mapcar #'build-code choice)))))

(defmacro defrule (&rest rule)
  (compile-rule rule))

