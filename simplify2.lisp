(defmacro with-profiling (fn-names &body body)
  `(progn
     (profile ,@fn-names)
     (unwind-protect
	  (progn ,@body)
       (report)
       (unprofile ,@fn-names))))

(defvar *test-data*
  (mapcar #'infix->prefix
	  '((d (a * x ^ 2 + b * x + c) / d x)
	    (d ((a * x ^ 2 + b * x + c) / x) / d x)
	    (d ((a * x ^ 3 + b * x ^ 2 + c * x + d) / x ^ 5) / d x)
	    ((sin (x + x)) * (sin (2 * x)) + (cos (d (x ^ 2) / d x)) ^ 1)
	    (d (3 * x + (cos x) / x) / d x))))

(defvar *answers* (mapcar #'simplify *test-data*))

(defun test-it (&optional (profile t))
  "Time a test run, check the results"
  (let ((answers (if profile
		     (with-profiling (simplify simplify-expr pat-match match-variable variable-p)
		       (mapcar #'simplify *test-data*))
		     (time (mapcar #'simplify *test-data*)))))
    (mapc #'assert-equal answers *answers*)
    t))

(defun assert-equal (x y)
  (assert (equal x y) (x y)
	  "Expected ~a to be equal to ~a" x y))

(defun simplify-expr (expr)
  (cond ((simplify-by-fn expr))
	((rule-based-translator
	  exp
	  (rules-for (expr-op expr))
	  :rule-if #'expr-lhs
	  :rule-then #'expr-rhs
	  :action #'(lambda (bindings response)
		      (simplify (sublis bindings response)))))
	((evaluable expr) (eval expr))
	(t expr)))

(defvar *rules-for* (make-hash-table :test #'eq))

(defun main-op (rule)
  (expr-op (expr-lhs rule)))

(defun index-rules (rules)
  (clrhash *rules-for*)
  (dolist (rule rules)
    (setf (gethash (main-op rule) *rules-for*)
	  (nconc (gethash (main-op rule) *rules-for*)
		 (list rule)))))

(defun rules-for (op)
  (gethash op *rules-for*))

(index-rules *simplification-rules*)
