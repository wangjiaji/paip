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

(defvar *bindings* nil
  "A list of bindings used by the rule compiler")

(defun compile-rule (rule)
  "Compile a single rule"
  (let ((*bindings* nil))
    `(lambda (x)
       ,(compile-expr 'x (expr-lhs rule)
		      (delay (build-expr (expr-rhs rule) *bindings*))))))

(defun compile-expr (var pattern consequent)
  (cond ((get-binding pattern *bindings*)
	 `(if (equal ,var ,(lookup pattern *bindings*))
	      ,(force consequent)))
	((variable-p pattern)
	 (push (cons pattern var) *bindings*)
	 (force consequent))
	((atom pattern)
	 `(if (eql ,var ',pattern)
	      ,(force consequent)))
	((starts-with pattern '?is)
	 (push (cons (second pattern) var) *bindings*)
	 `(if (,(third pattern) ,var)
	      ,(force consequent)))
	(t `(if (op? ,var ',(expr-op pattern))
		,(compile-args var pattern consequent)))))

(defun compile-args (var pattern consequent)
  (let ((L (symbol var 'L))
	(R (symbol var 'R)))
    (if (expr-rhs pattern)
	`(let ((,L (expr-lhs ,var))
	       (,R (expr-rhs ,var)))
	   ,(compile-expr L (expr-lhs pattern)
			  (delay (compile-expr R (expr-rhs pattern)
					       consequent))))
	`(let ((,L (expr-lhs ,var)))
	   ,(compile-expr L (expr-lhs pattern) consequent)))))

(defun build-expr (expr bindings)
  (cond ((assoc expr bindings)
	 (rest (assoc expr bindings)))
	((variable-p expr)
	 (error "Variable ~a occurred on RHS" expr))
	((atom expr) `',expr)
	(t (let ((new-expr (mapcar #'(lambda (x)
				       (build-expr x bindings))
				   expr)))
	     `(simplify-expr (list ,@new-expr))))))

(defun op? (expr op)
  (and (expr-p expr) (eq (expr-op expr) op)))

(defun symbol (&rest args)
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  (make-symbol (format nil "~{~a~}" args)))

(defun compile-rule-set (op)
  (set-simp-fn op
	       (compile nil `(lambda (x)
			       ,(reduce #'combine-rules
					(mapcar #'compile-indexed-rule
						(rules-for op)))))))

(defun compile-indexed-rule (rule)
  (let ((*bindings* nil))
    (compile-args 'x (expr-lhs rule)
		  (delay (build-expr (expr-rhs rule) *bindings*)))))

(defun combine-rules (a b)
  (cond ((and (listp a) (listp b)
	      (= (length a) (length b) 3)
	      (equal (first a) (first b))
	      (equal (second a) (second b)))
	 (list (first a) (second a) (combine-rules (third a) (third b))))
	((matching-ifs a b)
	 `(if ,(second a)
	      ,(combine-rules (third a) (third b))
	      ,(combine-rules (fourth a) (fourth b))))
	((starts-with a 'or)
	 (if (matching-ifs (first (last a)) b)
	     (append (butlast a)
		     (list (combine-rules (first (last a)) b)))
	     (append a (list b))))
	(t `(or ,a ,b))))

(defun matching-ifs (a b)
  (and (starts-with a 'if) (starts-with b 'if)
       (equal (second a) (second b))))

(defun compile-all-rules-indexed (rules)
  (index-rules rules)
  (let ((all-ops (delete-duplicates (mapcar #'main-op rules))))
    (mapc #'compile-rule-set all-ops)))

(defun simplify-expr (expr)
  (cond ((simplify-by-fn expr))
	((evaluable expr) (eval expr))
	(t expr)))

