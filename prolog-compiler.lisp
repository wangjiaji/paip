(defun prolog-compile (symbol &optional (clauses (get-clauses symbol)))
  "Compile a symbol; make a seperate function for each arity"
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (compile-predicate symbol arity (clauses-with-arity clauses #'= arity))
      ;; Compile all clauses with other arities
      (prolog-compile symbol (clauses-with-arity clauses #'/= arity)))))

(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity"
  (find-all arity clauses
	    :key #'(lambda (clause)
		     (relation-arity (clause-head clause)))
	    :test test))

(defun relation-arity (relation)
  "Returns the number of arguments to a relation"
  (length (args relation)))

(defun args (x)
  (rest x))

(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity into a single Lisp function"
  (let ((predicate (make-predicate symbol arity))
	(parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,predicate (,@parameters cont)
	 ,@(mapcar #'(lambda (clause)
		       (compile-clause parameters clause 'cont))
		   clauses))))))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-{arity})"
  (loop for i from 1 to arity
       collect (new-symbol '?arg i)))

(defun make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (symbol symbol '/ arity))

(defun compile-clause (params clauses cont)
  "Transform away the head, and compile the resulting body"
  (compile-body
   (nconc (mapcar #'make-= params (args (clause-head clause)))
	  (clause-body clause))
   cont))

(defun make-= (x y)
  `(= ,x ,y))

(defun compile-body (body cont)
  "Compile the body of a clause"
  (if (null body)
      `(funcall ,cont)
      (let* ((goal (first body))
	     (macro (prolog-compiler-macro (predicate goal)))
	     (macro-val (if macro
			    (funcall macro goal (rest body) cont))))
	(if (and macro (not (eq macro-val :pass)))
	    macro-val
	    (compile-call (make-predicate (predicate goal)
					  (relation-arity goal))
			  (mapcar #'(lambda (arg)
				      (compile-arg arg))
				  (args goal))
			  (if (null (rest body))
			      cont
			      `#'(lambda ()
				   ,(compile-body (rest body) cont))))))))

(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate"
  `(,predicate ,@args ,cont))

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a predicate"
  (get name 'prolog-compiler-macro))

(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog"
  `(setf (get ',name 'prolog-compiler-macro)
	 #'(lambda ,arglist ,@body)))

(def-prolog-compiler-macro = (goal body cont)
  (let ((args (args goal)))
    (if (/= (length args) 2)
	:pass
	`(if ,(compile-unify (first args) (second args))
	     ,(compile-body body cont)))))

(defun compile-unify (x y)
  "Return code that tests if var and term unify"
  '(unify! ,(compile-arg x) ,(compile-arg y)))

(defun compile-arg (arg)
  "Generate code for an argument to a goal in the body"
  (cond ((variable-p arg) arg)
	((not (has-variable-p arg)) `',arg)
	((proper-listp arg)
	 `(list ,@(mapcar #'compile-arg arg)))
	(t `(cons ,(compile-arg (first arg))
		  ,(compile-arg (rest arg))))))

(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))
