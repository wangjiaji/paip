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
  (let ((*predicate* (make-predicate symbol arity))
	(parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,*predicate* (,@parameters cont)
	 ,@(maybe-add-undo-bindings
	    (mapcar #'(lambda (clause)
			(compile-clause parameters clause 'cont))
		    clauses)))))))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-{arity})"
  (loop for i from 1 to arity
       collect (new-symbol '?arg i)))

(defun make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (conc-symbol symbol '/ arity))		; intern function name

(defun compile-clause (params clause cont)
  "Transform away the head, and compile the resulting body"
  (bind-unbound-vars params
		     (compile-body
		      (nconc (mapcar #'make-= params (args (clause-head clause)))
			     (clause-body clause))
		      cont
		      (mapcar #'self-cons params))))

(defun make-= (x y)
  `(= ,x ,y))

(defvar *predicate* nil
  "The Prolog predicate currently been compiled")

(defun compile-body (body cont bindings)
  "Compile the body of a clause"
  (cond ((null body)
	 `(funcall ,cont)
	 ((eq (first body) '!)
	  `(progn ,(compile-body (rest body) cont bindings)
		  (return-from ,*predicate* nil)))
	 (t (let* ((goal (first body))
		   (macro (prolog-compiler-macro (predicate goal)))
		   (macro-val (if macro
				  (funcall macro goal (rest body) cont bindings))))
	      (if (and macro (not (eq macro-val :pass)))
		  macro-val
		  (compile-call (make-predicate (predicate goal)
						(relation-arity goal))
				(mapcar #'(lambda (arg)
					    (compile-arg arg bindings))
					(args goal))
				(if (null (rest body))
				    cont
				    `#'(lambda ()
					 ,(compile-body (rest body)
							cont
							(bind-new-variables bindings goal)))))))))))

(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal"
  (let ((variables (remove-if #'(lambda (v) (assoc v bindings))
			      (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))

(defun self-cons (x)
  (cons x x))

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

(def-prolog-compiler-macro = (goal body cont bindings)
  (let ((args (args goal)))
    (if (/= (length args) 2)
	:pass
	(multiple-value-bind (code1 bindings1)
	    (compile-unify (first args) (second args) bindings)
	  (compile-if code1 (compile-body body cont bindings1))))))

(defun compile-unify (x y bindings)
  "Return 2 values: code to test if x and y unify, and a new binding list"
  (cond ((not (or (has-variable-p x) (has-variable-p y)))
	 (values (equal x y) bindings))
	((and (consp x) (consp y))
	 (multiple-value-bind (code1 bindings1)
	     (compile-unify (first x) (first y) bindings)
	   (multiple-value-bind (code2 bindings2)
	       (compile-unify (rest x) (rest y) bindings1)
	     (values (compile-if code1 code2) bindings2))))
	((variable-p x)
	 (compile-unify-variable x y bindings))
	(t (compile-unify-variable y x bindings))))

(defun compile-if (pred then-part)
  "Compile a Lisp `if' from, no `else' part"
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))
  
(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body"
  (cond ((eq arg '?) '(?))
	((variable-p arg)
	 (let ((binding (get-binding arg bindings)))
	   (if (and (not (null binding))
		    (not (eq arg (binding-val binding))))
	       (compile-arg (binding-val binding) bindings)
	       arg)))
	((not (find-if-anywhere #'has-variable-p arg)) `',arg)
	((proper-listp arg)
	 `(list ,@(mapcar #'(lambda (a)
			      (compile-arg a bindings))
			  arg)))
	(t `(cons ,(compile-arg (first arg) bindings)
		  ,(compile-arg (rest arg) bindings)))))

(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

(defun maybe-add-undo-bindings (compiled-exprs)
  "Undo any bindings that need undoing. If there are any, bind the trail before we start"
  (if (= 1 (length compiled-exprs))
      compiled-exprs
      `((let ((old-trail (fill-pointer *trail*)))
	  ,(first compiled-exprs)
	  ,@(loop for expr in (rest compiled-exprs)
		 collect '(undo-bindings! old-trail)
		 collect expr)))))

(defun bind-unbound-vars (params expr)
  "If there are any variables in expr (besides the params) then bind them to new vars"
  (let ((expr-vars (set-difference (variables-in expr) params)))
    (if expr-vars
	`(let ,(mapcar #'(lambda (var)
			   `(,var (?)))
		       expr-vars)
	   ,expr)			; eval expr in the new bindings
	expr)))

(defun make-anonymous (expr &optional (anon-vars (anonymous-variables-in expr)))
  "Replace varialbes that are only used once with ?"
  (cond ((consp expr)
	 (cons (make-anonymous (first expr) anon-vars)
	       (make-anonymous (rest expr) anon-vars)))
	((member expr anon-vars) '?)
	(t expr)))

(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree"
  (let ((seen-once nil)
	(seen-more nil))
    (labels ((walk (x)
	       (cond ((variable-p x)
		      (cond ((member x seen-once)
			     (setf seen-once (delete x seen-once))
			     (push x seen-more))
			    ((member x seen-more) nil)
			    (t (push x seen-once))))
		     ((consp x)
		      (walk (first x))
		      (walk (rest x))))))
      (walk tree)
      seen-once)))			; anonymous vars are only seen once in a clause

(defun compile-unify-variable (x y bindings)
  "x is a variable, y may be"
  (let* ((xb (follow-binding x bindings))
	 (x1 (if xb (cdr xb) x))
	 (yb (if (variable-p y) (follow-binding y bindings)))
	 (y1 (if yb (cdr yb) y)))
    (cond ((or (eq x '?) (eq y '?))	; ignore anonymous variable
	   (values t bindings))
	  ((not (and (equal x x1) (equal y y1))) ; if any of them are bound, unify the bound value
	   (compile-unify x1 y1 bindings))
	  ((find-anywhere x1 y1)	; x1 occurs in y1
	   (values nil bindings))
	  ((consp y1)
	   (values `(unify! ,x1 ,(compile-arg y1 bindings))
		   (bind-variables-in y1 bindings)))
	  ((not (null xb))
	   (if (and (variable-p y1) (null yb))
	       (values 't (extend-bindings y1 x1 bindings))
	       (values `(unify! ,x1 ,(compile-arg y1 bindings))
		       (extend-bindings x1 y1 bindings))))
	  ((not (null yb))
	   (compile-unify-variable y1 x1 bindings))
	  (t (values 't (extend-bindings x1 y1 bindings))))))

(defun bind-variables-in (expr bindings)
  "Bind all variables in expr to themselves, and add that to bindings (except for variables already bound"
  (dolist (var (variables-in expr))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)

(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings"
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
	b
	(or (follow-binding (cdr b) bindings)
	    b))))

(defmacro with-undo-bindings (&body body)
  "Undo bindings after each expression in body except the last"
  (if (= 1 (length body))
      (first body)
      `(let ((old-trail (fill-pointer *trail*)))
	 ,(first body)
	 ,@(loop for expr in (rest body)
		collect '(undo-bindings! old-trail)
		colect expr))))

(defun not/1 (relation cont)
  "Negation by failure: If you can't prove G, then (not G) is true"
  (with-undo-bindings
      (call/1 relation #'(lambda () (return-from not/1)))
    (funcall cont)))

(defun bagof/3 (expr goal result cont)
  "Find all solution to `goal', and for each solution collect the value of `expr'"
  (let ((answers nil))
    (call/1 goal #'(lambda ()
		     (push (deref-copy expr) answers)))
    (if (and (not (null answers))
	     (unify! result (nreverse answers)))
	(funcall cont))))

(defun deref-copy (expr)
  "Copy the `expr', replacing variables with new ones"
  (sublis (mapcar #'(lambda (var)
		      (cons (deref var) (?)))
		  (unique-find-anywhere-if #'variable-p expr))
	  expr))

(defun setof/3 (expr goal result cont)
  "Find all unique solutions to `goal', collect the value of `expr' in each solution to `result'"
  (let ((answers nil))
    (call/1 goal #'(lambda ()
		     (push (deref-copy expr) answers)))
    (if (and (not (null answers))
	     (unify! result (delete-duplicates answers :test #'deref-equal)))
	(funcall cont))))

(defun is/2 (var expr cont)
  "Does `var' unifies to the lisp `expr'?"
  (if (and (not (find-if-anywhere #'unbound-var-p expr))
	   (unify! var (eval (deref-expr expr))))
      (funcall cont)))

(defun unbound-var-p (expr)
  (and (variable-p expr)
       (not (bound-p expr))))

(defun call/1 (goal cont)
  "Prove `goal' by calling it"
  (deref goal)
  (apply (make-predicate (first goal) (length (args goal)))
	 (append (args goal) (list cont))))
