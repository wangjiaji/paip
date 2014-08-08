(defun clause-head (clause)
  (first clause))

(defun clause-body (clause)
  (rest clause))

(defun get-clauses (pred)
  (get pred 'clauses))

(defun (setf get-clauses) (clauses pred)
  (setf (get pred 'clauses) clauses))

(defun predicate (relation)
  (first relation))

(defvar *db-predicates* nil
  "A list of all predicates stored in the database")

(defvar *uncompiled* nil
  "Prolog symbols that have not been compiled")

(defmacro <- (&rest clause)
  "Add a clause to the database"
  `(add-clause ',(make-anonymous clause)))

(defun add-clause (clause)
  "Add a clause to the database, indexed by head's predicate"
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)
    (setf (get-clauses pred)
	  (nconc (get-clauses pred) (list clause)))
    pred))

(defun clear-db ()
  "Remove all clauses (for all predicates) from the database"
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clauses for a single predicate"
  (setf (get predicate 'clauses) nil))

(defun prove (goal bindings)
  "Return a list of possible solutions to goal"
  (mapcan #'(lambda (clause)
	      (let ((new-clause (rename-variables clause)))
		(prove-all (clause-body new-clause)
			   (unify goal (clause-head new-clause) bindings))))
	  (get-clauses (predicate goal))))

(defun prove-all (goals bindings)
  "Return a list of solutions to the conjunction of goals"
  (cond ((eq bindings fail) fail)
	((null goals) (list bindings))
	(t (mapcan #'(lambda (goal-solution)
		       (prove-all (rest goals) goal-solution))
		   (prove (first goals) bindings)))))

(defun rename-variables (x)
  "Replace all variables in x with new ones"
  (sublis (mapcar #'(lambda (var)
		      (cons var (gensym (string var))))
		  (variables-in x))
	  x))

(defun variables-in (expr)
  "Return a list of all the variables in expr"
  (unique-find-anywhere-if #'variable-p expr))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate, with duplicates removed"
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if predicate
			       (first tree)
			       (unique-find-anywhere-if predicate
							(rest tree)
							found-so-far))))

(defmacro ?- (&rest goals)
  `(top-level-prove ',(replace-?-vars goals)))

(defun top-level-prove (goals)
  "Prove the list of goals by compiling and calling it"
  ;; Redefine top-level query
  (clear-predicate 'top-level-query)
  (let ((vars (delete '? (variables-in goals))))
    (add-clause `((top-level-query)
		  ,@goals
		  (show-prolog-vars ,(mapcar #'symbol-name vars)
				    ,vars))))
  ;; Run the code
  (run-prolog 'top-level-query/0 #'ignore)
  (format t "~&No.")
  (values))

(defun run-prolog (procedure cont)
  "Run a 0 arity prolog procedure with a given continuation"
  (prolog-compile-symbols)		; compile everything
  (setf (fill-pointer *trail*) 0)	; reset backtracking trail
  (setf *var-counter* 0)
  (catch 'top-level-prove
    (funcall procedure cont)))

(defun prolog-compile-symbol (&optional (symbols *uncompiled*))
  (mapc #'prolog-compile symbols)
  (setf *uncompiled* (set-difference *uncompiled* symbols)))

(defun ignore (&rest args)
  "Dummy function"
  (declare (ignore args))
  nil)
  
(defun show-prolog-solutions (vars solutions)
  "Print the variables in each of the solutions"
  (if (null solutions)
      (format t "~&No.")
      (mapc #'(lambda (solution)
		(show-prolog-vars vars solution))
	    solutions))
  (values))				; No return value

(defun show-prolog-vars (vars bindings)
  "Print each variable with its bindings"
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
	(format t "~&~a = ~a" var (subst-bindings bindings var))))
  (princ ";"))

(defun replace-?-vars (expr)
  "Replace any ? within expr with a var of the form ?123"
  (cond ((eq expr '?) (gensym "?"))
	((atom expr) expr)
	(t (cons (replace-?-vars (first expr))
		 (replace-?-vars (rest expr))))))

(clear-db)
(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))
