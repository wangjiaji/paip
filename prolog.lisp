(defparameter *primitives* '(and sub ind rel val))

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
  (run-prolog 'top-level-query/0 #'dummy-ignore)
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

(defun dummy-ignore (&rest args)
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

(defvar *search-cut-off* nil
  "Has the search been stopped?")

(defun prove-all (goals bindings depth)
  (cond ((eq bindings fail) fail)
	((null goals) bindings)
	(t (prove (first goals) bindings (rest goals) depth))))

(defun prove (goal bindings other-goals depth)
  (if (zerop depth)
      (progn (setf *search-cut-off* t)
	     fail)
      (let ((clauses (get-clauses (predicate goal))))
	(if (listp clauses)
	    (some #'(lambda (clause)
		      (let ((new-clause (rename-variables clause)))
			(prove-all (append (clause-body new-clause) other-goals)
				   (unify goal (clause-head new-clause) bindings)
				   (1- depth))))
		  clauses)
	    (funcall clauses (rest goal) bindings other-goals depth)))))

(defparameter *depth-start* 5
  "Depth of the first round")
(defparameter *depth-incr* 5
  "Amount of incrementation after each iteration")
(defparameter *depth-max* most-positive-fixnum
  "Deepest depth")

(defun top-level-prove (goals)
  (let ((all-goals `(,@goals (show-prolog-vars ,@(variables-in goals)))))
    (loop for depth from *depth-start* to *depth-max* by *depth-incr*
	 while (let ((*search-cut-off* nil))
		 (prove-all all-goals no-bindings depth)
		 *search-cut-off*)))
  (format t "~&No")
  (values))

(defun show-prolog-vars (vars bindings other-goals depth)
  (if (> depth *depth-incr*)
      fail
      (progn
	(if (null vars)
	    (format t "~&Yes")
	    (dolist (var vars)
	      (format t "~&~a = ~a" var (subst-bindings bindings var))))
	(if (continue-p)
	    fail
	    (prove-all other-goals bindings depth)))))

(defun add-fact (fact)
  "Add fact to database"
  (cond ((eq (predicate fact) 'and)
	 (mapc #'add-fact (args fact)))
	((or (not (every #'atom (args fact)))
	     (some #'variable-p (args fact))
	     (not (member (predicate fact) *primitives*)))
	 (error "ill formed fact: ~a" fact))
	((not (fact-present-p fact))
	 (index fact)
	 (run-attached-fn fact)))
  t)


(defun fact-present-p (fact)
  "Is `fact' in database?"
  (retrieve fact))

(defun run-attached-fn (fact)
  "Run the function associated with `fact'"
  (apply (get (predicate fact) 'attached-fn) (args fact)))

(defmacro def-attached-fn (pred args &body body)
  "Define the attached function for a premitive"
  `(setf (get ',pred 'attached-fn)
	 #'(lambda ,args ,@body)))

(def-attached-fn ind (individual category)
  (query-bind (?super) `(sub ,category ?super)
	      (add-fact `(ind ,individual ,?super))))

(def-attached-fn val (relation ind1 ind2)
  (query-bind (?cat1 ?cat2) `(rel ,relation ?cat1 ?cat2)
	      (add-fact `(ind ,ind1 ,cat1))
	      (add-fact `(ind ,ind2 ,cat2))))

(def-attached-fn rel (relation cat1 cat2)
  (query-bind (?a ?b) `(ind ,relation ?a ?b)
	      (run-attached-fn `(ind ,relation ,?a ,?b))))

(def-attached-fn sub (subcat supercat)
  (query-bind (?super-super) `(sub ,supercat ?super-super)
	      (index-new-fact `(sub ,subcat ,?super-super))
	      (query-bind (?sub-sub) `(sub ?sub-sub ,?super-super)
			  (index-new-fact `(sub ,?sub-sub ,subcat))))
  (query-bind (?sub-sub) `(sub ?sub-sub ,subcat)
	      (index-new-fact `(sub ,?sub-sub ,supercat)))
  (query-bind (?super-super) `(sub ,subcat ?super-super)
	      (query-bind (?sub-sub) `(sub ?sub-sub ,supercat)
			  (query-bind (?ind) `(ind ?ind ,?sub-sub)
				      (index-new-fact `(ind ,?ind ,?super-super))))))

(defun index-new-fact (fact)
  (unless (fact-present-p fact)
    (index fact)))

(defun retrieve-fact (query &optional (bindings no-bindings))
  "Find all facts matching query. Return a list of bindings"
  (if (eq (predicate query) 'and)
      (retrieve-conjunction (args query) (list bindings))
      (retrieve query bindings)))

(defun retrieve-conjunction (conjuncts bindings-lists)
  (mapcan #'(lambda (bindings)
	      (cond ((eq bindings fail) nil)
		    ((null conjuncts) bindings)
		    (t (retrieve-conjunction (rest conjuncts)
					     (retrieve-fact (subst-bindings bindings (first conjuncts))
							    bindings)))))
	  bindings-lists))

(defun mapc-retrieve (fn query &optional (bindings no-bindings))
  "Apply `fn' to every list of bindings matching the query"
  (dolist (bucket (fetch query))
    (dolist (answer bucket)
      (let ((new-bindings (unify query answer bindings)))
	(unless (eq new-bindings fail)
	  (funcall fn new-bindings))))))

(defun retrieve (query &optional (bindings no-bindings))
  (let ((answers nil))
    (mapc-retrieve #'(lambda (bindings)
		       (push bindings answers))
		   query
		   bindings)
    answers))

(defun retrieve-bagof (query)
  "Find all facts that match `query'. Return a list of queries with bindings filled in"
  (mapcar #'(lambda (bindings)
	      (subst-bindings bindings query))
	  (retrieve-fact query)))

(defun retrieve-setof (query)
  "Find all facts that match `query'. Return a list of unique queries with bindings filled in"
  (remove-duplicates (retrieve-bagof query) :test #'equal))

(clear-db)
(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))

(defmacro a (&rest args)
  "Define a new individual and assert facts about it in the database"
  `(add-fact ',(translate-expr (cons 'a args))))

(defmacro each (&rest args)
  "Define a new category and assert facts about it in the database"
  `(add-fact ',(translate-expr (cons 'each args))))

(defmacro ?? (&rest queries)
  "Return a list of answers satisfying `queries'"
  `(retrieve-setof ',(translate-expr (maybe-add 'and (replace-?-vars queries))
				     :query)))

(defun translate-expr (expr &optional query-mode-p)
  "Translate `expr' into a conjunction of the four primitives"
  (let ((conjuncts nil))
    (labels
	((collect-fact (&rest terms)
	   (push terms conjuncts))
	 (translate (expr)
	   ;; Figure out what kind of expression this is
	   (cond ((atom expr) expr)
		 ((eq (first expr) 'a)
		  (translate-a (rest expr)))
		 ((eq (first expr) 'each)
		  (translate-each (rest expr)))
		 (t (apply #'collect-fact expr)
		    expr)))
	 (translate-a (args)
	   ;; translate (a category [ind] (rel filler)*)
	   (let* ((category (pop args))
		  (self (cond ((and args (atom (first args)))
			       (pop args))
			      (query-mode-p
			       (gentemp "?"))
			      (t (gentemp (string category))))))
	     (collect-fact 'ind self category)
	     (dolist (slot args)
	       (translate-slot 'val self slot))
	     self))
	 (translate-each (args)
	   (let* ((category (pop args)))
	     (when (eq (predicate (first args)) 'isa)
	       (dolist (super (rest (pop args)))
		 (collect-fact 'sub category super)))
	     (dolist  (slot args)
	       (translate-slot 'rel category slot))
	     category))
	 (translate-slot (primitive self slot)
	   (assert (= (length slot) 2))
	   (collect-fact primitive (first slot) self (translate (second slot)))))
      (translate expr)
      (maybe-add 'and (nreverse conjuncts)))))

(defun maybe-add (op exprs &optional if-nil)
  "Make a conjunction of `exprs' when necessary"
  (cond ((null exprs) if-nil)
	((= 1 (length exprs))
	 (first exprs))
	(t (cons op exprs))))

(defun replace-?-vars (expr)
  "Replace each ? in expr with a temporary var"
  (cond ((eq expr '?) (gentemp "?"))
	((atom expr) expr)
	(t (cons (replace-?-vars (first expr))
		 (replace-?-vars (rest expr))))))
