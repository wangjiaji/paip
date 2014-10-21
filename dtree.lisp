(defun make-empty-nlist ()
  (cons 0 nil))

(defun nlist-n (x)
  "Length of an nlist"
  (car x))

(defun nlist-list (x)
  "Content of an nlist"
  (cdr x))

(defun nlish-push (item nlist)
  (incf (car nlist))
  (push item (nlist-list nlist))
  nlist)

(defstruct (dtree (:type vector))
  (first nil) (rest nil) (atoms nil) (unknown (make-empty-nlist)))

(let ((predicates nil))
  (defun get-dtree (predicate)
    "Fetch (or make) the dtree for this predicate"
    (cond ((get predicate 'dtree))
	  (t (push predicate predicates)
	     (setf (get predicate 'dtree) (make-dtree)))))

  (defun clear-dtree ()
    "Remove all predicates' dtrees"
    (dolist (predicate predicates)
      (setf (get predicate 'dtree) nil))
    (setf predicates nil)))

(defun index (key)
  "Store key in a dtree node. Key must be (predicate . args)"
  (dtree-index key key (get-dtree (first key))))

(defun dtree-index (key value dtree)
  "Index value under all atoms of key in dtree"
  (cond ((consp key)
	 (dtree-index (first key)
		      value
		      (or (dtree-first dtree)
			  (setf (dtree-first dtree) (make-dtree))))
	 (dtree-index (rest key)
		      value
		      (or (dtree-rest dtree)
			  (setf (dtree-rest dtree) (make-dtree)))))
	((null key))
	((variable-p key)
	 (nlist-push value (dtree-unknown dtree)))
	(t (nlist-push value (lookup-atom key dtree)))))

(defun lookup-atom (atom dtree)
  (or (lookup atom (dtree-atoms dtree))
      (let ((new (make-empty-nlist)))
	(push (cons atom new) (dtree-atoms dtree))
	new)))

(defun test-index ()
  (let ((props '((p a b) (p a c) (p a ?x) (p b c) (p b (f c)) (p a (f .?x)))))
    (clear-dtree)
    (mapc #'index props)
    (write (list props (get-dtree 'p))
	   :circle t :array t :pretty t)
    (values)))

(defun fetch (query)
  "Return a list of buckets potentially matching `query', which must be a relation of form (predicate .args)"
  (dtree-fetch query (get-dtree (predicate query)) nil 0 nil most-positive-fixnum))

(defun dtree-fetch (pat dtree var-list-in var-n-in best-list best-n)
  "Return 2 values: a list of lists of possible matches to pat, and the number of elements in the list of lists"
  (if (or (null dtree) (null pat) (variable-p pat)
      (values best-list best-n)
      (let* ((var-nlist (dtree-unknown dtree))
	     (var-n (+ var-n-in (nlist-n var-nlist)))
	     (var-list (if (null (nlist-list var-nlist))
			   var-list-in
			   (cons (nlist-list var-nlist)
				 var-list-in))))
	(cond ((>= var-n best-n) (values best-list best-n)) ; Stop if answers are increasing by going down the list
	      ((atom pat) (dtree-atom-fetch pat dtree var-list var-n best-list best-n))
	      (t (multiple-value-bind (list1 n1)
		     (dtree-fetch (first pat) (dtree-first dtree) var-list var-n best-list best-n)
		   (dtree-fetch (rest pat) (dtree-rest dtree) var-list var-n list1 n1)))))))

(defun dtree-atom-fetch (atom dtree var-list var-n best-list best-n)
  "Return the answers indexed at this atom (along with the vars), or return the previous best answer"
  (let ((atom-nlist (lookup atom (dtree-atoms dtree))))
    (cond ((or (null atom-nlist) (null (nlist-list atom-nlist)))
	   (values var-list var-n))
	  ((and atom-nlist (< (incf var-n (nlist-n atom-nlist)) best-n))
	   (values (cons (nlist-list atom-nlist) var-list) var-n))
	  (t (values best-list best-n)))))

(proclaim '(inline mapc-retrieve))

(defun mapc-retrieve (fn query)
  "For every fact that matches `query', apply `fn' to the binding list"
  (dolist (bucket (fetch query))
    (dolist (answer bucket)
      (let ((bindings (unify query answer)))
	(unless (eq bindings fail)
	  (funcall fn bindings))))))

(defun retrieve (query)
  "Find all facts that match `query'. Return a list of bindings"
  (let ((answers nil))
    (mapc-retrieve #'(lambda (bindings)
		       (push bindings answers))
		   query)
    answers))

(defun retrieve-matches (query)
  "Find all facts that match `query'. Return a list of expressions that match the query"
  (mapcar #'(lambda (bindings)
	      (subst-bindings bindings query))
	  (retrive query)))

(defmacro query-bind (variables query &body body)
  "Execute the body for each match to the query"
  (let* ((bindings (gensym "BINDINGS"))
	 (vars-and-vals (mapcar #'(lambda (var)
				    (list var `(subst-bindings ,bindings ',var)))
				variables)))
    `(mapc-retrieve #'(lambda (,bindings)
			(let ,vars-and-vars
			  ,@body))
		    ,query)))
