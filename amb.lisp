(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals"
  (cond ((eq bindings fail) fail)
	((null goals) bindings)
	(t (prove (first goals) bindings (rest goals)))))

(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal"
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
	(some #'(lambda (clause)
		  (let ((new-clause (rename-variables clause)))
		    (prove-all (append (clause-body new-clause) other-goals)
			       (unify goal (clause-head new-clause) bindings))))
	      clauses)
	(funcall clauses (rest goal) bindings other-goals))))

(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals))) no-bindings)
  (format t "~&No.")
  (values))

(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its bindings. Then ask the user if more solutions are required"
  (if (null vars)
      (format t "~&Yes")
      (format t "~{~a~^, ~}" (mapcar #'(lambda (var)
					 (list var (subst-bindings bindings var)))
				     vars)))
  (if (continue-p)
      fail				; try again
      (prove-all other-goals bindings))) ; finish the rest of the computation

(setf (get-clauses 'show-prolog-vars) 'show-prolog-vars)

(defun continue-p ()
  "Ask user if we should continue looking for solutions"
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
     (format t " Type ; to see more results or . to stop")
     (continue-p))))

;; Destructive Unification
(defconstant unbound "Unbound")
(defvar *variable-counter* 0)

(defstruct (variable (:constructor ? ()) (:print-function print-variable))
  (name (incf *variable-counter*))
  (binding unbound))

(defun print-variable (variable stream depth)
  (if (or (and (numberp *print-level*)
	       (>= depth *print-level*))
	  (variable-p (deref variable)))
      (format stream "?~a" (variable-name variable))
      (write variable :stream stream)))

(defun bound-p (variable)
  (not (eq (variable-binding variable) unbound)))

(defmacro deref (expr)
  "Follow pointers for bound variables"
  `(progn (loop while (and (variable-p ,expr) (bound-p ,expr))
	       do (setf ,expr (variable-binding ,expr)))
	  ,expr))

(defun unify! (x y)
  "Destructively unify two expressions"
  (cond ((eql (deref x) (deref y)) t)
	((variable-p x) (set-binding! x y))
	((variable-p y) (set-binding! y x))
	((and (consp x) (consp y))
	 (and (unify! (first x) (first y))
	      (unify! (rest x) (rest y))))
	(t nil)))

(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))

(defun set-binding! (variable value)
  "Set variable's binding to value. Always returns t"
  (unless (eq variable value)
    (vector-push-extend variable *trail*)
    (setf (variable-binding variable) value)))

(defun undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail"
  (loop until (= (fill-pointer *trail*) old-trail)
       do (setf (variable-binding (vector-pop *trail*)) unbound)))

