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
(defvar *var-counter* 0)

(defstruct (var (:constructor ? ()) (:print-function print-var))
  (name (incf *var-counter*))
  (binding unbound))

(defun print-var (var stream depth)
  (if (or (and (numberp *print-level*)
	       (>= depth *print-level*))
	  (var-p (deref var)))
      (format stream "?~a" (var-name var))
      (write var :stream stream)))

(defun bound-p (var)
  (not (eq (var-binding var) unbound)))

(defmacro deref (expr)
  "Follow pointers for bound variables"
  `(progn (loop while (and (var-p ,expr) (bound-p ,expr))
	       do (setf ,expr (var-binding ,expr)))
	  ,expr))

(defun unify! (x y)
  "Destructively unify two expressions"
  (cond ((eql (deref x) (deref y)) t)
	((var-p x) (set-binding! x y))
	((var-p y) (set-binding! y x))
	((and (consp x) (consp y))
	 (and (unify! (first x) (first y))
	      (unify! (rest x) (rest y))))
	(t nil)))

(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))

(defun set-binding! (var value)
  "Set var's binding to value. Always returns t"
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value)))

(defun undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail"
  (loop until (= (fill-pointer *trail*) old-trail)
       do (setf (var-binding (vector-pop *trail*)) unbound)))

