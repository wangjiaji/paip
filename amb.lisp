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
			       (unify goal (clause-head clause) bindings))))
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
      (dolist (var vars)
	(format t "~&~a = ~a" var (subst-bindings bindings var))))
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
