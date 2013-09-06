(defstruct  (rule (:type list)) pattern response)

(defstruct (expr (:type list)
		(:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun expr-p (x) (consp x))
(defun expr-args (x) (rest x))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

(defparameter *student-rules*
  (mapcar #'expand-pat-match-abbrev
	  '(((?x* |.|) ?x)
	    ((?x* |.| ?y*) (?x ?y))
	    ((if ?x* |,| then ?y*) (?x ?y))
	    ((if ?x* then ?y*) (?x ?y))
	    ((if ?x* |,| ?y*) (?x ?y))
	    ((?x* |,| and ?y*) (?x ?y))
	    ((find ?x* and ?y*) ((= to-find-1 ?x) (= to-find-2 ?y)))
	    ((find ?x*) (= to-find ?x))
	    ((?x* equals ?y*) (= ?x ?y))
	    ((?x* same as ?y*) (= ?x ?y))
	    ((?x* = ?y*) (= ?x ?y))
	    ((?x* is equal to ?y*) (= ?x ?y))
	    ((?x* is ?y*) (= ?x ?y))
	    ((?x* - ?y*) (- ?x ?y))
	    ((?x* minus ?y*) (- ?x ?y))
	    ((difference between ?x* and ?y*) (- ?y ?x))
	    ((difference ?x* and ?y*) (- ?y ?x))
	    ((?x* + ?y*) (+ ?x ?y))
	    ((?x* plus ?y*) (+ ?x ?y))
	    ((sum ?x* and ?y*) (+ ?x ?y))
	    ((product ?x* and ?y*) (* ?x ?y))
	    ((?x* * ?y*) (* ?x ?y))
	    ((?x* times ?y*) (* ?x ?y))
	    ((?x* / ?y*) (/ ?x ?y))
	    ((?x* per ?y*) (/ ?x ?y))
	    ((?x* divided by ?y*) (/ ?x ?y))
	    ((half ?x*) (/ ?x 2))
	    ((one half ?x*) (/ ?x 2))
	    ((twice ?x*) (* 2 ?x))
	    ((square ?x*) (* ?x ?x))
	    ((?x* % less than ?y*) (* ?y (/ (- 100 ?x) 100)))
	    ((?x* % more than ?y*) (* ?y (/ (+ 100 ?x) 100)))
	    ((?x* % ?y*) (* (/ ?x 100) ?y)))))

(defun student (words)
  "Solve algebra word prolems"
  (solve-equations
   (create-list-of-equations
    (translate-to-expression (remove-if #'noise-word-p words)))))

(defun translate-to-expression (words)
  "Translate words to equations or expressions"
  (or (rule-based-translator words
			     *student-rules*
			     :rule-if #'rule-pattern
			     :rule-then #'rule-response
			     :action #'(lambda (bindings response)
					 (print bindings)
					 (sublis (mapcar #'translate-pair bindings)
						 response)))
      (make-variable words)))

(defun translate-pair (pair)
  "Translate the value part of the pair into an equation or expression"
  (cons (binding-var pair)
	(translate-to-expression (binding-val pair))))

(defun create-list-of-equations (expr)
  "Seperate out equations embedded in nested parenthesis"
  (cond ((null expr) nil)
	((atom (first expr)) (list expr))
	(t (append (create-list-of-equations (first expr))
		   (create-list-of-equations (rest expr))))))

(defun make-variable (words)
  "Create a variable name based on the given list of words"
  (first words))

(defun noise-word-p (word)
  (member word '(a an the this number of $)))

(defun solve-equations (equations)
  "Print the equations and their solution"
  (print-equations "The equations to be solved are: " equations)
  (print-equations "The solution is: " (solve equations nil)))

(defun solve (equations known)
  "Solve a set of equations by constraint propagation"
  (or (some #'(lambda (equation)
		(let ((x (one-unknown equation)))
		  (when x
		    (let ((answer (solve-arithmatic (isolate equation x))))
		      (solve (subst (expr-rhs answer) (expr-lhs answer)
				    (remove equation equations))
			     (cons answer known))))))
	    equations)
      (when (> (length equations) 1)
	(solve (reduce-unknowns equations) known))
      known))

(defun isolate (e x)
  "Isolate x on the left side of e"
  (cond ((eq (expr-lhs e) x) e)		; Nothing to do
	((in-expr x (expr-rhs e))	; x is on the rhs of e
	 (isolate (mkexp (expr-rhs e)
			 '=
			 (expr-lhs e))
		  x))			; Put x on the left, try again
	((in-expr x (expr-lhs (expr-lhs e)))
	 (isolate (mkexp (expr-lhs (expr-lhs e))
			 '=
			 (mkexp (expr-rhs e)
				(inverse-op (expr-op (expr-lhs e)))
				(expr-rhs (expr-lhs e))))
		  x))
	((commutative-p (expr-op (expr-lhs e)))
	 (isolate (mkexp (expr-rhs (expr-lhs e))
			 '=
			 (mkexp (expr-rhs e)
				(inverse-op (expr-op (expr-lhs e)))
				(expr-lhs (expr-lhs e))))
		  x))
	(t (isolate (mkexp (expr-rhs (expr-lhs e))
			   '=
			   (mkexp (expr-lhs (expr-lhs e))
				  (expr-op (expr-lhs e))
				  (expr-rhs e)))
		    x))))

(defun print-equations (header equations)
  (format t "~%~a~{~% ~{ ~a~}~}~%" header (mapcar #'prefix->infix equations)))

(defconstant operators-and-inverse
  '((+ -) (- +) (* /) (/ *) (= =)))

(defun inverse-op (op)
  (second (assoc op operators-and-inverse)))

(defun unknown-p (x)
  (symbolp x))

(defun in-expr (x expr)
  "True if x appears anywhere in expr"
  (or (eq x expr)
      (and (expr-p expr)
	   (or (in-expr x (expr-lhs expr))
	       (in-expr x (expr-rhs expr))))))

(defun no-unknown (expr)
  "Returns true if there are no unknowns in expr"
  (cond ((unknown-p expr) nil)
	((atom expr) t)
	((no-unknown (expr-lhs expr))
	 (no-unknown (expr-rhs expr)))
	(t nil)))

(defun one-unknown (expr)
  "True if there is exactly one unknown in expr"
  (cond ((unknown-p expr) expr)
	((atom expr) nil)
	((no-unknown (expr-lhs expr))
	 (one-unknown (expr-rhs expr)))
	((no-unknown (expr-rhs expr))
	 (one-unknown (expr-lhs expr)))
	(t nil)))

(defun commutative-p (op)
  (member op '(+ * =)))

(defun solve-arithmatic (equation)
  "Do the arithmetic for the rhs, return the whole equation"
  (mkexp (expr-lhs equation) '= (eval (expr-rhs equation))))

(defun binary-expr-p (x)
  (and (expr-p x) (= (length (expr-args x)) 2)))

(defun prefix->infix (expr)
  (if (atom expr)
      expr
      (mapcar #'prefix->infix
	      (if (binary-expr-p expr)
		  (list (expr-lhs expr) (expr-op expr) (expr-rhs expr))
		  expr))))

