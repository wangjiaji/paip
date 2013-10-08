(defun infix->prefix (infix-expr)
  (prefix->infix infix-expr))

(defun infix->prefix (expr)
  (cond ((atom expr) expr)
	((= (length expr) 1)
	 (infix->prefix (first expr)))
	((rule-based-translator
	  expr
	  *infix->prefix-rules*
	  :rule-if #'rule-pattern
	  :rule-then #'rule-response
	  :action #'(lambda (bindings response)
		      (sublis (mapcar #'(lambda (pair)
					  (cons (first pair)
						(infix->prefix (rest pair))))
				      bindings)
			      response))))
	((symbolp (first expr))
	 (list (first expr) (infix->prefix (rest expr))))
	(t (error "Illegal expr"))))

(defun variable-p (expr)
  "Variables are symbols M through Z"
  (member expr '(x y z m n o p q r s t u v w)))

(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))
(pat-match-abbrev 'n? '(?is n numberp))
(pat-match-abbrev 'm? '(?is m numberp))
(pat-match-abbrev 's? '(?is s not-numberp))

(defun not-numberp (x) (not (numberp x)))

(defun rule-pattern (rule) (first rule))
(defun rule-response (rule) (second rule))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
	  '(((x+ = y+) (= x y))
	    ((- x+) (- x))
	    ((+ x+) (+ x))
	    ((x+ + y+) (+ x y))
	    ((x+ - y+) (- x y))
	    ((x+ * y+) (* x y))
	    ((x+ / y+) (/ x y))
	    ((x+ ^ y+) (^ x y))))
  "A set of infix->prefix rules, ordered by precedence.")

(defun simp-rule (rule)
  "Transform a rule into proper format"
  (let ((expr (infix->prefix rule)))
    (mkexp (expand-pat-match-abbrev (expr-lhs expr))
	   (expr-op expr)
	   (expr-rhs expr))))

(defparameter *simplification-rules*
  (append
   (mapcar #'infix->prefix
	   '((x + 0 = x) (0 + x = x)
	     (x + x = 2 * x)
	     (x - 0 = x) (0 - x = - x)
	     (x - x = 0)
	     (- - x = x)
	     (x * 1 = x) (1 * x = x)
	     (x * 0 = 0) (0 * x = 0)
	     (x * x = x ^ 2)
	     (x / 0 = undefined) (0 / x = 0)
	     (x / 1 = x) (x / x = 1)
	     (0 ^ 0 = undefined)
	     (x ^ 0 = 1) (0 ^ x = 0)
	     (1 ^ x = 1) (x ^ 1 = x)
	     (x ^ -1 = 1 / x)
	     (x * (y / x) = y) ((y / x) * x = y)
	     ((y * x) / x = y) ((x * y) / x = y)
	     (x + - x = 0) ((- x) + x = 0)
	     (x + y - x = y)))
   (mapcar #'simp-rule
	   '((s? * n?  = n * s)
	     (n? * (m? * s?) = (n * m) * s)
	     (x * (n? * y) = n * (x * y))
	     ((n? * x) * y = n * (x * y))
	     (n? + s? = s + n)
	     ((x + m?) + n? = x + n + m)
	     (x + (y + n?) = (x + y) + n)
	     ((x + n?) + y = (x + y) + n)
	     (log 1 = 0) (log 0 = undefined) (log e = 1)
	     (sin 0 = 0) (sin pi = 0)
	     (cos 0 = 1) (cos pi = -1)
	     (sin(pi / 2) = 1) (cos(pi / 2) = 0)
	     (log (e ^ x) = x) (e ^ (log x) = x)
	     ((x ^ y) * (x ^ z) = x ^ (y + z))
	     ((x ^ y) / (x ^ z) = x ^ (y - x))
	     (log x + log y = log(x * y))
	     (log x - log y = log(x / y))
	     ((sin x) ^ 2 + (cos x) ^ 2 = 1))))

(defun ^ (x y) "Exponentiation" (expt x y))

(defun simplifier ()
  "Read a mathematical expression, simplify it, print the result."
  (loop (print 'simplifier>)
     (print (simp (read)))))

(defun simp (inf-expr)
  (prefix->infix (simplify (infix->prefix inf-expr))))

(defun simplify (expr)
  "Simplify an expression by first simplifying its components"
  (dbg :simplify "~&;; Simplifying ~a" expr)
  (if (atom expr)
      expr
      (simplify-expr (mapcar #'simplify expr))))

(defun simplify-expr (expr)
  "Simplify using a rule, or by doing arithmatic"
  (cond ((rule-based-translator
	  expr
	  *simplification-rules*
	  :rule-if #'expr-lhs
	  :rule-then #'expr-rhs
	  :action #'(lambda (bindings response)
		      (simplify (sublis bindings response)))))
	((evaluable expr) (eval expr))
	(t expr)))

(defun evaluable (expr)
  (and (every #'numberp (expr-args expr))
       (or (member (expr-op expr) '(+ - * /))
	   (and (eq (expr-op expr) '^)
		(integerp (second (expr-args expr)))))))

