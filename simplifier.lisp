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
	    ((d y+ / d x) (d y x))
	    ((Int y+ d x) (int y x))
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
	     ((sin x) ^ 2 + (cos x) ^ 2 = 1)
	     (d x / d x = 1)
	     (d (u + v) / d x = (d u / d x) + (d v / d x))
	     (d (u - v) / d x = (d u / d x) - (d v / d x))
	     (d (-u) / d x = - (d u / d x))
	     (d (u * v) / d x = u * (d v / d x) + v * (d u / d x))
	     (d (u / v) / d x = (v * (d u / d x) - u * (d v / d x)) / v ^ 2)
	     (d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))
	     (d (u ^ v) / d x = v * u ^ (v - 1) * (d u / d x) + u ^ v * (log u) * (d u / d v))
	     (d (log u) / d x = (d u / d x) / u)
	     (d (sin u) / d x = (cos u) * (d u / d x))
	     (d (cos u) / d x = - (sin u) * (d u / d x))
	     (d (e ^ u) / d x = (e ^ u) * (d u / d x))
	     (d u / d x = 0)))))

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
  (cond ((simplify-by-fn exp))
	(rule-based-translator
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

(defun simp-fn (op)
  (get op 'simp-fn))

(defun set-simp-fn (op fn)
  (setf (simp-fn op) fn))

(defun simplify-by-fn (expr)
  "Simplify with the simplification function for expr if it has one"
  (let* ((fn (simp-fn (expr-op expr)))
	 (result (if fn (funcall fn expr))))
    (if result
	(simplify result))))

(defun factorize (expr)
  "Return a list of the factors of expr"
  (let ((factors '())
	(constant 1))
    (labels
	((fac (x n)
	   (cond ((numberp x)
		  (setf constant (* constant (expt x n))))
		 ((starts-with x '*)
		  (fac (expr-lhs x) n)
		  (fac (expr-rhs x) n))
		 ((starts-with x '/)
		  (fac (expr-lhs x) n)
		  (fac (expr-rhs x) (- n)))
		 ((and (starts-with x '-) (= 1 (length (expr-args x))))
		  (setf constant (- constant))
		  (fac (expr-lhs x) n))
		 ((and (starts-with x '^) (numberp (expr-rhs x)))
		  (fac (expr-lhs x) (* n (expr-rhs x))))
		 (t (let ((factor (find x factors :key #'expr-lhs :test #'equal)))
		      (if factor
			  (incf (expr-rhs factor) n)
			  (push `(^ ,x ,n) factors)))))))
      (fac expr 1)
      (case constant
	(0 '((^ 0 1)))
	(1 factors)
	(t `((^ ,constant 1) . ,factors))))))

(defun unfactorize (factors)
  (cond ((null factors) 1)
	((= 1 (length factors)) factors)
	(t `(* ,(first factors) ,(unfactorize (rest factors))))))

(defun divide-factors (numer denom)
  "Divide a list of factors by another, producing a third"
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor (find (expr-lhs d) result :key #'expr-lhs :test #'equal)))
	(if factor
	    (decf (expr-rhs factor) (expr-rhs d))
	    (push `(^ ,(expr-lhs d) ,(- (expr-rhs d))) result))))
    (delete 0 result :key #'expr-rhs)))

(defun integrate (expr x)
  (cond ((free-of expr x)
	 `(* ,expr x))
	((starts-with expr '+)
	 `(+ ,(integrate (expr-lhs expr) x) ,(integrate (expr-rhs expr) x)))
	((starts-with expr '-)
	 (ecase (length (expr-args expr))
	   (1 (integrate (expr-lhs expr) x))
	   (2 `(- ,(integrate (expr-lhs expr) x) ,(integrate (expr-rhs expr) x)))))
	((multiple-value-bind (const-factors x-factors)
	     (partition-if #'(lambda (factor)
			       (free-of factor x))
			   (factorize expr))
	   (simplify
	    `(* ,(unfactorize const-factors)
		,(cond ((null x-factors) x)
		       ((some #'(lambda (factor)
				  (deriv-divides factor x-factors x))
			      x-factors))
		       (t `(int? ,(unfactorize x-factors) ,x)))))))))

(defun partition-if (pred seq)
  "Return 2 lists, elemets that satisfy pred, and those that don't"
  (let ((yes-list ())
	(no-list ()))
    (dolist (elt alist)
      (if (funcall pred elt)
	  (push elt yes-list)
	  (push elt no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun deriv-divides (factor x-factors x)
  (assert (starts-with factor '^))
  (let* ((u (expr-lhs factor))
	 (n (expr-rhs factor))
	 (k (divide-factors factors (factorize `(* ,factor ,(deriv u x))))))
    (cond ((free-of k x)
	   (if (= n -1)
	       `(* ,(unfactorize k) (log ,u))
	       `(/ (* ,(unfactorize k) (^ ,u ,(1+ n))) ,(1+ n))))
	  ((and (= n 1) (in-integral-table? u))
	   (let ((k2 (divide-factors factors (factorize `(* ,u ,(deriv (expr-lhs u) x))))))
	     (if (free-of k2 x)
		 `(* ,(integrate-from-table (expr-op u) (expr-lhs u)) ,(unfactorize k2))))))))

(defun deriv (y x)
  (simplify `(d ,y ,x)))

(defun integration-table (rules)
  (dolist (i-rule rules)
    (let ((rule (infix->prefix i-rule)))
      (setf (get (expr-op (expr-lhs (expr-lhs rule))) 'int) rule))))

(defun in-integral-table? (expr)
  (and (expr-p expr) (get (expr-op expr) 'int)))

(defun integrate-from-table (op arg)
  (let ((rule (get op 'int)))
    (subst arg (expr-lhs (expr-lhs (expr-lhs rule))) (expr-rhs rule))))

(integration-table
 '((Int log(x) d x = x * log(x) - x)
   (Int exp(x) d x = exp(x))
   (Int sin(x) d x = - cos(x))
   (Int cos(x) d x = sin(x))
   (Int tan(x) d x = - log(cos(x)))
   (Int sinh(x) d x = cosh(x))
   (Int cosh(x) d x = sinh(x))
   (Int tanh(x) d x = log(cosh(x)))))

(set-simp-fn 'Int #'(lambda (expr)
		      (integrate (expr-lhs expr) (expr-rhs expr))))
