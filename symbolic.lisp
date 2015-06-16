(proclaim '(inline main-var degree coef var= var> poly make-poly))

(deftype polynomial () 'simple-vector)

(defun main-var (p)
  (svref (the polynomial p) 0))

(defun coef (p i)
  (svref (the polynomial p) (1+ i)))

(defun degree (p)
  (- (length (the polynomial p)) 2))

(defun poly (x &rest coefs)
  "Make a polynomial with main variable `x' and `coeffs' in increasing order"
  (apply #'vector x coefs))

(defun make-poly (x degree)
  "Make a polynomial with all zero coeffs and `degree' degree"
  (let ((p (make-array (+ degree 2) :initial-element 0)))
    (setf (main-var p) x)
    p))

(defsetf main-var (p) (val)
  `(setf (svref (the polynomial ,p) 0) ,val))

(defsetf coef (p i) (val)
  `(setf (svref (the polynomial ,p) (1+ i)) ,val))

(defun prefix->canon (x)
  "Change (+ (^ x 2) (* x 3)) to #(x 0 3 1)"
  (cond ((numberp x) x)
	((symbolp x) (poly x 0 1))
	((and (expr-p x) (get (expr-op x) 'prefix->canon))
	 (apply (get (expr-op x) 'prefix->canon)
		(mapcar #'prefix->canon (expr-args x))))
	(t (error "Not a polynomial: ~a" x))))

(dolist (item '((+ poly+) (- poly-) (* poly*poly)
		(^ poly^n) (D deriv-poly)))
  (setf (get (first item) 'prefix->canon) (second item)))

(defun poly+ (&rest args)
  "Unary or binary polynomial addition"
  (ecase (length args)
    (1 (first args))
    (2 (poly+poly (first args) (second args)))))

(defun poly- (&rest args)
  "Unary or binary polynomial subtraction"
  (ecase (length args)
    (1 (poly*poly -1 (first args)))
    (2 (poly*poly (first args) (poly*poly -1 (second args))))))

(defun poly+poly (p q)
  "Add two polynomials"
  (normalize-poly
   (cond ((numberp p) (k+poly p q))
	 ((numberp q) (k+poly q p))
	 ((var= (main-var p) (main-var q))
	  (poly+same p q))
	 ((var> (main-var q) (main-var p))
	  (k+poly q p))
	 (t (k+poly p q)))))

(defun k+poly (k p)
  "Add a constant `k' to polynomial `p'"
  (cond ((eql k 0) p)
	((and (numberp k) (numberp p))
	 (+ k p))
	(t (let ((r (copy-poly p)))
	     (setf (coef r 0) (poly+poly (coef r 0) k))
	     p))))

(defun poly+same (p q)
  "Add two polynomials with the same main variable"
  (if (> (degree p) (degree q))
      (poly+same q p)
      (let ((r (copy-poly q)))
	(loop for i from 0 to (degree p) do
	     (setf (coef r i) (poly+poly (coef r i) (coef p i))))
	r)))

(defun copy-poly (p)
  (copy-seq p))

(defun poly*poly (p q)
  "Multiply two polynomials"
  (normalize-poly
   (cond ((numberp p) (k*poly p q))
	 ((numberp q) (k*poly q p))
	 ((var= (main-var p) (main-var q))
	  (poly*same p q))
	 ((var> (main-var p) (main-var q))
	  (k*poly q p))
	 (t (k*poly p q)))))

(defun k*poly (k p)
  "Multiply a polynomial by a constant"
  (cond ((eql k 0) 0)
	((eql k 1) p)
	((and (numberp k) (numberp p))
	 (* k p))
	(t (let ((r (make-poly (main-var p) (degree p))))
	     (loop for i from 0 to (degree p) do
		  (setf (coef r i) (poly*poly k (coef p i))))
	     r))))

(defun poly*same (p q)
  "Multiply two polynomials with the same variable"
  (let* ((r-degree (+ (degree p) (degree q)))
	 (r (make-poly (main-var p) r-degree)))
    (loop for i from 0 to (degree p) do
	 (unless (eql (coef p i) 0)
	   (loop (for j from 0 to (degree q) do
		      (setf (coef r (+ i j))
			    (poly+poly (coef r (+ i j))
				       (poly*poly (coef p i)
						  (coef q j))))))))
    r))

(defun normalize-poly (p)
  "Alter a polynomial by dropping trailing zeros"
  (if (numberp p)
      p
      (let ((p-degree (1- (position 0 p :test-not #'eql :from-end t))))
	(cond ((<= p-degree 0)
	       (normalize-poly (coef p 0)))
	      ((< p-degree (degree p))
	       (delete 0 p :start p-degree))
	      (t p)))))

(defun poly^n (p n)
  "Raise polynomial to the nth order"
  (check-type n unsigned-byte)
  (cond ((zerop n)
	 (assert (not (eql p 0)))
	 1)
	((integerp p)
	 (expt p n))
	(t (poly*poly p (poly^n p (1- n))))))

(defun deriv-poly (p x)
  "Differentiate p with respect to x"
  (assert (and (typep x 'polynomial)
	       (= (degree x) 1)
	       (zerop (coef x 0))
	       (eql 1 (coef x 1))))
  (cond ((numberp p) 0)
	((var> (main-var p) (main-var x)) 0)
	((var= (main-var p) (main-var x))
	 (let ((r (subseq p 1)))
	   (setf (main-var r) (main-var x))
	   (loop for i from 1 to (degree r) do
		(setf (coef r i) (poly*poly (1+ i) (coef r i))))
	   (normalize-poly r)))
	(t (let ((r (copy-poly p)))
	     (loop for i from 0 to (degree p) do
		  (setf (coef r i) (deriv-poly (coef r i) x)))
	     (normalize-poly r)))))

(defun prefix->infix (expr)
  (if (atom expr)
      expr
      (intersperse (expr-op expr)
		   (mapcar #'prefix->infix (expr-args expr)))))

(defun intersperse (op args)
  "Place `op' between `args'"
  (if (= 1 (length args))
      (first args)
      (rest (loop for arg in args
	       collect op
		 collect arg))))

(defun canon->prefix (p)
  (if (numberp p)
      p
      (args->prefix
       '+
       0
       (loop for i from (degree p) downto 0
	  collect (args->prefix '*
				1
				(list (canon->prefix (coef p i))
				      (exponent->prefix (main-var p) i)))))))

(defun exponent->prefix (base exponent)
  (case exponent
    (0 1)
    (1 base)
    (t `(^ ,base ,exponent))))

(defun args->prefix (op identity args)
  (let ((useful-args (remove identity args)))
    (cond ((null usefull-args) identity)
	  ((and (eq op '*) (member 0 args)) 0)
	  ((= 1 (length args))
	   (first usefull-args))
	  (t (cons op (mapcan #'(lambda (expr)
				  (if (starts-with expr op)
				      (expr-args expr)
				      (list expr)))
			      usefull-args))))))

(defun canon (infix-expre)
  "Canonicalize argument and convert it back to infix"
  (prefix->infix
   (canon->prefix
    (prefix->canon
     (infix->prefix infix-expr)))))

(defun canon-simplifier ()
  (loop
     (print 'canon>)
     (print (canon (read)))))

(defun poly^n (poly n)
  "Raise `poly' to the nth power"
  (check-type n (integer 0 *))
  (cond ((zerop n) 1)
	((integerp poly) (expt poly n))
	((t (let ((a (make-poly (main-var poly) (degree poly)))
		  (b (normalize-poly (subseq poly 0 (1- (length poly)))))
		  (a^n (make-array (1+ n)))
		  (b^n (make-array (1+ n)))
		  (result (make-poly (main-var poly) (* (degree p) n))))
	      ;; Set a to the head of `poly'
	      (setf (coef a (degree poly)) (coef poly (degree poly)))
	      ;; Comput a^i and b^i for i from 1 to n
	      (setf (aref a^n 0) 1)
	      (setf (aref b^n 0) 1)
	      (loop for i from 1 to n do
		   (setf (aref a^n i) (poly*poly a (aref a^n (1- i))))
		   (setf (aref b^n i) (poly*poly b (aref b^n (1- i)))))
	      (let ((c 1))		; c is used to compute choose function incrementally
		(loop for i from 0 to n do
		     (p-add-into! result c
				  (poly*poly (aref a^n i)
					     (aref b^n (1- n)))))
		(setf c (/ (* c (- n i)) (1+ i))))
	      (normalize-poly result))))))

(defun p-add-into! (result c p)
  "Destructively add c*p into result"
  (if (or (numberp p)
	  (not (var= (main-var p) (main-var result))))
      (setf (coef result 0)
	    (poly+poly (coef result 0) (poly*poly c p)))
      (loop for i from 0 to (degree p) do
	   (setf (coef result i)
		 (poly+poly (coef result i)
			    (poly*poly c (coef p i))))))
  result)

(defun make-rat (numerator denominator)
  "Build a rational: a quotient of two polynomials"
  (if (numberp denominator)
      (k*poly (/ 1 denominator) numerator)
      (cons numerator denominator)))

(defun rat-numerator (rat)
  "Get the numerator of `rat'"
  (typecase rat
    (cons (cdr rat))
    (number (denominator rat))
    (t 1)))

