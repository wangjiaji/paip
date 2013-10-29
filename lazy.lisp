(defstruct delay (value nil) (func nil))

(defmacro delay (&body body)
  `(make-delay :func #'(lambda () ,@body)))

(defun force (x)
  (if (not (delay-p x))
      x
      (progn
	(when (delay-func x)
	  (setf (delay-value)
		(funcall (delay-func x)))
	  (setf (delay-func x) nil))
	(delay-value x))))

(defmacro make-pipe (head tail)
  `(cons ,head #'(lambda () ,tail)))

(defconstant empty-pipe nil)

(defun head (pipe)
  (first pipe))

(defun tail (pipe)
  (let ((rp (rest pipe)))
    (if (functionp rp)
	(setf rp (funcall rp))
	rp)))

(defun elt-pipe (pipe i)
  (if (zerop i)
      (head pipe)
      (elt-pipe (tail pipe) (1- i))))

(defun integers (&optional (start 0) end)
  (if (or (null end) (<= start end))
      (make-pipe start (integers (1+ start) end))
      nil))

(defun enumerate (pipe &key count key (result pipe))
  "Go through all (or count) elements in pipe, applying key to each of them"
  (if (or (eq pipe empty-pipe) (eql count 0))
      result
      (progn
	(unless (null key)
	  (funcall key (head pipe)))
	(enumerate (tail pipe) :count (if count (1- count)) :key key :result result))))

(defun filter (pred pipe)
  (if (funcall pred (head pipe))
      (make-pipe (head pipe) (filter pred (tail pipe)))
      (filter pred (tail pipe))))

(defun sieve (pipe)
  (make-pipe (head pipe)
	     (filter #'(lambda (x)
			 (/= (mod x (head pipe)) 0))
		     (sieve (tail pipe)))))

(defvar *primes* (sieve (integers 2)))

(defun map-pipe (fn pipe)
  (if (eq pipe empty-pipe)
      empty-pipe
      (make-pipe (funcall fn (head pipe))
		 (map-pipe fn (tail pipe)))))

(defun append-pipes (x y)
  (if (eq x empty-pipe)
      y
      (make-pipes (head x) (append-pipes (tail x) y))))

(defun mapcan-pipe (fn pipe)
  (if (eq pipe empty-pipe)
      (let ((x (funcall fn (head pipe))))
	(make-pipe (head x)
		   (append-pipes (tail x)
				 (mapcan-pipe fn (tail pipe)))))))

(defun generate-all (phrase)
  (if (listp phrase)
      (if (null phrase)
	  (list nil)
	  (combine-all-pipes (generate-all (first phrase))
			     (generate-all (rest phrase))))
      (let ((choices (rules-rhs (assoc phrase *grammar*))))
	(if choices
	    (mapcan-pipe #'generate-all choices)
	    (list (list phrase))))))

(defun combine-all-pipes (xp yp)
  (mapcan-pipe #'(lambda (y)
		   (map-pipe #'(lambda (x)
				 (append-pipes x y))
			     xp))
	       yp))

