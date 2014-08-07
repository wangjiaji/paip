(defmacro while (test &rest body)
  "Repeat body while test is true"
  (list* 'loop
	 (list 'unless test '(return nil))
	 body))

(defun prind (seq)
  (if (null seq)
      (princ seq)
      (progn
	(princ (car seq))
	(princ " ")
	(if (consp (cdr seq))
	    (prind (cdr seq))
	    (princ (list 'DOT (cdr seq)))))))

(defun find-all (item sequence &rest kwargs &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test test-not kwargs)
      (apply #'remove item sequence :test-not test kwargs)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debug info is (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (let ((fs (concatenate 'string "~&" format-string)))
      (apply #'format *debug-io* fs args))))

(defun debug-it (&rest ids)
  "Start dbg output for the given ids"
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug-it (&rest ids)
  "Stop dbg output for the given ids"
  (setf *dbg-ids* (if (null ids)
		      nil
		      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debug info"
  (when (member id *dbg-ids*)
    (format *debug-io* "~&~V@T~?" (* 2 indent) format-string args)))

(defun permutation (set)
  (if (null set)
      '(())
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (subsets)
			      (cons x subsets))
			  (permutation (remove x set))))
	      set)))
			  
(defun starts-with (seq x)
  "Check if seq's first element is x"
  (and (consp seq) (eql x (first seq))))

(defun flatten (alist)
  "Flattens a list so it does not contain sublists"
  (cond ((null alist) alist)
	((atom (first alist))
	 (cons (first alist) (flatten (rest alist))))
	(t (append (flatten (first alist)) (flatten (rest alist))))))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun interactive-prompt (prompt transformer)
  "Read an expression, transform it, print it, then continue"
  (loop (handler-case (progn
			(if (stringp prompt)
			    (print prompt)
			    (funcall prompt))
			(print (funcall transformer (read))))
	  ;; In case of error, do this
	  (error (condition)
	    (format t "~&;; Error ~a ignored, back to top level" condition)))))

(defun prompt-generator (&optional (num 0) (ctrl-string "[~d] "))
  "Return a function that prints prompts like [1], [2], etc."
  #'(lambda () (format t ctrl-string (incf num))))

(defun compose (&rest functions)
  #'(lambda (x)
      (reduce #'funcall functions :from-end t :initial-value x)))

(defun find-anywhere (item tree)
  "Does item occurs anywhere in the tree"
  (if (atom tree)
      (if (eql item tree) tree)
      (or (find-anywhere item (car tree))
	  (find-anywhere item (cdr tree)))))

(defun find-if-anywhere (pred tree)
  "If any item in the `tree' satisfies the `pred'"
  (if (atom tree)
      (funcall pred tree)
      (or (find-if-anywhere pred (car tree))
	  (find-if-anywhere pred (cdr tree)))))

(defun conc-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or string to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))
