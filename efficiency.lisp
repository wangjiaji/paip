(defmacro defresource (name &key constructor (initial-copies 0) (size (max initial-copies 10)))
  (let ((resource (symbol name '-resource))
	(deallocate (symbol 'deallocate name))
	(allocate (symbol 'allocate- name)))
    `(let ((,resource (make-array ,size :fill-pointer 0)))
       (defun ,allocate ()
	 (if (= (fill-pointer ,resource) 0)
	     ,constructor
	     (vector-pop ,resource)))
       (defun ,deallocate (,name)
	 (vector-push-extend ,name ,resource))
       ,(if (> initial-copies 0)
	    '(mapc #',deallocate (loop repeat ,initial-copies
				      collect (,allocate))))
       `,name)))
  
(defmacro with-resource ((var resource &optional protect) &body body)
  (let ((allocate (symbol 'allocate- resource))
	(deallocate (symbol 'deallocate- resource)))
    (if protect
	`(let ((,var nil))
	   (unwind-protect (progn (setf ,var (,allocate)) ,@body)
	     (unless (null ,var)
	       (,deallocate ,var))))
	`(let ((,var (,allocate)))
	   ,@body
	   (,deallocate ,var)))))

(proclaim '(inline queue-contents make-queue enqueue
	    dequeue front empty-queue-p queue-nconc))

(defun queue-contents (q)
  (cdr q))

(defun make-queue ()
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  (setf (car q)
	(setf (rest (car q))
	      (cons item nil)))
  q)

(defun dequeue (q)
  (pop (cdr q))
  (if (null (cdr q))
      (setf (car q) q))
  q)

(defun front (q)
  (first (queue-contents q)))

(defun empty-queue-p (q)
  (null (queue-contents q)))

(defun queue-nconc (q list)
  (setf (car q)
	(last (setf (rest (car q)) list))))

