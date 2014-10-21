(defstruct account
  (name "")
  (balance 0.00)
  (interest-rate 0.06))

(defun account-withdraw (account amount)
  (if (<= amount (account-balance account))
      (decf (account-balance account) amount)
      'insufficient-funds))

(defun account-deposit (account amount)
  (incf (account-balance account) amount))

(defun account-interest (account)
  (incf (account-balance account-balance)
	(* (account-interest-rate account)
	   (account-balance account))))

(defmacro define-class (class inst-vars class-vars &body methods)
  "Define a class for object-oriented programming"
  `(let ,class-vars
     (mapcar #'ensure-generic-fn ',(mapcar #'first methods))
     (defun ,class ,inst-vars
       #'(lambda (message)
  `(let ,class-vars
     (mapcar #'ensure-generic-fn ',(mapcar #'first methods))
     (defun ,class ,inst-vars
       #'(lambda (message)
	   (case message
	     ,@(mapcar #'make-clause methods))))))

(defun make-clause (clause)
  "Translate a message from define-class into a case clause"
  `(,(first clause) #'(lambda ,(second clause)
			,@(rest clause))))

(defun ensure-generic-fn (message)
  "Define an object oriented dispatch function for a message, unless already defined"
  (unless (generic-fn-p message)
    (let ((fn #'(lambda (obj &rest args)
		  (apply (get-method obj message) args))))
      (setf (symbol-function message) fn)
      (setf (get message 'generic-fn) fn))))

(defun generic-fn-p (fn-name)
  (and (fboundp fn-name)
       (eq (get fn-name 'generic-fn)
	   (symbol-function fn-name))))

(define-class account
    (name &optional (balance 0.00)) 
    ((interest-rate 0.06))
  (withdraw (amount)
	    (if (<= amount balance)
		(decf balance amount)
		('insufficient-funds)))
  (deposit (amount)
	   (incf balance amount))
  (balance () balance)
  (name () name)
  (interest ()
	    (incf balance (* interest-rate balance))))

(define-class password-account
    (password account)
    ()
  (change-password (pass new-pass)
		   (if (equal pass password)
		       (setf password new-pass)
		       'wrong-password))
  (otherwise (pass &rest args)
	     (if (equal pass password)
		 (apply message account args)
		 'wrong-password)))

