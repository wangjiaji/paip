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
	   (case message
	     ,@(mapcar #'make-clause methods))))))

(defun make-clause (clause)
  "Translate a message from define-class into a case clause"
  `(,(first clause) #'(lambda ,(second clause)
			,@(rest clause))))

(defun ensure-generic-fn (message)
  
