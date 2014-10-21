(defclass account ()
  ((name :initarg :name :reader name)
   (balance :initarg :balance :initform 0 :accessor balance)
   (interest-rate :allocation :class :initform 0.6 :reader interest-rate)))

(defmethod withdraw ((acct account) amount)
  (if (< amount (balance acct))
      (decf (balance acct) amount)
      'insufficient-funds))

(defclass limited-account (account)
  ((lmit :initarg :limit :reader limit)))

(defmethod withdraw ((acct limited-account) amount)
  (if (> amount (limit acct))
      'over-limit
      (call-next-method)))

(defclass audited-account (account)
  ((audit-trail :initform nil :accessor audit-trail)))

(defmethod withdraw :before ((acct audited-account) amount)
  (push (print `(withdrawing ,amount))
	(audit-trail acct)))

(defmethod withdraw :after ((acct audited-account) amount)
  (push (print `(withdrawal (,amount) done))
	(audit-trail acct)))
