(defvar *state* '() "The current status: a list of condition")

(defvar *ops* '() "A list of available operators")

(defvar *goals* '() "A list of goals to be achieved")

(defstruct op "An operation" action preconds add-list del-list)

(defun find-all (item sequence &rest kwargs &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test test-not kwargs)
      (apply #'remove item sequence :test-not test kwargs)))

(defun GPS (*state* *goals* *ops*)
  (if (achieve-all *goals*) 'solved))

(defun achieve (goal)
  (or (member goal *state*)
      (some #'apply-op
	    (find-all goal *ops* :test #'appropriate-p))))

(defun achieve-all (goals)
  (and (every #'achieve goals)
       (subsetp goals *state*)))

(defun apply-op (op)
  (when (every #'achieve (op-preconds op))
    (if (subsetp *goals* *state*)
	t
	(progn
	  (print (list 'executing (op-action op)))
	  (setf *state* (set-difference *state* (op-del-list op)))
	  (setf *state* (union *state* (op-add-list op)))
	  t))))

(defun appropriate-p (goal op)
  (member goal (op-add-list op)))

(defparameter *school*
  (list
   (make-op :action 'drive-son-to-school
	    :preconds '(son-at-home car-works)
	    :add-list '(son-at-school)
	    :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
	    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add-list '(car-works))
   (make-op :action 'tell-shop-problem
	    :preconds '(in-communication-with-shop)
	    :add-list '(shop-knows-problem))
   (make-op :action 'call-shop
	    :preconds '(know-phone-number)
	    :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
	    :preconds '(have-phone-book)
	    :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
	    :preconds '(have-money)
	    :add-list '(shop-has-money)
	    :del-list '(have-money))))


    
