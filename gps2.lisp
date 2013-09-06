(defun executing-p (x)
  "Is x of the form: (executing ...)?"
  (starts-with x 'executing))

(defun starts-with (seq x)
  "Check if seq's first element is x"
  (and (consp seq) (eql x (first seq))))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention"
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make new operator that obeys the (EXECUTING op) convention"
  (convert-op (make-op :action action
		       :preconds preconds
		       :add-list add-list
		       :del-list del-list)))

(defvar *ops* nil "A list of available operators")

(defstruct op "An operation" action preconds add-list del-list)

(defun gps (state goals &optional (*ops* *ops*))
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

(defun achieve-all (state goals goal-stack)
  (let ((current-state state))
    (if (and (every #'identity (maplist #'(lambda (gs)
					    (setf current-state
						  (achieve current-state
							   (first gs)
							   goal-stack
							   (rest gs))))
					goals))
	     (subsetp goals current-state :test #'equal))
	current-state)))

;; (defun achieve-all (state goals goal-stack)
;;   "Achieve each goal and make sure they still hold at the end"
;;   (let ((current-state state))
;;     (if (and (every #'(lambda (g)
;; 			(setf current-state
;; 			      (achieve current-state g goal-stack)))
;; 		    goals)
;; 	     (subsetp goals current-state :test #'equal))
;; 	current-state)))

(defun achieve (state goal goal-stack remains)
  "A goal is achieved if it's already in the stack or if there's an appropriate op for it"
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member goal state :test #'equal) state)
	((member goal goal-stack :test #'equal) nil)
	(t (some #'(lambda (op)
		     (achieve-all (apply-op state goal op goal-stack)
				  remains
				  goal-stack))
		 (appropriate-ops goal state)))))

		 ;; (find-all goal *ops* :test #'appropriate-p)))))

(defun apply-op (state goal op goal-stack)
  "Returns a new, transformed state if op is applicable"
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op) (cons goal goal-stack))))
    (unless (null state2)		; Returns an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
			     (member x (op-del-list op) :test #'equal))
			 state2)
	      (op-add-list op)))))

(defun appropriate-p (goal op)
  "An op is appropriate if the goal is in its add-list"
  (member goal (op-add-list op) :test #'equal))

(defun use (oplist)
  "Use oplist as the default list of operators"
  (length (setf *ops* oplist)))

(defun appropriate-ops (goal state)
  "Return a list of appropriate ops sorted by number of unfullfilled preconds"
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p))
	#'<
	:key #'(lambda (op)
		 (count-if #'(lambda (precond)
			       (not (member precond state :test #'equal)))
			   (op-preconds op)))))
