(defconstant fail nil "Indicates pat-match failure")
(defconstant no-binding '((t . t)) "Indicates pat-match success, with no variable")

(defun simple-equal (x y)
  "Are x and y equal? (Don't check inside strings.)"
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
	   (simple-equal (rest x) (rest y)))))

(defun variable-p (x)
  "A variable is a symbol starts with `?'"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-value (binding)
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part for var from a binding list."
  (binding-value (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add (var . val) into bindings"
  (acons var val (if (eq bindings no-binding) nil bindings)))

(defun pat-match (pattern input &optional (bindings no-binding))
  "Match pattern against input in the context of bindings."
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-match pattern input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern)
		    (rest input)
		    (pat-match (first pattern) (first input) bindings)))
	(t fail)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((null binding)
	   (extend-bindings var input bindings))
	  ((equal input (binding-value binding))
	   bindings)
	  (t fail))))

(defun segment-pattern-p (pattern)
  "A segment pattern is ((?* var) . pattern)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	;; We assume pattern starts with a constant,
	;; that is, there are no 2 consecutive vars.
	(let ((pos (position (first pat) input :start start :test #'equal)))
	  (if (null pos)
	      fail
	      (let ((b2 (pat-match pat
				   (subseq input pos)
				   (match-variable var
						   (subseq input 0 pos)
						   bindings))))
		(if (eq b2 fail)
		    ;; Try a longer one if this match fails
		    (segment-match pattern input bindings (1+ pos))
		    b2)))))))
(defun rule-pattern (rule)
  (first rule))

(defun rule-responses (rule)
  (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y)
     (What do you think about ?y)
     (Do you with that ?y)
     (Really -- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?)
     (You are being a bit negtive)
     (Are you saying NO just to be negtive?))
    (((?* ?x) I was (?* ?y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(defun make-safe (input)
  (read-from-string (remove-if-not #'(lambda (c)
				       (or (char= c #\Space)
					   (alphanumericp c)))
				   input :start 1 :end (1- (length input)))))

(defun eliza ()
  "Respond to user input using pattern matching"
  (loop with input
     until (equal input '(bye)) do
       (print 'eliza>)
       (setf input (make-safe (read-line)))
       (format t "~&~@(~{~a~^ ~}~)" (flatten (use-eliza-rules input history)))
     collecting input into history))
;;       (write (flatten (use-eliza-rules input)) :pretty t)))

(defun use-eliza-rules (input history)
  "Find some rule with which to transform the input"
  (or
   (some #'(lambda (rule)
	     (let ((result (pat-match (rule-pattern rule) input)))
	       (if (not (eq result fail))
		   (sublis (switch-viewpoint result)
			   (random-elt  (rule-responses rule))))))
	 *eliza-rules*)
   (append '(You said) (switch-viewpoint (random-elt history)))))

(defun switch-viewpoint (words)
  "Switch word from I to you and vice versa"
  (sublis '((I . you) (you . I) (me . you) (am . are)) words))


  
