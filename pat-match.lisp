(defconstant fail nil "Indicate pat-match failure")
(defconstant no-bindings '((t . t)))

(defun variable-p (x)
  "A variable is a symbol starts with a ?"
  (and (symbolp x)
       (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (var . val) pair in the binding list"
  (assoc var bindings))

(defun binding-var (binding)
  "Get the var part of the binding"
  (car binding))

(defun binding-val (binding)
  "Get the val part of the binding"
  (cdr binding))

(defun make-binding (var val)
  (cons var val))

(defun lookup (var bindings)
  "Get the val for var in the binding list"
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a binding into the binding list"
  (cons (make-binding var val)
	(if (eq bindings no-bindings)	; If binding list is the default one
	    nil				; Cons on to an empty list
	    bindings)))

(defun match-variable (var input bindings)
  "Check if var matches input.
Returns the extended binding list if match is successful"
  (let ((binding (get-binding var bindings)))
    (cond ((null binding)
	   (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))
  
(defun segment-pattern-p (pattern)
  "A segment pattern is like ((?* var) . pattern)"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  (and (consp pattern) (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  (funcall (segment-match-fn (first (first pattern)))
	   pattern input bindings))

(defun single-matcher (pattern input bindings)
  (funcall (single-match-fn (first pattern))
	   (rest pattern) input bindings))

(defun segment-match-fn (x)
  (when (symbolp x)
    (get x 'segment-match)))

(defun single-match-fn (X)
  (when (symbolp x)
    (get x 'single-match)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-matcher pattern input bindings))
	((single-pattern-p pattern)
	 (single-matcher pattern input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern)
		    (rest input)
		    (pat-match (first pattern) (first input) bindings)))
	(t fail)))

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)
(setf (get '?li 'single-match) 'match-literal)

(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun match-is (var-and-pred input bindings)
  "Binds var if input satisfies pred"
  (let* ((var (first var-and-pred))
	 (pred (second var-and-pred))
	 (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
	    (not (funcall pred input)))
	fail
	new-bindings)))

(defun match-literal (pattern input bindings)
  (if (eq pattern input)
      bindings
      fail))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input"
  (cond ((eq bindings fail) fail)
	((null patterns) bindings)
	(t (match-and (rest patterns)
		      input
		      (pat-match (first patterns) input bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any of the patterns matches"
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns input bindings))))
	(if (eq new-bindings fail)
	    (match-or (rest patterns) input bindings)
	    new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns matches the input"
  (if (match-or patterns input bindings)
      fail
      bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	(let ((pos (first-match-pos (first pat) input start)))
	  (if (null pos)
	      fail
	      (let ((b2 (pat-match pat
				   (subseq input pos)
				   (match-variable var
						   (subseq input 0 pos)
						   bindings))))
		(if (eq b2 fail)
		    ;; Try another if b2 fails
		    (segment-match pattern input bindings (1+ pos))
		    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input.
If pat1 is non-constant, then just return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
	 (position pat1 input :start start :test #'equal))
	((< start (length input)) start)
	(t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input"
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input"
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
	(pat-match (pat input bindings)))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables"
  (and (progv
	   (mapcar #'car bindings)
	   (mapcar #'cdr bindings)
	 (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern"
  (setf (get symbol 'expand-pat-match-abbrev)
	(expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat"
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
	((atom pat) pat)
	(t (cons (expand-pat-match-abbrev (first pat))
		 (expand-pat-match-abbrev (rest pat))))))

(defun rule-based-translator (input rules &key (matcher #'pat-match)
					    (rule-if #'first)
					    (rule-then #'rest)
					    (action #'sublis))
  "Find the first rule that matches the input, and apply the action"
  (some #'(lambda (rule)
	    (let ((result (funcall matcher (funcall rule-if rule) input)))
	      (if (not (eq result fail))
		  (funcall action result (funcall rule-then rule)))))
	rules))

(defun use-elize-rules (input)
  (rule-based-translator
   input
   *eliza-rules*
   :acton #'(lambda (bindings responses)
	      (sublis (switch-viewpoint bindings)
		      (random-elt responses)))))
