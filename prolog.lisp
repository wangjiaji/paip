(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
  "Indicates pat-match success with no variables")

(defun variable-p (x)
  "Is x a symbol begins with a question mark?"
  (and (symbol x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (acons var val (if (and (eq bindings no-bindings))
		     nil
		     bindings)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding)
	   (extend-bindings var input bindings))
	  ((equal input (binding-val binding))
	   bindings)
	  (t fail))))
