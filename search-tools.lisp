(defclass problem ()
  ((states :initarg :states :accessor problem-states)))

(defmethod searcher ((prob problem))
  "Find a state that solves the search problem"
  (cond ((no-states-p prob) fail)
	((goal-p prob) (current-state prob))
	(t (let ((current (pop-state prob)))
	     (setf (problem-states prob)
		   (problem-combiner prob
				     (problem-successors prob current)
				     (problem-states prob))))
	   (searcher prob))))

(defmethod current-state ((prob problem))
  "The current state is the first of the possible states"
  (first (problem-states prob)))

(defmethod pop-state ((prob problem))
  "Remove and return the current state"
  (pop (problem-states prob)))

(defmethod no-state-p ((prob problem))
  "Are there anymore unexplored states?"
  (null (problem-states prob)))

(defmethod searcher :before ((prob problem))
  (dbg 'search "~&;; Search: ~a" (problem-states prob)))

(defclass eql-problem (problem)
  ((goal :initarg :goal :reader problem-goal)))

(defmethod goal-p ((prob eql-problem))
  (eql (current-state prob) (problem-goal prob)))

(defclass dfs-problem (problem) ()
  (:documentation "Depth-first search problem"))

(defclass bfs-problem (problem) ()
  (:documentation "Breadth-first search problem"))

(defmethod problem-combiner ((prob dfs-problem) new old)
  "Depth-first search looks at new states first"
  (append new old))

(defmethod problem-combiner ((prob bfs-problem) new old)
  "Breadth-first search looks at old states first"
  (append old new))

(defclass binary-tree-problem (problem) ())

(defmethod problem-successors ((prob binary-tree-problem) state)
  (let ((n (* 2 state)))
    (list n (1+ n))))
