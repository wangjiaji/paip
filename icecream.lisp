(defparameter *icecream-ops*
  (list
   (make-op :action 'eat-icecream
	    :preconds '(has-icecream)
	    :add-list '(eaten-dessert eaten-icecream)
	    :del-list '(has-icecream))
   (make-op :action 'eat-cake
	    :preconds '(has-cake)
	    :add-list '(eaten-dessert eaten-cake)
	    :del-list '(has-cake))
   (make-op :action 'get-free-icecream
	    :preconds '(purchased-cake eaten-cake)
	    :add-list '(has-icecream))
   (make-op :action 'buy-cake
	    :preconds '(has-money)
	    :add-list '(purchased-cake has-cake)
	    :del-list '(has-money))))
