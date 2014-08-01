(clear-db)

(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list) (iright ?y ?x ?list))

(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest)) (iright ?left ?right ?rest))

(<- (= ?x ?x))

(<- (zebra ?h ?w ?z)
    ;; Each house is of the form:
    ;; (house nationality pet cigarette drink house-color)
    (= ?h ((house norwegian ? ? ? ?)
	   (house ? ? ? ? blue)
	   (house ? ? ? milk ?)
	   ?
	   ?))				; 5 houses
    (member (house englishman ? ? ? red) ?h)
    (member (house spaniard dog ? ? ?) ?h)
    (member (house ? ? ? coffee green) ?h)
    (member (house ukrainian ? ? tea ?) ?h)
    (iright (house ? ? ? ? ivory)
	    (house ? ? ? ? green) ?h)
    (member (house ? snails winston ? ?) ?h)
    (member (house ? ? kools ? yellow) ?h)
    (nextto (house ? ? chesterfield ? ?)
	    (house ? fox ? ? ?) ?h)
    (nextto (house ? ? kools ? ?)
	    (house ? horse ? ? ?) ?h)
    (member (house ? ? luckystrike orange-juice ?) ?h)
    (member (house japanese ? parliaments ? ?) ?h)
    ;; Questions
    (member (house ?w ? ? water ?) ?h)
    (member (house ?z zebra ? ? ?) ?h))
