(<- (grandfather ?g ?c)
    (father ?g ?p)
    (parent ?p ?c))

(<- (grandmother ?g ?c)
    (mother ?g ?p)
    (parent ?p ?c))

(<- (parent ?p ?c)
    (father ?p ?c))

(<- (parent ?p ?c)
    (mother ?p ?c))

