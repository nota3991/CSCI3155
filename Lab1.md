#1(a)
Line 3; Line 1
#1(b)
Line 2; Line 5; Line 5; Line 1

#2
The body of g is well typed since what is returned is a tuple of integers (a, b). This is
determined since a and b are both integer types.

val (a, b) = (1, (x, 3)):(Int, (Int, Int)) because
	x:Int
	3:Int
	(x, 3):(Int, Int)
	1:Int
	(1, (x, 3)):(Int, (Int, Int))