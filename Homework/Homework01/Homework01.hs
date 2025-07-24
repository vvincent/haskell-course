
-- Question 1
-- Write a multiline comment below.
{-
    this is a multiline comment
    more comment.
-}

-- Question 2
-- Define a function that takes a value and multiplies it by 3.
func x = x * 3

-- Question 3
-- Define a function that calculates the area of a circle.
areaOfCircle r = pi * (r * r)

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder. 
volumeOfCylinder h r = areaOfCircle r * h

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
isEqualTo40 h r = volumeOfCylinder h r >= 42    
