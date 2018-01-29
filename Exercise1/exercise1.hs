-- Name - Aasawaree Deshmukh

-- CIS 623, Exercises 1

-- Problem 1 ----------------------------------------------------------

-- degenerate a b c 
--   tests whether the line ax+by+c=0 is degenerate
-- EXAMPLES
--   (degenerate 0 0 1) should return True
--   (degenerate 1 1 0) should return False
degenerate :: Float -> Float -> Float -> Bool
degenerate a b c = if a == 0 && b == 0 then True else False 

-- TESTS
t2a = degenerate 0 0 1 -- should be True
t2b = degenerate 1 1 0 -- should be False
t2c = degenerate 3 0 1 -- should be False
t2d = degenerate 0 1 0 -- should be False
t2e = degenerate 0 0 7 -- should be True

-- Problem 2 ----------------------------------------------------------

-- onLine (x1,y1) a b c 
--   tests whether (x1,y1) is on the line ax+by+c=0.
-- EXAMPLES
--   (onLine (1,1) 1 1 0) should return True
--   (onLine (1,1) 1 1 1) should return False
onLine :: (Float,Float) -> Float -> Float -> Float -> Bool
onLine (x,y) a b c = if (a*x + b*y + c) == 0 then True else False 

-- TESTS
t1a = onLine (1,1) 1 1 0  -- should be False
t1b = onLine (1,1) 1 1 1  -- should be False
t1c = onLine (2,5) (-2) 1 (-1) -- should be True  

-- Problem 3 -----------------------------------------------------------

-- horizonal a b c 
--   tests whether the line ax+by+c=0 is horizonal.
--   We assume the line is nondegenerate.
-- EXAMPLES
--   (horizonal 0 (-3) 4) should be True
--   (horizonal 1 1 12)   should be False
horizonal ::  Float -> Float -> Float -> Bool
horizonal a b c = if a == 0 && b /= 0 then True else False

-- TESTS
t3a = horizonal 0 (-3) 4 -- should be True
t3b = horizonal 1 1 12   -- should be False
t3c = horizonal 0 0 1    -- should be False
-- Problem 4 -----------------------------------------------------------

-- vertical a b c 
--   tests whether the line ax+by+c=0 is vertical.
--   We assume the line is nondegenerate.

-- EXAMPLES
--   (vertical 3 0 (-2)) should return True
--   (vertical 1 1 12)   should return False

vertical  ::  Float -> Float -> Float -> Bool
vertical a b c = if b == 0 && a /= 0 then True else False -- <<<<< CHANGE THIS <<<<<

-- TESTS
t4a = vertical 3 0 (-2) -- should be True
t4b = vertical 1 1 12   -- should be False
t4c = vertical 0 0 1   -- should be False

-- Problem 5 -----------------------------------------------------------

-- xIntercept a b c 
--   = the x-intercept of the line ax+by+c=0, if it has one.
--   If the line is degenerate or horizonal, then 0.0 is returned.
-- EXAMPLES
--   (xIntercept 0 0 0) should return 0.0
--   ****ADD MORE****
xIntercept :: Float -> Float -> Float -> Float
xIntercept a b c = if not ( degenerate a b c ) && not ( horizonal a b c ) then (-c/a) else 0.0

-- TESTS
t5a = xIntercept 0 0 0 -- should be 0.0
t5b = xIntercept 1 4 1 -- should be -1.0
t5c = xIntercept 2 3 1 -- should be -0.5
t5d = xIntercept (-1) 2 2 -- should be 2.0

-- Problem 6 -----------------------------------------------------------

-- yIntercept a b c 
--   = the y-intercept of the line ax+by+c=0, if it has one.
--   If the line is degenerate or vertical, then 0.0 is returned.
-- EXAMPLES
--   (yIntercept 0 0 0) should return 0.0

yIntercept :: Float -> Float -> Float -> Float
yIntercept a b c = if not ( degenerate a b c ) && not ( vertical a b c ) then (-c/b) else 0.0 

-- TESTS
t6a = yIntercept 0 0 0  --  should be 0.0
t6b = yIntercept 1 4 1 -- should be -0.25
t6c = yIntercept 2 3 1 -- should be -0.33
t6d = yIntercept (-1) (-2) 2 -- should be 1.0

-- Problem 7 -----------------------------------------------------------

-- parallel a1 b1 c1 a2 b2 c2 
--   tests whether the two lines a1*x+b1*y+c1=0 and a2*x+b2*y+c2=0
--   are parallel.  We assume neither line is degenerate.
-- EXAMPLES
--  (parallel 1 2 24 3 6 (-9)) should return True
--   ****ADD MORE****

-- DEFINITION
parallel :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
parallel a1 b1 c1 a2 b2 c2 =  if (a1 * b2) == (a2 * b1) then True else False 

-- TESTS
t7a = parallel 1 2 24 3 6 (-9) -- should be True
t7b = parallel 2 4 4 3 6 8 -- should be True
t7c = parallel 1 0 0 3 6 4 -- should be False


-- Problem 8 -----------------------------------------------------------

-- intersect a1 b1 c1 a2 b2 c2 
--  tests whether the two lines a1*x+b1*y+c1=0 and a2*x+b2*y+c2=0 intersect 
--  in a single point.  If either of the two lines are degenerate, 
--  then False is returned.
-- EXAMPLES
--   (intersect 1 1 0 (-1) 1 0) should be True

intersect :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
intersect a1 b1 c1 a2 b2 c2 = if not (parallel a1 b1 c1 a2 b2 c2) then True else False

-- TESTS
t8a = intersect 1 1 0 (-1) 1 0 --  should be True
t8b = intersect 2 4 4 3 6 8 -- should be False

-- Problem 9 -----------------------------------------------------------

-- intersectionPt a1 b1 c1 a2 b2 c2 
--   determines the point of intersection the two lines
--   a1*x+b1*y+c1=0 and a2*x+b2*y+c2=0, if they have one.
--   If either line is degenerate or if the lines do not 
--   intersect in a point, (0.0,0.0) is returned.
-- EXAMPLES
--   (intersectionPt 1 0 (-1) 0 1 (-1)) should return (1.0,1.0)

intersectionPt :: Float -> Float -> Float -> Float -> Float 
                  -> Float -> (Float,Float)  
intersectionPt a1 b1 c1 a2 b2 c2 = if intersect a1 b1 c1 a2 b2 c2 then ((b1*c2 - b2*c1)/(a1*b2 - a2*b1) , (a2*c1 - a1*c2)/(a1*b2 - a2*b1)) else (0.0 , 0.0)

-- TESTS
-- The following is a helper function to do the tests here.
-- Given a1, b1, c1, a2, b2, and c2, it finds the supposed
-- intersection point via intersectionPt and then checks
-- that the result really is on the two lines.  The ``where''
-- business in simply one way of introducing local variables
-- in Haskell.  We will cover it in detail later in the course.
checkIntersect a1 b1 c1 a2 b2 c2 
    = (onLine (x,y) a1 b1 c2) && (onLine (x,y) a2 b2 c2)
      where (x,y) = intersectionPt a1 b1 c1 a2 b2 c2 

t9a = checkIntersect 1 0 (-1) 0 1 (-1) -- should be True
t9b = checkIntersect 1 1 (-2) (-1) 2 (-1) -- should be False

-- Problem 10 ----------------------------------------------------------

-- lineEqual a1 b1 c1 a2 b2 c2 
--   tests whether the two lines a1*x+b1*y+c1=0 and  a2*x+b2*y+c2=0 are 
--   nondegenerate and equal.
-- EXAMPLES
--   (lineEqual 1 1 1 2 2 2) should be True

lineEqual :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
lineEqual a1 b1 c1 a2 b2 c2 = if parallel a1 b1 c1 a2 b2 c2 && ( (-c1/a1) == (-c2/a2) || (-c1/b1) == (-c2/b2)) then True else False

-- TESTS
t10a = lineEqual 1 1 1 2 2 2 -- should be True
t10b = lineEqual 1 0 0 2 0 0 -- should be True
t10c = lineEqual 0 1 0 0 2 0 -- should be True
t10d = lineEqual 0 1 1 0 2 0 -- should be True

------------------------------------------------------------------------
------------------------------------------------------------------------
