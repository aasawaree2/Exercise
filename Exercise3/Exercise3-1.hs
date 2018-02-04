--Problem 1.

--An expression of type Fruit is either an Apple(String, Bool) or an Orange(String, Int).
--We use a String to indicate the variety of the apple or orange, a Bool to describe whether an
--apple has a worm and an Int to count the number of segments in an orange. For example:

--Apple("Granny Smith", False) -- a Granny Smith apple with no worm
--Apple("Braeburn", True) -- a Braeburn apple with a worm
--Orange("Sanguinello", 10) -- a Sanguinello orange with 10 segments


--Write a function isBloodOrange :: Fruit -> Bool which returns True for blood oranges and
--False for apples and other oranges. Blood orange varieties are: Tarocco, Moro and Sanguinello. For example:

--isBloodOrange(Orange("Moro",12)) == True
--isBloodOrange(Apple("Granny Smith", True)) == False


--Assume the following are already defined:

--data Fruit = Apple(String, Bool)
-- | Orange(String, Int)
-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
--deriving (Show)


-- Problem 2.

--Write a function bloodOrangeSegments :: [Fruit] -> Int which returns the total number of blood orange 
--segments in a list of fruit.


--Assume the following are already defined:

--data Fruit = Apple(String, Bool)
-- | Orange(String, Int)
--deriving (Show)


--Problem 3.

--Write a function worms :: [Fruit] -> Int which returns the number of apples that contain worms.

--Assume the following are already defined:

--data Fruit = Apple(String, Bool)
-- | Orange(String, Int)
--deriving (Show)


--Problem 4.


--We can write propositions using the type Prop which has already been defined for you.
--For example, the proposition "((~P) AND Q)" can be written as the expression (Not (Var "P") :&: Var "Q")

--In the same vein, write the following formula as an expression p1 of type Prop:

--((P OR Q) AND (P AND Q))



--Assume the following are already defined:


--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
--deriving (Eq, Ord, Show)


--Problem 5.


--In the same vein, write the following formula as an expression p2 of type Prop.

--((P OR Q) AND ((~P) AND (~Q)))



--Assume the following are already defined:


--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
--deriving (Eq, Ord, Show)

--Problem 6.


--In the same vein, write the following formula as an expression p3 of type Prop.

--((P AND (Q OR R)) AND (((~P) OR (~Q)) AND ((~P) OR (~R))))



--Assume the following are already defined:


--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
--deriving (Eq, Ord, Show)

--Problem 7.

--A proposition is a tautology if it is always true, i.e.in every possible environment. Using names, envs and
--eval, write a function tautology :: Prop -> Bool which checks whether the given proposition is a tautology.

--Assume the following are already defined:


--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
--deriving (Eq, Ord, Show)

--type Names = [Name]
--type Env = [(Name, Bool)]

--Problem 8.

--Create two QuickCheck tests to verify that tautology is working correctly. Use the following facts as the 
--basis for your test properties:

--For any property P
--either P is a tautology, or ~P is satisfiable,
--either P is not satisfiable, or ~P is not a tautology.

--Be careful to distinguish the negation for Bool, not, from that for Prop, Not.


--Assume the following are already defined:


--type Name = String
--data Prop = Var Name
-- | F
-- | T
--  | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
--deriving (Eq, Ord, Show)

--type Names = [Name]
--type Env = [(Name, Bool)]

--Problem 9.


--We extend the declaration of the datatype Prop with the infix constructors :->: and :<->:.

--Find the printer showProp and evaluator eval functions and extend
--their definitions to cover the new constructors :->: and :<->:. (Only
--the eval function is tested when you press Check Function.)



--Assume the following are already defined:


-- Implementing propositional logic in Haskell
-- The datatype 'Prop'

--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
-- | Prop :->: Prop
-- | Prop :<->: Prop
--deriving (Eq, Ord, Show)

--type Names = [Name]
--type Env = [(Name, Bool)]


--Problem 10.


--We extend the declaration of the datatype Prop with the infix constructors :->: and :<->:.

--Find the names functions and extend its definitions to cover the new constructors :->: and :<->:.


--Assume the following are already defined:


-- Implementing propositional logic in Haskell
-- The datatype 'Prop'

--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
-- | Prop :->: Prop
-- | Prop :<->: Prop
--deriving (Eq, Ord, Show)

--type Names = [Name]
--type Env = [(Name, Bool)]



--Problem 11.


--Define the following formulas as expressions of type Prop:

--p4: ((P \IMPLIES Q) \AND (P \IFF Q))
--p5 ((P \IMPLIES Q) \AND (P \AND (\NOT Q)))
--p6: ((P \IFF Q) \AND ((P \AND (\NOT Q)) \OR ((\NOT P) \AND Q)))



--Assume the following are already defined:


-- Implementing propositional logic in Haskell
-- The datatype 'Prop'

--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
-- | Prop :->: Prop
-- | Prop :<->: Prop
--deriving (Eq, Ord, Show)



--Problem 12.


--Write a function equivalent :: Prop -> Prop -> Bool that returns True just when the two propositions are equivalent. For example:

-- *Main> equivalent (Var "P" :&: Var "Q") (Not (Not (Var "P") :|: Not (Var "Q")))
--True
-- *Main> equivalent (Var "P") (Var "Q")
--False
-- *Main> equivalent (Var "R" :|: Not (Var "R")) (Var "Q" :|: Not (Var "Q"))
--True

--You can use names and envs to generate all relevant environments, and use eval to evaluate the two Prop.


--Assume the following are already defined:


--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
-- | Prop :->: Prop
-- | Prop :<->: Prop
--deriving (Eq, Ord, Show)

--type Names = [Name]
--type Env = [(Name, Bool)]


--Problem 13.


--Write another version of equivalent, this time by combining the two arguments into a larger proposition and using tautology or satisfiable to evaluate it.


--Assume the following are already defined:


--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
-- | Prop :->: Prop
-- | Prop :<->: Prop
--deriving (Eq, Ord, Show)

--type Names = [Name]
--type Env = [(Name, Bool)]




--Problem 14.


--The subformulas of a proposition are defined as follows:

--A propositional letter P or a constant t or f has itself as its only subformula.
--A proposition of the form NOT P has as subfomulas itself and all the subformulas of P.
--A proposition of the form P AND Q, P OR Q, P -> Q, or P <-> Q has as subformulas itself and all the subformulas of P and Q.

--Add a definition for the function subformulas :: Prop -> [Prop] that returns all of the subformulas of a formula. For example:
-- *Main> map showProp (subformulas p2)
--["((P|Q)&((~P)&(~Q)))","(P|Q)","P","Q","((~P)&(~Q))","(~P)","(~Q)"]


--Assume the following are already defined:



--type Name = String
--data Prop = Var Name
-- | F
-- | T
-- | Not Prop
-- | Prop :|: Prop
-- | Prop :&: Prop
-- | Prop :->: Prop
-- | Prop :<->: Prop
--deriving (Eq, Ord, Show)

--type Names = [Name]
--type Env = [(Name, Bool)]