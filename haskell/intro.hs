-- comment

:set prompt "string"
:module + Data.Ratio -- load module
:m -- same thing


2 + -3 -- doesn't work
2 + (-3) -- works
2*-3 -- treated as 2 *- 3, and *- is not an operator


-- Bools and integers are not coerced
1 && True -- error
1 && 1 -- error

2 /= 3 -- not equal
not True -- !true


-- Prefix (polish) notation
(+) 2 3 -- 5
(-) 3 2 -- 1


:info (+) -- info
          -- infixl (infix left)
          -- Precedence, higher integer has higher precedence
          -- Left associative (e.g. 3 + 4 + 3 == (3 + 4) + 3, 3 * 4 / 3 ==
          -- (3*4)/3) 
          -- infixr (infix precedence right)
          -- Precedence, higher integer has higher precedence
          -- Right associative (e.g. 2^3^2 == 2^(3^2))
          -- the combination of precedence and associativity rules are called
          -- fixity rules

pi -- predefined
e  -- not predefined
let e = exp 1 -- fixed


[1, 2, 3] -- list
          -- commas are separators, not terminators:
[1, 2, ]  -- error
[] -- empty list
[Bool, False, "testing"] -- error, all elements must be same type

[1..10] -- enumeration notation, generates [1,2,3,4,5,6,7,8,9,10]
[1.0,1.25..2.0] -- step of 1.0 - 1.25, that is 0.25, generates [1.0, 1.25, 1.5, 1.75, 2.0]
[1,4..15] -- [1,4,7,10,13] -- skips 15
[10,9..1] -- [10,9,8,7,6,5,4,3,2,1]
[10..1] -- []
[1..] -- infinite list

-- Careful with floating-points:
[1.0..1.8]
[1.0,2.0] -- Haskell rounds off from 1.0 to 1.8+0.5, to avoid floating-point roundoff problems


-- ++, concatenation
[3, 1, 3] ++ [3, 7] -- [3, 1, 3, 3, 7]
[] ++ [False, True] ++ [True] -- [False, True, True]


-- :, cons
1 : [2, 3] -- [1, 2, 3]
1 : [] -- [1]

"string"
putStrLn "string" -- print
['l', 'o', 't', 's', ' ', 'o' 'f', 'w', 'o', 'r', 'k'] -- == "lots of work"
"" == [] -- True
'a' : "bc" -- "abc"
"foo" ++ "bar" -- "foobar"


:set +t -- show Type information
:unset +t -- disable Type information
:type 'a' -- type information
'c'
-- 'c'
-- it :: Char
"foo"
-- "foo"
-- it :: [Char]



x :: y -- the expression x has type y
       -- the expression it has type [Char] -- [Char] == String
       -- the expression it has type Char


11%29 -- rational number, 11 is numerator, 29 is denominator
-- it :: Ratio Integer


-- Types:
-- Char, unicode character
-- Bool, boolean
-- Int, Signed, fixed-width integers (32 or 64 bits wide), guaranteed to be
-- wider then 28 bits.
-- Integer, signed, unbounded size
-- Double, floats, typically 64 bits.
-- Float, discouraged.
-- [a], "any type"

let a = [] :: [Int] -- let a be [] of type [Int]


-- Function calling:
odd 3
compare 2 3
compare 2 3 == LT -- "function application has higher precedence then using operators" ?
(compare 2 3) == LT -- same as above
compare sqrt 3 sqrt 6

((compare 2) 3) -- same thing

let a = (compare 2)
(a 3)  -- same thing

-- car
head [1, 2, 3, 4] -- 1
head "abc" - 'a'

-- cdr
tail [1, 2, 3, 4] -- [2, 3, 4]
tail "list" -- "ist"
tail [] -- error

-- tuple
(1964, "Labyrinths")
-- :: (Bool, [Char])
-- () is a special type which has one value, (). together called 'unit'.
-- Similar to void.

-- 2-tuple (pair)
-- 3-tuple (triple)
-- 5-tuple
-- etc



-- Defining a data type -- must start with a capital letter
--          value constructor
-- type constructor   components
--   v          v    v   v      v
data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

-- Synonyms
type CustomerID = Int
type  ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody
                  deriving (Show)

-- Creating a new value of data type...
myInfo = Book 9780135073455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

-- Book :: Int -> String -> [String] -> BookInfo
-- myInfo :: BookInfo
-- :type BookInfo -- not in scope


data Bool = False | True
-- The Bool type has two value constructors, False and True

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
