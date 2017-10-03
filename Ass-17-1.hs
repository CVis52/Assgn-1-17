{-
    Title: CPSC 521 -- Assignment 1 
    Author: Clayton Vis
    UCID: 30015308
    Date: 26 Sept 2017
-}

{-
    #######################
    ## Created Datatypes ##
    #######################
Note: Used throughout all questions
-}

data List a = Nil
            | Cons a (List a)
            deriving(Show)



foldListR :: (a -> b -> b) -> b -> List a -> b
foldListR f g Nil = g
foldListR f g (Cons a as) = f a (foldListR f g as)

{- My Right Fold
    Folding over a data structure replaces each constructor with a function applied to
    the contents. 

    Since Nil has no contents, g isn't really a function. It could be
    thought of as an "accumulator" as it must be the same type 'b' as the fold returns.
    However, the constructor (Cons a as) has contents. Thus we apply the given function
    to the content; both directly (f a), and then recursively to the nested content.

    The given function, f, is only evaluated once the recursion reaches the base case,
    Nil. This is why the fold evalutes elements from the right of the list first.
-}




foldListL :: (b -> a -> b) -> b -> List a -> b
foldListL f g Nil = g
foldListL f g (Cons a as) = foldListL f (f g a) as

{- My Left Fold
    As the right fold, we replace the Nil constructor with an  g.

    It may help to think of 'g' as an accumulator. We must update the "accumulator"
    before passing on to the next level of recursion. This is done by evaluating 
    (f g a), giving the next level of recursion an updated g, the "accumulator".


For the left fold I used https://wiki.haskell.org/Foldr_Foldl_Foldl%27 as a reference.
-}

myLast :: List a -> a
myLast (Cons x (Nil))   = x
myLast (Cons _ (xs))    = myLast xs
myLast Nil              = error "myLast: empty list"

myInit :: List a -> List a
myInit (Cons x (Nil)) = Nil
myInit (Cons x (xs))  = Cons x (myInit xs)
myInit Nil            = error "myInit: empty list"

myNull :: List a -> Bool
myNull Nil = True
myNull (Cons _ (_)) = False
{- My Last Element
    Recurses through a list until the last item is reached.

This is the same approach used in the standard prelude package. I referenced the code
at https://www.haskell.org/onlinereport/standard-prelude.html
-}


{-
    ################
    ## Question 1 ##
    ################
Write your own function for appending two lists and for reversing a list.
Define your own data type for lists and write the append function for this datatype.
-}


--Append Solution
app :: ([a],[a]) -> [a]                  
app (lst1, lst2)  = foldr (:) lst2 lst1

myApp :: (List a, List a) -> List a
myApp (l1, l2) = foldListR Cons l2 l1

-- Folding right over lst1, if we leave the concat construct the same, by passing
-- (:) or Cons to the fold, and replace the Nil construct with the second list we
-- can append lst2 to the end of lst1.

--Note: I needed to define a right fold, foldListR, for the List datatype.


-- Reverse Solution
rev :: [a] -> [a]
rev = foldl (\x y -> y:x) []


myRev :: List a -> List a
myRev = foldListL (\x y -> (Cons y (x))) Nil

-- Folding left over the list and we pass a concat function swapping the order of the
-- contents will reverse the order of the list.

--Note: I needed to define a left fold, foldListL, for the List datatype.



{-
    ################
    ## Question 2 ##
    ################
Write your own function for flattening a list of lists to a list.
Write a flatten function for your own datatype for lists.
-}


flatten :: [[a]] -> [a]
flatten = foldr (\x1 x2 -> app(x1,x2)) []

myFlatten :: List(List a) -> List a
myFlatten = foldListR (\x1 x2 -> myApp(x1,x2)) Nil

-- Using a right fold with the append functions we created in Q1. and an empty list
-- will take a list of [[a]] and for each element [a] will append them all to one list.



{-
    ################
    ## Question 5 ##
    ################
Write a function which splits a list into two lists, the first containing the odd
indexed elements and the second containing the even indexed elements.
-}

msplit:: [a] -> ([a],[a])
msplit (x:xs) = (takeSeconds (x:xs), takeSeconds xs)
    where
        takeSeconds :: [a] -> [a]
        takeSeconds [] = []
        takeSeconds (x1:[]) = x1:[]
        takeSeconds (x1:_:xs) = x1:takeSeconds xs

myMSplit :: List a -> (List a, List a)
myMSplit (Cons x (xs)) = (takeSeconds (Cons x (xs)), takeSeconds xs)
    where
        takeSeconds :: List a -> List a
        takeSeconds Nil = Nil
        takeSeconds (Cons x1 (Nil)) = Cons x1 (Nil)
        takeSeconds (Cons x1 (Cons _ (xs))) = Cons x1 (takeSeconds xs)



{-
    ################
    ## Question 6 ##
    ################
Write a function to merge two lists of integers, taking the least first element
at each step.
-}

mergeInt :: ([Integer],[Integer]) -> [Integer]
mergeInt ([], []) = []
mergeInt ([], (y:ys)) = y:mergeInt([], ys)
mergeInt ((x:xs), []) = x:mergeInt(xs, [])
mergeInt (x:xs, y:ys)   | x > y         =       y:mergeInt(x:xs, ys)
                        | otherwise     =       x:mergeInt(xs, y:ys)


myMergeInt :: (List Integer, List Integer) -> List Integer
myMergeInt (Nil, Nil) = Nil
myMergeInt (Nil, (Cons y (ys))) = Cons y (myMergeInt (Nil, ys))
myMergeInt ((Cons x (xs)), Nil) = Cons x (myMergeInt (xs, Nil))
myMergeInt ( (Cons x (xs)), (Cons y (ys)) ) | x > y     = Cons y (myMergeInt (Cons x (xs), ys))
                                            | otherwise = Cons x (myMergeInt (xs, Cons y (ys)))


{-
    ################
    ## Question 7 ##
    ################
-}

--mergesortInt :: [Integer] -> [Integer]

{-
    ################
    ## Question 8 ##
    ################
-}
--mergesort :: Ord a => [a] -> [a]


{-
    #################
    ## Question 11 ##
    #################
Given a relation -- rel:: a -> b -> Bool -- and a list of [a] and a list of [b],
write a function which returns a list of pairs of an element a, and the list of bs
from the second list, to which it is related

For example, if the relation given is the divide relation, then
relgrp div [2,3] [1,2,3,4,5,6] = [(2,[2,4,6]),(3,[3,6])]
-}


relgrp :: (a -> b -> Bool) -> [a] -> [b] -> [(a,[b])]
relgrp rel as bs = foldr 
                        (\x -> (:) (x, foldr (relTest x) [] bs) ) 
                        [] 
                        as
    where
        relTest a1 b1 bs = if (rel a1 b1) then (b1:bs) else bs

myRelGrp :: (a -> b -> Bool) -> List a -> List b -> List (a, List b)
myRelGrp rel as bs = foldListR 
                        (\x -> Cons (x, foldListR (myRelTest x) Nil bs) ) 
                        Nil 
                        as
    where
        myRelTest a1 b1 bs = if (rel a1 b1) then Cons b1 (bs) else bs

{- Process:
Fold over the first list [a], accumulate [(a,[b])]
For each element, a, in the fold, fold over the second list [b], accumulate [b]
      For each element, b, in the second fold, if (rel a b == True), then b:bs
                                                                     else bs
-}

{-
    #################
    ## Question 12 ##
    #################
Program the "group" function: given a predicate pred :: a -> a -> Bool, and a list,
the group function breaks the list into a series of (maximal) sublists such that any
two consecutive elements satisfy the predicate 'pred'. 
-}

group :: (a -> a -> Bool) -> [a] -> [[a]]
group pred as = foldl 
                        (\ys x -> if (null $ last ys)
                                    then 
                                        app (init ys, [[x]])
                                    else
                                        if (pred (last $ last ys) x)
                                            then
                                                app (init ys, [app ((last ys), [x])])
                                            else
                                                app (ys, [[x]])
                        )
                        [[]] 
                        as


{-
myGroup :: (a -> a -> Bool) -> List a -> List (List a)
myGroup pred as = foldListL
                            (\ys x -> if (myNull $ myLast ys)
                                        then
                                            myApp (myInit ys, Cons (Cons x (Nil))  (Nil))
                                        else
                                            if (pred (myLast $ myLast ys) x)
                                                then
                                                    myApp (myInit ys, Cons (myApp (myLast ys, Cons x (Nil))) (Nil))
                                                else
                                                    myApp (ys, Cons (Cons x (Nil)) (Nil))

                            )
                            Cons (Cons Nil (Nil)) (Nil)
                            as

-}

{- Process:
Foldl over the list of [a], accumulating a list of [[a]]
For each element, x, take a y where y is the last element from the last list of [a]
from the list of [[a]]. If pred y x passes, we have found two elements in a row that
pass. Thus, add the x to the last [a] in the list [[a]], otherwise we append a new list
of [a], containing only the x, to the end of the list [[a]].
-}



{-
    #################
    ## Question 17 ##
    #################
Write a function for adding and multiplying polynomials. You may represent the polynomials as lists of real numbers, so [1,0,3,4.2] = 1 + 3x^2 + 4.2^3.
-}

addpoly :: [Float] -> [Float] -> [Float]
addpoly [] [] = []
addpoly [] (y:ys) = y : addpoly [] ys
addpoly (x:xs) [] = x : addpoly [] xs
addpoly (x:xs) (y:ys) =  (x + y) : addpoly xs ys



multpoly :: [Float] -> [Float] -> [Float]
multpoly x y = x


{-
    #############################
    ######  TESTING ITEMS  ######
    #############################
-}

--main = print $ foldListR (:) [] (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Nil)))))

divTest :: Integral a => a -> a -> Bool
divTest a b | (\(x,y) -> y) (divMod b a) == 0      = True
         | otherwise                            = False

nbr :: Integral a => a -> a -> Bool
nbr x y | x == y        = True
        | abs(x - y) == 1    = True
        | otherwise     = False

list1 = [1,2,3,4]
list2 = [5,6,7]
tups = (list1, list2)

myList1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Nil))))
myList2 = Cons 5 (Cons 6 (Cons 7 (Nil)))
myTuple = (myList1, myList2)


list3 = [2,3]
list4 = [1,2,3,4,5,6]

myList3 = Cons 2 (Cons 3 (Nil))
myList4 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Nil))))))










