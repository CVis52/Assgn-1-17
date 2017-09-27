{-
    Title: 
    Author: Clayton Vis
    Date: 26 Sept 2017


-}





{-
#######################
## Created Datatypes ##
#######################
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
msplit x1:x2:xs = ([x1],[x2])


{-
msplit lst = helper lst 0 ([], [])

helper :: [a] -> Int -> ([a],[a]) -> ([a],[a])
helper [] _ (fst, snd) = (fst, snd)
helper (x:xs) 0 (fst, snd) = helper xs (1) ((x:fst), snd)
helper (x:xs) 1 (fst, snd) = helper xs (0) (fst, (x:snd))
-}



--mergeInt :: ([Integer],[Integer]) -> [Integer]


--mergesortInt :: [Integer] -> [Integer]


--mergesort :: Ord a => [a] -> [a]


main = print $  myFlatten (Cons (Cons 1 Nil) (Cons (Cons 2 Nil) Nil))
