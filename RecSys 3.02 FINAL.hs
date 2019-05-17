data User= U String deriving (Eq,Show)
data Item = I String deriving (Eq,Show)
data Fractional a=> Rating a= NoRating | R a deriving (Eq,Show)--fractional
member a []=False
member a(x:xs)= if (a==x) then True else member a xs
dis :: Eq a => [a] -> [a]
dis []=[]
dis (x:xs)=if (member x xs) then dis xs else x:dis xs
--dis x= disHelper [] x
--disHelper _ []=[]
--disHelper a (x:xs)=if ((member x xs)==False ) then x:disHelper a xs else disHelper (a++[x]) xs
getItemFromRate (_,a,_)=a
fromRatingsToItems1 []= []
fromRatingsToItems1 (x:xs)= (getItemFromRate x) : (fromRatingsToItems1 xs)
fromRatingsToItems::Eq a => [(b,a,c)] -> [a]
fromRatingsToItems a= dis (fromRatingsToItems1 a)
getUserFromRate (a,_,_)=a
fromRatingsToUsers1 []= []
fromRatingsToUsers1 (x:xs)= (getUserFromRate x) : (fromRatingsToUsers1 xs)
fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsers a= dis (fromRatingsToUsers1 a)
--getMaxRating a= getMaxRateHelp a (-1)
--getRate (U _,I _,a)=a
--getMaxRateHelp [] m=m
--getMaxRateHelp (x:xs) m= if ((getRate x) <= m) then getMaxRateHelp xs m else getMaxRateHelp xs (getRate x)
--hasRating u i a= hasRatingHelper u i a (getMaxRating a)
--hasRatingHelper _ _ _ 0= False
--hasRatingHelper (U a) (I i) b c= if ((member (U a, I i, c) b)==True) then True else hasRatingHelper (U a) (I i) b (c-1)
hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating _ _ []=False
hasRating (a) (i) (x:xs)= if ((getUserFromRate x)== (a) && (getItemFromRate x)==(i) ) then True else hasRating (a) (i) xs
getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating (a) (i) ((s,y, r):xs)= if ((hasRating (a) (i) ((s, y, r):xs))== True ) then

 if ((s)== (a) && (y)==(i) ) then r else getRating (a) (i) xs else error "No given rating"
formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]
formMatrixUser _ [] _=[]
formMatrixUser a (b:is) x= if ((hasRating a b x) ==True) then (R (getRating a b x):(formMatrixUser a is x))

 else (NoRating:(formMatrixUser a is x)) 
formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
formMatrix [] _ _=[]
formMatrix (a:xs) i x= (formMatrixUser a i x) : (formMatrix xs i x) 
numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem a x=numHelp a x 0
numHelp _ [] c=c
numHelp a (x:xs) c=if ((x!!a)==NoRating) then numHelp a xs c else numHelp a xs (c+1)
differeneRatings :: Fractional a => Rating a -> Rating a -> a
differeneRatings NoRating _ = 0.0
differeneRatings _ NoRating= 0.0
differeneRatings (R a) (R b)= (a-b)
matrixPairs :: Num a => a -> [(a,a)]
matrixPairs a= matrixHelper a 0 0
matrixHelper a b c= if (a==(b+1) && a==(c+1)) then [(b,c)] else if (c==(a-1))

 then [(b,c)]++(matrixHelper a (b+1) 0 ) else [(b,c)]++(matrixHelper a b (c+1)) 
dMatrix :: Fractional a => [[Rating a]] -> [a]
dMatrix (x:xs)= reverse(dMatrix2 (matrixPairs (length(x))) ((length(x)*length(x))-1) (x:xs))
dMatrix2 p a x=if (a==(-1)) then [] else (dMatrix3 (p!!a) x):(dMatrix2 p (a-1) x)   
dMatrix3 (y1,y2) (x:xs)= (differeneRatings (x!!y1) (x!!y2))+(dMatrix3 (y1,y2) xs)  
dMatrix3 _ []=0
--getMin a b= if (a<b) then a else b
freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]
freqMatrix (x:xs)= reverse(freqMatrix2 (matrixPairs (length(x))) ((length(x)*length(x))-1) (x:xs))
freqMatrix2 p a x=if (a==(-1)) then [] else (freqMatrix3 (p!!a) x):(freqMatrix2 p (a-1) x)
freqMatrix3 (y1,y2) x= freqMatrix4 y1 y2 x
freqMatrix4 _ _ []=0
freqMatrix4 a b (x:xs)=if ((x!!a)/= NoRating && (x!!b)/=NoRating) then 1+(freqMatrix4 a b xs) else freqMatrix4 a b xs
diffFreqMatrix :: Fractional a => [[Rating a]] -> [a]
diffFreqMatrix x= diffFreqMatrix2 (dMatrix x) (freqMatrix x)
diffFreqMatrix2 [] []=[]
diffFreqMatrix2 (x:xs) (y:ys)= (x/y):diffFreqMatrix2 xs ys
getFromMatrix y1 y2 x= (getHelper y1 y2 (sqrt(fromIntegral(length(x)))) 0 0 x)
getHelper _ _ _ _ _ []=	error "Not Found"
getHelper y1 y2 m c1 c2 (x:xs) = if ((y1 == c1) && (y2 == c2)) then x else if ((c2+1) == m) then (getHelper y1 y2 m (c1+1) 0 xs) else (getHelper y1 y2 m c1 (c2+1) xs)   	
predictHelper  x u i=reverse(predictHelper1 x u i (sqrt(fromIntegral((length(x))))-1)) --gets only the relevant differences from diffFreqMatrix
predictHelper1 x u i c=if (c==(-1)) then [] else if (c==i) then predictHelper1 x u i (c-1) else (getFromMatrix i c x):predictHelper1 x u i (c-1)
predHelper x u i=predHelper1 (formMatrixUser ((fromRatingsToUsers x)!!u) (fromRatingsToItems x) x) --gets item ratings that were rated by our User
predHelper1 []=[]
predHelper1 (x:xs)=if (x==NoRating)then predHelper1 xs else x:predHelper1 xs 
preHelper [] _ =0
preHelper _ []=0
preHelper ((R a):xs) (y:ys)= (a+y)+(preHelper xs ys) --adds averages on ratings of user
finalResultHelper x u i= (preHelper (predHelper x u i) (predictHelper (diffFreqMatrix (formMatrix (fromRatingsToUsers x) (fromRatingsToItems x) x)) u i))/fromIntegral(length(predHelper x u i))
getItemsUserRated _ [] _=[]
getItemsUserRated u (i:is) x=if ((hasRating u i x) ==True)then i:getItemsUserRated u is x else getItemsUserRated u is x 
predict x u i=if (((hasRating (fromRatingsToUsers x!!u) (getFromMatrix 0 i (fromRatingsToItems x))) x)==True) then ((getRating (fromRatingsToUsers x!!u) (getFromMatrix 0 i (fromRatingsToItems x))) x) else finalResultHelper x u i 
--predict1 u i x=if ((hasRating (fromRatingsToUsers x!!u) ((fromRatingsToItems x)!!i) x)==True) then (getRating u i x) else predict2 u i (diffFreqMatrix (formMatrix (fromRatingsToUsers x) (fromRatingsToItems x) x))
--predict1 u i x= (diffFreqMatrix (formMatrix (fromRatingsToUsers x) (fromRatingsToItems x) x))
   
