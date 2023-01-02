mylength [] = 0
mylength (x:xs) = 1 + mylength xs
mylast [x] = x
mylast (x:xs) = mylast xs
mynull [] = True
mynull x = False
mysum [] = 0
mysum (x:xs) = x + mysum xs
myzip [] x = []
myzip x [] = []
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys
myreplicate 0 x = []
myreplicate n x = x:myreplicate (n-1) x
myminimum [] = []
myminimum [x] = [x]
myminimum (a:b:xs) =  myminimum(min a b : xs)
mytake 0 x = []
mytake n [] = []
mytake n (x:xs) = x : mytake (n-1) xs
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]
mychoice a b c = mylast ([a | c] ++ [b | not c])

allCombH 1 [] = []
allCombH 1 (x:xs) = [[x] | x<- x:xs]
allCombH 2 (x:xs) = [x:b | b <-allCombH 1 xs]
allCombH 3 (x:xs)
  | 3 > length (x:xs) = []
  | otherwise = [x:b | b <- allCombH 2 xs] ++ allCombH 3 (x:drop 1 xs)
allCombH n (x:xs)
  | n > length (x:xs) = []
  | otherwise = [x:y | y <- allCombH (n-1) xs]  ++ allCombH n (x:drop 1 xs)
allComb n (x:xs)
  | n > length (x:xs) = []
  | otherwise = allCombH n (x:xs) ++ allComb n xs


main = do
  print(allComb 4 [1..10])
