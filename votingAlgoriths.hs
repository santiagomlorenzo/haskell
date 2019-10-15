import Data.List

fakeVotes :: [String]
fakeVotes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count value = length . filter (== value)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
-- rmdups (current:rest) = current : rmdups (filter (/= current) rest) [my version]
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)]
result totalVotes = sort results
  where 
    candidates = rmdups totalVotes
    votes candidate = count candidate totalVotes
    results = map (\candidate -> (votes candidate, candidate)) candidates

winner :: Ord a => [a] -> a
winner = snd . last . result


--------------------------------------------------------------------------------------------------------------------------

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmEmpty :: Eq a => [[a]] -> [[a]]
rmEmpty = filter (/=[])

elim :: Eq a => a -> [[a]] -> [[a]]
elim value = map (filter (/=value))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

alternateWinner :: Ord a => [[a]] -> a
alternateWinner list = case rank (rmEmpty list) of
                            [c]    -> c
                            (c:cs) -> alternateWinner (elim c list)
