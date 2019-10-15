import Data.Char
import Data.List

-- encoder

letterToInt :: Char -> Int
letterToInt letter = ord letter - ord 'a'

intToLetter :: Int -> Char
intToLetter number = chr(ord 'a' + number)

shiftLowerCaseLetter :: Int -> Char -> Char
shiftLowerCaseLetter shiftFactor letter
  | isLower letter = intToLetter shiftedLetterUnicode
  | otherwise = letter
  where
    shiftedLetter = letterToInt letter + shiftFactor
    shiftedLetterUnicode = shiftedLetter `mod` 26

encode :: Int -> String -> String
encode shifter characters = map (shiftLowerCaseLetter shifter) characters


------------------------------------------------------------------------------------------------------------------------

-- cracker

englishLetterFrequencyTable :: [Float]
englishLetterFrequencyTable = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
                               0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
                               6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

countLowercase :: String -> Int
countLowercase = length.(filter (\x -> x>='a' && x<='z'))

countAppearances :: Char -> String -> Int
countAppearances character = length.(filter (\x -> x == character))

percentage :: Int -> Int -> Float
percentage int1 int2 = floatRatio * 100
  where floatRatio = fromIntegral int1 / fromIntegral int2

percentageOfAppearances characters totalCases character = percentage appearances totalCases
  where
    appearances = countAppearances character characters

appearancesFrequencyTable :: String -> [Float]
appearancesFrequencyTable characters = map (percentageOfAppearances characters lowercaseCharactersAmount) ['a'..'z']
    where
      lowercaseCharactersAmount = countLowercase characters 

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

dephase :: Int -> [a] -> [a]
dephase n xs = drop n xs ++ take n xs

decode shifter = encode (negate shifter) 

crack :: String -> String
crack encodedPhrase = decode shifter encodedPhrase
  where
    shifter = head(elemIndices (minimum chitab) chitab) 
    chitab = [chisqr (dephase n table') englishLetterFrequencyTable | n <- [0..25]]
    table' = appearancesFrequencyTable encodedPhrase