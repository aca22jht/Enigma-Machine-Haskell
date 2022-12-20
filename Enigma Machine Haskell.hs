  {--COM2108 FUNCTIONAL PROGRAMMING ASSIGNMENT
  JASMINE TAY HUI PING
  Enigma.hs-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
  module Enigma where
  import Data.List
  import Data.Char 
  import Data.Maybe
  import Debug.Trace

----------------------------------------------------------------------------------------------------------------------------------------------------------

{- Part 1: Simulation of the Enigma 
(Top-down design, bottom-up implementation and testing) -}

  type Rotor = (String, Int)
  type Reflector = [(Char,Char)]
  type Offset = Int
  type Offsets = (Offset, Offset, Offset)
  type SteckerPair = (Char,Char)
  type Stecker = [(Char,Char)]
  type Crib = [(Char, Char)]
  type Menu = [Int]

 
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
              | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker
        deriving (Eq)


  {- Main function; encodeMessage, used for both SimpleEnigma and SteckeredEnigma.-}
  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = []
  encodeMessage (m:ms) (SimpleEnigma rL rM rR rf os)
    = enigmaEncode m (SimpleEnigma rL rM rR rf next_os) : encodeMessage ms (SimpleEnigma rL rM rR rf next_os)
      where
        next_os = nextState rL rM rR os
  encodeMessage (m:ms) (SteckeredEnigma rL rM rR rf os sb)
    = enigmaEncode m (SteckeredEnigma rL rM rR rf next_os sb) : encodeMessage ms (SteckeredEnigma rL rM rR rf next_os sb)
      where
        next_os = nextState rL rM rR os


  {- Helper function; enigmaEncode, takes a letter and an Enigma, encodes it from Right to Left then sends it to the reflector (reflectorB)
   and encodes it from Left to Right again.-}
  enigmaEncode :: Char -> Enigma -> Char
  enigmaEncode lt (SimpleEnigma rL rM rR rf os) = encodeLtoR (letterSwap rf (encodeRtoL lt sE)) sE
                                                        where
                                                        sE = SimpleEnigma rL rM rR rf os
  enigmaEncode lt (SteckeredEnigma rL rM rR rf os sb) = steckerLetter sb (encodeLtoR (letterSwap rf (encodeRtoL (steckerLetter sb lt) sE)) sE)
                                                        where
                                                        sE = SimpleEnigma rL rM rR rf os
                                                        sTE = SteckeredEnigma rL rM rR rf os sb


  {- Helper function; letterSwap, takes a reflector an a letter c, returns the letter that outputs out from the reflector. -}
  letterSwap :: Reflector -> Char -> Char
  letterSwap r c | (fst(findGuess c r)) == c = snd(findGuess c r)
                         | otherwise = fst (findGuess c r)


  {- Helper function; findGuess, returns the stecker pair for that letter. -}
  findGuess letter [] = (letter, letter)
  findGuess letter ((c1, c2):rest) | letter == c1 || letter == c2 = (c1, c2)
                                 | otherwise = findGuess letter rest


  {- Helper function; encodeLtoR, takes a letter and enigma, returns a letter after putting l throught the left, middle
   and right rotors (in that order) where l is the letter to be encoded. For the decoding later on.
   rL means Left rotor, rM means Middle rotor and rR means Right rotor.-}
  encodeRtoL :: Char -> Enigma -> Char
  encodeRtoL l (SimpleEnigma rL rM rR rf os) = rotorEncode rL (getOffset os 3) (rotorEncode rM (getOffset os 2) (rotorEncode rR (getOffset os 1) l))
                                                                                                where
                                                                                                sE = SimpleEnigma rL rM rR rf os
  encodeLtoR :: Char -> Enigma -> Char
  encodeLtoR l (SimpleEnigma rL rM rR rf os) = reverseRotorEncode rR (getOffset os 1) (reverseRotorEncode rM (getOffset os 2)
                                                                             (reverseRotorEncode rL (getOffset os 3) l))
                                                                             where
                                                                              sE = SimpleEnigma rL rM rR rf os


  {- Helper function; getOffset, takes a tuple of offsets and a number, returns the offset that corresponds to the given number.-}
  getOffset :: (Int,Int,Int) -> Int -> Int
  getOffset (z,y,x) a | a==1 = x
                      | a==2 = y
                      | otherwise = z


  {- Helper function; SteckerLetter, takes a steckerboard and a letter, returns the letter that outputs from the steckerboard
  'sb' means steckerboard and 'l' is the letter to be encoded. -}
  steckerLetter :: Stecker -> Char -> Char
  steckerLetter sb l | (fst (findGuess l sb)) == l = snd (findGuess l sb)
                     | otherwise = fst (findGuess l sb)

  {- Enigma plugboard (steckerboard) which contains all the possible stecker pairs -}
  steckerboard1 :: Stecker
  steckerboard1 = [
        ('B','E'),
        ('Q','R'),
        ('T','Y'),
        ('U','I'),
        ('O','P'),
        ('A','S'),
        ('D','F'),
        ('G','H'),
        ('J','K'),
        ('L','Z'),
        ('X','C'),
        ('V','W'),
        ('N','M')]

        
  {- Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]


  {- Helper function; nextState, Rotors are in order to match the user input in encodeMessage, x is the Right Rotor, k1 is Right Rotor's 
  knock-on pos, y is the Middle Rotor, k2 is the Middle Rotor's knock-on pos, z is the left rotor, k3 is the left rotor's knock-on pos. 
  nr1 is new rotor1, or the new state of rotor1 (Right Rotor), similarly for nr2 and nr3. -}
  nextState :: Rotor -> Rotor -> Rotor -> Offsets -> Offsets
  nextState (_,k3) (_,k2) (_,k1) (z,y,x)
   = (nr3, nr2, nr1)
     where
       nr1 = (x+1) `mod` 26
       nr2 | nr1 == k1 = (y+1) `mod` 26 -- nr1 is = to knock on 1
           | otherwise = y
       nr3 | nr2 == k2 && nr2 /= y = (z+1) `mod` 25 -- double stepping
           | otherwise = z


  {- Reverse a Rotor encoded function; reverseRotorEncode, takes any rotor, an offset and a character, reverses the encoded character 'c' to its previous state.-}
  reverseRotorEncode :: Rotor -> Int -> Char -> Char
  reverseRotorEncode rotor_no o c = alphabet !! ((alphaPos (reverseEncode rotor_no 0 (alphabet !! ((alphaPos c + o) `mod` 26))) - o) `mod` 26)


  {- Encode a Rotor function; rotorEncode, takes any rotor, an offset and a character, uses the encode function I did above, ciphers and maps.-}
  rotorEncode :: Rotor -> Int -> Char -> Char
  rotorEncode rotor_no o c = alphabet !! ((alphaPos(encode rotor_no 0 (alphabet !! (((alphaPos c) + o) `mod` 26))) - o) `mod` 26)


  {- Reverse Encode function; reverseEncode, takes any one of the rotors as input, an offset and a character,
  returns the character at a certain position before it got encoded.
  elemIndex: it returns the index of the first occurence of an element in the list-}
  reverseEncode :: Rotor -> Int -> Char -> Char
  reverseEncode rotor_no o c = alphabet !! ((fromJust(elemIndex c (getCipher rotor_no)) - o) `mod` 26)
  

  {- Encode function; encode, takes any one of the rotors as input, an offset and a character, 
  returns the alphabet at a certain position after the alphabet shifts up by the offset.
  !! Takes a list of things ([a]) and an Int then gives you back one of the thing in the list (a).
  you get the Cipher STRING and return the one thing in the list which the character that you wanna encode, adds an offset-}
  encode :: Rotor->Int->Char->Char
  encode rotor_no o c = getCipher rotor_no !! ((alphaPos c+ o) `mod` 26)
 
 
  {- Helper function; getKnockOn, extract knock-on value from the tuple of the Rotor. -}
  getKnockOn :: Rotor->Int
  getKnockOn = snd -- get knock-on position


  {- Helper function; getCipher, extract the string value from the tuple of the Rotor. -}
  getCipher :: Rotor->String
  getCipher = fst


  rotor1 :: (String, Int)
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int) -- Position 17 on the alphabet is Knock-On position for rotor1
  rotor2 :: (String, Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3 :: (String, Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4 :: (String, Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5 :: (String, Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)


  {- Helper function; alphaPos, given an uppercase letter, returns its index in the alphabet
  ('A' = position 0; 'Z' = position 25) -}
  alphaPos :: Char -> Int
  alphaPos c = ord c - ord 'A'


  {- NOTE: index starts from 0 until 25 (26 characters in the alphabet) -}
  alphabet :: String
  alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 


---------------------------------------------------------------------------------------------------------------------------------------------------

{- Part 2: Finding the Longest Menu by taking a crib and returning the longest possible menu in the crib 
  (Top-down design, bottom-up implementation and testing) -}


  {- Main function; longestMenu, find the longest menu, takes a crib and returns the longest menu of that given crib. -}
  longestMenu :: Crib -> Menu
  longestMenu crib | null (fst(unzip crib)) = []
                   | otherwise = reverse (searchLongestMenu(longestMenuAtEachIndex 0 crib))


  {- Helper function; longestMenuAtEachIndex, takes a starting index and a crib.
   Returns a list of menus which are the longest menus for each index from the
   starting index on, where i = starting index. -}
  longestMenuAtEachIndex :: Int -> Crib -> [Menu]
  longestMenuAtEachIndex i crib | i == length(fst cribunzip) = []
                             | i == length(fst cribunzip) - 1 = [searchLongestMenu (searchMenus [i] crib [])]
                             | otherwise = searchLongestMenu (searchMenus [i] crib []): longestMenuAtEachIndex (i + 1) crib
                              where cribunzip = unzip crib


  {- Helper function; searchLongestMenu; takes a list of menus,
   returns the longest menu in a given list of menus.
   m = list of menus. -}
  searchLongestMenu :: [Menu] -> Menu
  searchLongestMenu listofMenus = minimumBy (\xs ys -> compare (length ys) (length xs)) listofMenus


  {- Helper function; searchMenus, takes an index, a crib and a menu.
   Returns a list of possible menus from the given index.
   n = index, c = crib, m = menu to be returned.-}
  searchMenus :: [Int] -> Crib -> Menu -> [Menu]
  searchMenus n c m | (length n) == 0 = [m]
                | (length n) == 1 && elem (head n) m = [m]
                | (length n) == 1 && not(elem (head n) m) = searchMenus (head (searchSuccessors [head n] c)) c ((head n):m)
                | elem (head n) m = searchMenus (tail n) c m
                | otherwise = searchMenus (head (searchSuccessors [head n] c)) c ((head n):m) ++ searchMenus (tail n) c m


  {- Helper function; searchSucessors, Takes an index and a crib.
   Returns a list of indexes for which the letter in the main text
   is equal to the letter in the cipher text at index n.
   n = given index, c = crib-}
  searchSuccessors :: [Int] -> Crib -> [[Int]]
  searchSuccessors n c | length n == 0 = [[]]
                   | length n == 1 = [deduceIndexes (searchOpposite c (head n)) 0 (fst cribunzip) [0]]
                   | otherwise = deduceIndexes (searchOpposite c (head n)) 0 (fst cribunzip) [0]: searchSuccessors (tail n) c
                  where cribunzip = unzip c


  {- Helper function; searchOpposite, Takes a crib and an index.
   Returns the letter in the cipher text at that index of the crib.-}
  searchOpposite :: Crib -> Int -> Char
  searchOpposite crib index = snd cribunzip !! index
      where cribunzip = unzip crib

  
  {- Helper function; deduceIndexes, takes a character, the current index, a string (text) and a list of indexes.
   Returns the indexes of the occurences of the letter in the string (text) based on the current index (ci) input by the user.
   c = letter, ci = current index, text = string potentially containing the letter, is = list of indexes -}
  deduceIndexes :: Char -> Int -> String -> [Int] -> [Int]
  deduceIndexes c ci text is | (length text) == 0 = []
                         | (length text) == 1 = if (text !! 0) == c then tail (is ++ [ci])
                                                 else tail is
                         | (head text) == c = deduceIndexes c (ci + 1) (tail text) (is ++ [ci])
                         | otherwise = deduceIndexes c (ci + 1) (tail text) is -- 'is' is list of indexes


  {- Crib1; zips the crib (crib1) in the required format. -}
  crib1 :: Crib
  crib1 = zip "WETTERVORHERSAGEBISKAYA" "RWIVTYRESXBFOGKUHQBAISE"

  -- ("WETTERVORHERSAGEBISKAYA", "RWIVTYRESXBFOGKUHQBAISE")

---------------------------------------------------------------------------------------------------------------------------------------------------

{- Part 3: Simulating the Bombe (Break Enigma)
  (Top-down design, bottom-up implementation and testing) -}
 -- Plain:  WETTERVORHESAGEBISKAYA
 -- Cipher: RWIVTYRESXBFOGKUHQBAISE
 -- Note: Steckerboard has type Stecker, the pair of steckers in the steckerboard have the type SteckerPair.

  {- Main function; breakEnigma -}
  {- Takes a crib, returns a Maybe (Offsets, Stecker) -}
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma crib = breakEA crib menu (0,0,0)
      where menu = longestMenu crib
           

 {- Break EA -}
  {- Takes a crib, a menu and offsets, generates a list of all possible pairs and calls findStecker 
    until one offset works if none work, then Nothing is returned. When the left rotor offsets reaches 
    26, that means all the rotors offsets are tried and it returns Nothing because it can't find a solution.
    BreakEA will be used in the breakEnigma main function where it will use the longestMenu of the crib as the menu.-}
  breakEA :: Crib -> Menu -> Offsets -> Maybe (Offsets, Stecker)
  breakEA crib menu off 
        | leftOff == 26 = Nothing
        | null boards = breakEA crib menu nextOff 
        | otherwise = Just (off, boards)  
        where nextOff = updateOffsets off               
              startLetter = fst(unzip crib) !! (menu!!0)
              boards = findStecker crib menu startLetter 0 off
              (leftOff, _, _) = off


  {- Find Stecker -} 
  {- Takes a crib, a menu, a character (fstChar), a trackAlphaNum local tracker and offsets
    generates a list of all possible pairs and then tries them all until one succeeds, if not
    then the null list is returned.
    trackAlphaNum; keeps track of where in the alphabet findStecker has tried to match the fstChar to already. -}
  findStecker :: Crib -> Menu -> Char -> Int ->  Offsets -> Stecker
  findStecker crib menu fstChar trackAlphaNum off 
        | trackAlphaNum == 26 = []
        | null allPairs = findStecker crib menu fstChar (trackAlphaNum + 1) off 
        | otherwise = allPairs
        where allPairs = followMenu crib menu [(fstChar, alphaAtPos trackAlphaNum)] off 
    

  {- Alpha at Position -}
  {- Returns the character at the position in the Alphabet -}
  alphaAtPos :: Int -> Char
  alphaAtPos index = chr (ord 'A' + index)


  {-Follow Menu-}
  {- Takes a crib, a menu, a steckerboard and offsets. Function calls followMenu 
  recursively with the next menu position until the menu is empty or adding the 
  current pairs returns nothing. Returns an updated steckerboard with new stecker pairs 
  computed. -}
  followMenu :: Crib -> Menu -> Stecker -> Offsets -> Stecker
  followMenu _ [] b _ = removeUnsteckered b
  followMenu crib (i:menu) sboard off 
        | steckboard == [] = [] 
        | otherwise = followMenu crib menu steckboard off
        where sE = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (indexOffsets off i 0)
              (p,e) = unzip crib                                 -- unzip crib to get the plain and the encoded cipher, used Pattern Matching here
              pAti = p !! i                                      -- pAti is basically the plain at index i
              c = steckerLetter sboard pAti   
              (r:rest) = encodeMessage [c] sE 
              newPair = (r, e !! i)           
              steckboard = steckerAdd newPair sboard



  {-Stecker Add-}
  {- steckerAdd, takes a stecker pair and a steckerboard,
   compares any combination of the pair with all the letters in the steckerboard,
   uses list comprehension to extract all the characters and compare them to the new pair -}
  steckerAdd :: SteckerPair -> Stecker -> Stecker
  steckerAdd (a,b) board
        | length [(x,y) | (x,y)<-board, (a==x && b==y) || (b==x && a==y)] /= 0 = board
        | a `elem` l = []
        | a `elem` r = []
        | b `elem` l = []
        | b `elem` r = []
        | otherwise = (a,b):board
        where r = [x | (x,y)<-board]
              l = [y | (x,y)<-board]


  {- Remove Unsteckered -}
  {- removeUnsteckered, takes a steckerboard and removes any unsteckered pairs e.g. (N,N) -}
  removeUnsteckered :: Stecker -> Stecker
  removeUnsteckered board = [(x,y) | (x,y)<-board, x/=y]


  {- Index Offsets -}
  {- Helper function; indexOffsets takes an initial offset, an index and numApply where 
   if numApply is equal to the index then returns the offsets at that index otherwise it keeps 
   incrementing by 1 until it finds the offsets for that index and returns it.
   e.g. (0,0,0) 31 0 = (0,1,5) 
   nextOff is the nextState of the rotors in the Enigma -}
  indexOffsets :: Offsets -> Int -> Int -> Offsets
  indexOffsets off index numApply 
      | numApply == index = off
      | otherwise = indexOffsets nextOff index (numApply+1) 
      where nextOff = nextState rotor1 rotor2 rotor3 off
      

  {- Update Rotor Offsets -}
  {- updateOffsets, function to increment the offsets without knock-on positions, different from nextState.
   traceShow function; prints the output of the offsets to the terminal so it doesn't look like the terminal is 
   not doing anything  I trace showed the leftmost rotor as it is the last one to determine when all the offsets 
   are reset to 0. -}
  updateOffsets :: Offsets -> Offsets
  updateOffsets (l, m, r)
    | r < 25    = (l, m, r+1)
    | m < 25    = (l, m+1, 0)
    | otherwise = traceShow (l+1) (l+1, 0, 0) 
  

  {- Starting Offsets -}
  startOffsets = (0,0,0)


{- THE END -}
