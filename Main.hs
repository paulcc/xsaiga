module Main where 

import Prelude as P
import System
import Text.PrettyPrint.HughesPJ

import Data.IntSet as S (toList)
import Data.IntMap as I (toList, fold, map, lookup, findWithDefault, IntMap)
import Data.Map    as M (toList) 

import PP
import Parser hiding ((<+>))
import ParserResults hiding ((<+>))

import qualified T_G1 as G1 
import qualified T_G2 as G2 
import qualified T_G3 as G3 
import qualified T_G4 as G4 
import qualified Misc

main 
 = do
	as <- getArgs
	maybe_repeat as

-- maybe_repeat (('C':n):c:[])
--  = ct (read n) (read c)

maybe_repeat (('X':n):as)
 = do
 	putStrLn $ "Running " ++ show n ++ " times!"
	sequence_ [ testG $ (head as {-++ show n-}) : tail as 
	          | n <- [1..read n] ]
maybe_repeat as
 = testG as

---------------------------------------

specs :: GrammarSpec String
specs = concat [ Misc.specs
               , G1.specs 
               , G2.specs 
               , G3.specs 
               , G4.specs 
	       ]

---------------------------------------

t0 i = testG ["T1" , "pp_ambig" , show i ]
t1 i = testG ["T4" , "pp_ambig" , show i ]
t2 i = testG ["T4f", "pp_ambig" , show i ]
t3 i = testG ["sm" , "aaa"      , show i ]
t4 i = testG ["sml" , "aaa"      , show i ]
t5 i = testG ["smml" , "aaa"      , show i ]
t5a i = testG ["sD" , "aaa"      , show i ]
t6 i = testG ["sG" , "aaa"      , show i ]
t7 i = testG ["sI" , "aaa"      , show i ]

testG :: [String] -> IO ()
testG as@[file,grammar,name,num] = testG_ file grammar name (read num)
testG as@[grammar,name,num]      = testG  (mkName as : as)
testG as@[grammar,num]           = testG  [grammar, "pp_ambig", num]
testG _                          = error $ "Usage: <exec> grammar name num"

mkName [n,g,t] = n ++ "-" ++ g ++ "-" ++ t

testG_ :: FilePath -> String -> String -> Int -> IO ()
testG_ filename grammar_name test_name test_num
 = do
	let (output,tests) = 
	      case P.lookup grammar_name specs of
	        Nothing  -> error $ "Bad grammar name: " ++ show grammar_name
	        Just val -> val

	let ts =
	      case getTest tests test_name test_num of 
	        Nothing  -> error $ "Bad test case: " ++ show (test_name,test_num)
	        Just val -> val

	-- setInput ts
	writeFile filename $ "Test case: " ++ show (grammar_name, test_name, test_num)
	appendFile filename "\n"
	appendFile filename $ show ts
	appendFile filename "\n"
	po (appendFile filename) (output ts 0)
	appendFile filename "\n"

