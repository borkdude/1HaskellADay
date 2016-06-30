module Data.SymbolTable.Compiler where

{-- A solution to the problem posted at http://lpaste.net/5530458314216833024
@1HaskellADay solution for 2016-03-09

See, yesterday we created a symbol-table of stock symbols (imported above),
so we could convert the ShowsGaps to ScoreCards for (eventual) further analysis.

That's one viable solution, but it involves dynamicism and a lot of 'Maybe'-ness
around converting between ShowsGaps and ScoreCards. Wouldn't it be nice if
ScoreCards had the symbols as indices? Well, it can't because its indices are
Ints.

Let's fix this (again, eventually) by making the indices indexible types (Ix).
Strings, in the main, are not Ix-values. How do we make them that?

Look at the Epilogue at the bottom (obviously) of Data.SymbolTable.

Guess what we're doing today!

Write a SymbolTable-compiler that takes a symbol table and writes it out as a
Haskell module containing the data type of your naming and the approapriate
show-representation.
--}

import Control.Arrow
import Control.Monad (join)
import Control.Monad.State
import Data.List (intercalate)

import Control.DList
import qualified Data.BidirectionalMap as BDM
import Data.SymbolTable

type DataTypeName = String
type ModuleName = String

compile :: DataTypeName -> ModuleName -> SymbolTable -> IO ()
compile typeName moduleName = 
   writeFile (filename moduleName)
   . uncurry (header moduleName touchMeNot imports)
   . (decls typeName &&& (showGenerator typeName &&& converts typeName))
   . BDM.toList . table

symId :: Int -> String
symId = ('S':) . show

{--
*Main> loadShowsGaps "stock-market/showsgaps-all.csv" ~> sg
*Main> let syms = execState (symsIx sg) empty
*Main> compile "Stock" "Graph.Stocks" syms

Graph.Stocks exported to http://lpaste.net/5184757380109303808

geophf:1HaskellADay geophf$ ghci "Graph/Stocks.hs" ~>
Ok, modules loaded: Graph.Stocks.
*Graph.Stocks> :t S1 ~> S1 :: Stock
*Graph.Stocks> S1 ~> AAPL
*Graph.Stocks> let s1 = read "AAPL" :: Stock ~> AAPL  -- creepy!
*Graph.Stocks> fromEnum s1 ~> 1
*Graph.Stocks> toEnum 1 :: Stock ~> AAPL              -- creepy!

... YAY!

From your shell, load in that module and answer the following questions.

*Main> compile nm "Graph.FooOrSommat" syms
... saved to http://lpaste.net/8450299102058512384

geophf:1HaskellADay geophf$ wc Graph/FooOrSommat.hs 
     342    3209   21770 Graph/FooOrSommat.hs
geophf:1HaskellADay geophf$ wc Analytics/Trading/Data/Stocks.hs 
    1114    5242   26710 Analytics/Trading/Data/Stocks.hs

What is the maximum value of Symbol?
*Graph.FooOrSommat> maxBound :: Symbol ~> ZU ~> fromEnum ~> 519

-- What is the Symbol value for "AAPL"?
*Graph.FooOrSommat> (read "AAPL") :: Symbol ~> AAPL ~> fromEnum ~> 1

-- What is the show-representation of the Symbol S302?
*Graph.FooOrSommat> S302 ~> MRVL

-- These changes will, of course, be integrated into the SymbolTable compiler.
--}

filename :: String -> FilePath
filename = (++ ".hs") . map (\c -> if c == '.' then '/' else c)

-- *Main> filename "Graph.Stocks" ~> "Graph/Stocks.hs"

header :: String -> [String] -> [String] -> [String] -> ([String], [String]) -> String
header moduleName intro imports decls (showme, readme) =
   weave [["module " ++ moduleName ++ " where"],
          intro, imports, decls, showme, readme]
      where weave = unlines . intercalate [""]

decls, showGenerator, converts :: String -> [(String, Int)] -> [String]

decls dataTypeName =
   let hdr = ("data " ++ dataTypeName ++ " ")
       len = length hdr
   in  recombineWith ((hdr ++) . drop len)
                     (++ ["   deriving (Eq, Ord, Enum, Bounded, Ix)"])
     . chunkBy len
     . recombineWith ('=':) (map ('|':)) . map ((' ':) . symId . snd)

recombineWith :: ([a] -> [a]) -> ([[a]] -> [[a]]) -> [[a]] -> [[a]]
recombineWith headf tailf = headf . head &&& tailf . tail >>> uncurry (:)

{--
*Main> decls "Stock" (BDM.toList (table syms)) ~>
["data Stock ","           = AA | AAPL | ABB | ABBV [...]",...]
--}

chunkBy :: Int -> [String] -> [String]
chunkBy offset [] = []
chunkBy offset syms =
   (uncurry (:) . second (chunkBy offset)) (aline offset 0 emptyDL syms)

aline :: Int -> Int -> DList String -> [String] -> (String, [String])
aline offset totes accum [] = (ans offset accum, [])
aline offset totes accum syms@(sym:bols) =
   let lenS = length sym + 1 + totes -- + 1 for the space of unwords
   in  if offset + lenS > 77 then (ans offset accum, syms)
       else aline offset lenS (accum <| sym) bols

ans :: Int -> DList String -> String
ans offset = (replicate offset ' ' ++) . unwords . dlToList

touchMeNot :: [String]
touchMeNot =
   ["-- autogenerated from Data.SymbolTable.Compiler.compile"]

showGenerator typename =
   (["instance Show " ++ typename ++ " where","   show = (showSym !)","",
     "showSym :: Array " ++ typename ++ " String"] ++)
   . uncurry showSyms . ((map snd >>> head &&& last) &&& id)

{--
*Main> let (nm, syms) = decompile "Symbol" S0
*Main> take 7 (showGenerator "Symbol" syms) ~>
["instance Show Symbol where",
 "   show = (showSym !)",
 "",
 "showSym :: Array Symbol String",
 "showSym = array (S0,S519) [",
 "   (S0,\"AA\") , (S1,\"AAPL\") , (S2,\"ABB\") , (S3,\"ABBV\") , (S4,\"ABEV\")",
 "   , (S5,\"ABGB\") , (S6,\"ABX\") , (S7,\"ABY\") , (S8,\"ACAD\") , (S9,\"ACHN\")"]
--}

-- pretty-prints out array information
showSyms :: (Int, Int) -> [(String, Int)] -> [String]
showSyms decl =
   (("showSym = array " ++ show (join (***) (U . symId) decl) ++ " [") :)
   . (++ ["   ]"])
   . chunkBy 4
   . recombineWith id (map (", " ++))
   . map (U . symId . snd &&& fst >>> show)

{--
*Main> showSyms (1,10) $ sequence ("hi", [1..10]) ~>
["showSym = array (S1,S10) [",
 "   (S1,\"hi\") , (S2,\"hi\") , (S3,\"hi\") , (S4,\"hi\") , (S5,\"hi\") , (S6,\"hi\")",
 "   , (S7,\"hi\") , (S8,\"hi\") , (S9,\"hi\") , (S10,\"hi\")"]
--}

data Stringy = Q String | U String

instance Show Stringy where
   show (Q s) = show s
   show (U s) = s

-- gives a well-formatted ('pretty-printed') SymbolTable Array representation

converts typeName =
   (["instance Read " ++ typeName ++ " where",
     "   readsPrec _ a = [(convert a, \"\")]", "",
     "convert :: String -> " ++ typeName,
     "convert sym =", 
     "   fromMaybe (error (\"Could not convert symbol \" ++ sym))",
     "             (Map.lookup sym convmap)","",
     "convmap :: Map String " ++ typeName, "convmap = Map.fromList ["] ++)
   . (++ ["   ]"])
   . converter

{--
*Main> take 12 (converts nm (BDM.toList $ table syms)) ~>
["instance Read Symbol where",
 "   readsPrec _ a = [(convert a, \"\")]","",
 "convert :: String -> Symbol",
 "convert sym =",
 "   fromMaybe (error (\"Could not convert symbol \" ++ sym))",
 "             (Map.lookup sym convmap)","",
 "convmap :: Map String Symbol",
 "convmap = Map.fromList [",
 "   (\"AA\",S0) , (\"AAPL\",S1) , (\"ABB\",S2) , (\"ABBV\",S3) , (\"ABEV\",S4)",
 "   , (\"ABGB\",S5) , (\"ABX\",S6) , (\"ABY\",S7) , (\"ACAD\",S8) , (\"ACHN\",S9)"]
--}

converter :: [(String, Int)] -> [String]
converter =
   chunkBy 4
   . recombineWith id (map (", " ++))
   . map (second (U . symId) >>> show)

{--
gives a well-formatted SymbolTable String -> type-value Map representation

*Main> converter $ sequence ("hi", [1..10]) ~>
["   (\"hi\",S1) , (\"hi\",S2) , (\"hi\",S3) , (\"hi\",S4) , (\"hi\",S5) , (\"hi\",S6)",
 "   , (\"hi\",S7) , (\"hi\",S8) , (\"hi\",S9) , (\"hi\",S10)"]
--}

-- so we need to ensure to have Data.Map and Data.Maybe in the header imports

imports :: [String]
imports = ["import Data.Array","import Data.Map (Map)",
   "import qualified Data.Map as Map","import Data.Maybe (fromMaybe)"]

-- Now, compile the symbols from Graph.ShowsGaps(.Symbols) into a data type
-- called Stock in the module Graph.Stocks. Verify everything's peachy-keen
-- by loading that module into your Haskell REPL.

-- Tomorrow, maybe, we'll rewrite ScoreCard, etc, to be indexed on Ix-values.
