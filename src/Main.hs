{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Grammar
import EqRel

import Control.Monad ( when )
import Data.List ( intercalate, groupBy, sortBy, isInfixOf )
import Data.Maybe ( fromMaybe, mapMaybe )
import qualified Data.Set as S
import qualified Data.Map as M

import System.Console.CmdArgs hiding ( name, args )
import qualified System.Console.CmdArgs as A
import System.FilePath.Posix ( takeFileName )
import System.IO ( stdout, hSetBuffering, BufferMode(..) )

data GfTest
  = GfTest
  { grammar       :: Maybe FilePath
  -- Languages
  , lang          :: Lang

  -- Functions and cats
  , function      :: Name
  , category      :: Cat
  , tree          :: String
  , start_cat     :: Maybe Cat
  , show_cats     :: Bool
  , show_funs     :: Bool
  , funs_of_arity :: Maybe Int
  , show_coercions:: Bool
  , show_contexts :: String
  , concr_string  :: String

  -- Information about fields
  , equal_fields  :: Bool
  , empty_fields  :: Bool
  , unused_fields :: Bool
  , erased_trees  :: Bool

  -- Compare to old grammar
  , old_grammar   :: Maybe FilePath
  , only_changed_cats :: Bool
  , only_lexicon :: Bool

 -- Misc
  , treebank      :: Maybe FilePath
  , count_trees   :: Maybe Int
  , debug         :: Bool
  , write_to_file :: Bool

  } deriving (Data,Typeable,Show,Eq)

gftest = GfTest
  { grammar       = def &= typFile      &= help "Path to the grammar (PGF) you want to test"
  , lang          = def &= A.typ "\"Eng Swe\""
                                        &= help "Concrete syntax + optional translations"
  , tree          = def &= A.typ "\"UseN tree_N\""
                        &= A.name "t"   &= help "Test the given tree"
  , function      = def &= A.typ "UseN"
                        &= A.name "f"   &= help "Test the given function(s)"
  , category      = def &= A.typ "NP"
                        &= A.name "c"   &= help "Test all functions with given goal category"
  , start_cat     = def &= A.typ "Utt"
                        &= A.name "s"   &= help "Use the given category as start category"
  , concr_string  = def &= A.typ "the"  &= help "Show all functions that include given string"
  , show_cats     = def                 &= help "Show all available categories"
  , show_funs     = def                 &= help "Show all available functions"
  , funs_of_arity = def &= A.typ "2"    &= help "Show all functions of arity 2"
  , show_coercions= def                 &= help "Show coercions in the grammar"
  , show_contexts = def &= A.typ "8140"  &= help "Show contexts for a given concrete type or a range of concrete types (given as FIds)"
  , debug         = def                 &= help "Show debug output"
  , equal_fields  = def &= A.name "q"   &= help "Show fields whose strings are always identical"
  , empty_fields  = def &= A.name "e"   &= help "Show fields whose strings are always empty"
  , unused_fields = def                 &= help "Show fields that never make it into the top category"
  , erased_trees  = def &= A.name "r"   &= help "Show trees that are erased"
  , treebank      = def &= typFile
                        &= A.name "b"   &= help "Path to a treebank"
  , count_trees   = def &= A.typ "3"    &= help "Number of trees of size <3>"
  , old_grammar   = def &= typFile
                        &= A.name "o"   &= help "Path to an earlier version of the grammar"
  , only_changed_cats = def             &= help "When comparing against an earlier version of a grammar, only test functions in categories that have changed between versions"
  , only_lexicon      = def             &= help "When comparing against an earlier version of a grammar, only test lexical categories"
  , write_to_file = def                 &= help "Write the results in a file (<GRAMMAR>_<FUN>.org)"
  }


main :: IO ()
main = do
 hSetBuffering stdout NoBuffering

 args <- cmdArgs gftest

 case grammar args of
  Nothing -> putStrLn "Usage: `gftest -g <PGF grammar> [OPTIONS]'\nTo see available commands, run `gftest --help' or visit https://github.com/GrammaticalFramework/gftest#readme"
  Just fp -> do
    let (absName,grName) = (takeFileName $ stripPGF fp, stripPGF fp ++ ".pgf") --doesn't matter if the name is given with or without ".pgf"

        (langName:langTrans) = case lang args of
                                 []    -> [ absName ++ "Eng" ] -- if no English grammar found, it will be given a default value later
                                 langs -> [ absName ++ t | t <- words langs ]

    -- Read grammar and translations
    gr      <- readGrammar langName grName
    grTrans <- sequence [ readGrammar lt grName | lt <- langTrans ]

    -- if language given by the user was not valid, use default language from Grammar
    let langName = concrLang gr

    let startcat = startCat gr `fromMaybe` start_cat args

        testTree' t n = testTree False gr grTrans t n ctxs
         where
          s    = top t
          c    = snd (ctyp s)
          -- cs   = c:[ coe --TODO fix: generates way too much
          --          | (cat,coe) <- coercions gr
          --          , c == cat ]
          ctxs = concat [ contextsFor gr sc c
                        | sc <- ccats gr startcat ]
                        -- , cat <- cs ]

        output = -- Print to stdout or write to a file
         if write_to_file args
           then \x ->
             do let fname = concat [ langName, "_", function args, category args, ".org" ]
                writeFile fname x
                putStrLn $ "Wrote results in " ++ fname
           else putStrLn


        intersectConcrCats cats_fields intersection =
          M.fromListWith intersection
                ([ (c,fields)
                | (CC (Just c) _,fields) <- cats_fields
                ] ++
                [ (cat,fields)
                | (c@(CC Nothing _),fields) <- cats_fields
                , (CC (Just cat) _,coe) <- coercions gr
                , c == coe
                ])

        printStats tab =
          sequence_ [ do putStrLn $ "==> " ++ c ++ ": "
                         putStrLn $ unlines (map (fs!!) xs)
                    | (c,vs) <- M.toList tab
                    , let fs = fieldNames gr c
                    , xs@(_:_) <- [ S.toList vs ] ]
 -----------------------------------------------------------------------------
 -- Testing functions

    -- Test a tree
    let trees = case tree args of
         [] -> []
         ts -> [ readTree gr t | t <- lines ts ]
    output $
      unlines [ testTree' tree 1 | tree <- trees ]

    -- Test a function
    let substrs xs = filter (/="*") $ groupBy (\a b -> a/='*' && b/='*') xs
    let cats = case category args of
         [] -> []
         cs -> if '*' `elem` cs
                then let subs = substrs cs
                      in nub [ cat | (cat,_,_,_) <- concrCats gr
                             , all (`isInfixOf` cat) subs ]
                else words cs

    output $
      unlines [ testTree' t n
              | cat <- case old_grammar args of
                         Nothing -> cats
                         Just _  -> [] -- if -c is given with -o, don't print out everything
              , (t,n) <- treesUsingFun gr (functionsByCat gr cat) `zip` [1..]]

    -- Test all functions in a category
    let funs = case function args of
         [] -> []
         fs -> if '*' `elem` fs
                then let subs = substrs fs
                     in nub [ f | s <- symbols gr, let f = show s
                            , all (`isInfixOf` f) subs
                            , arity s >= 1 ]
                 else words fs
    output $
      unlines [ testFun (debug args) gr grTrans startcat f
              | f <- case old_grammar args of
                       Nothing -> funs
                       Just _  -> [] -- if -f is given with -o, don't print out everything
              ]

-----------------------------------------------------------------------------
-- Information about the grammar

    -- Show contexts for a particular concrete category
    let printCtx fid = do print fid
                          mapM_ print
                            [ ctx dummyHole
                            | start <- ccats gr startcat
                            , ctx <- contextsFor gr start (mkCC gr fid) ]
    case show_contexts args of
      "" -> return ()
      xs -> case map read $ words xs of
              (bg:end:_) -> mapM_ printCtx [bg..end]
              [fid]      -> printCtx fid
              _          -> putStrLn "--show-contexts: single category (10) or a range (\"10 100\") expected"

    -- Show available categories
    when (show_cats args) $ do
      putStrLn "* Categories in the grammar:"
      let concrcats = sortBy (\(_,a,_,_) (_,b,_,_) -> a `compare` b) (concrCats gr)
      sequence_ [ do putStrLn cat
                     when (debug args) $
                       putStrLn $ unwords $
                         [ "    Compiles to concrete" ] ++
                         [ "categories " ++ show bg++"—"++show end
                         | bg/=end ] ++
                         [ "category   " ++ show bg
                         | bg==end ] ++
                         [ "\n\t\t\t\t    " ++ show (1 + (end-bg)) ++ " categories with " ++ (show $ length lbls) ++ " labels" ]
                | (cat,bg,end,lbls) <- concrcats
                , end >= 0]

    -- Show available functions
    when (show_funs args) $ do
      let printfun = if (debug args) then showConcrFun gr else show
      putStrLn "* Functions in the grammar:"
      putStrLn $ unlines $ nub [ printfun s | s <- symbols gr ]

    -- Show coercions in the grammar
    when (show_coercions args) $ do
      putStrLn "* Coercions in the grammar:"
      putStrLn $ unlines [ show cat++"--->"++show coe | (cat,coe) <- coercions gr ]

    case funs_of_arity args of
      Nothing -> return ()
      Just n -> do
        putStrLn $ "* Functions in the grammar of arity " ++ show n ++ ":"
        putStrLn $ unlines $
         if (debug args)
           then [ showConcrFun gr s | s <- symbols gr, arity s == n ]
           else nub [ show s | s <- symbols gr, arity s == n ]


    -- Show all functions that contain the given string
    -- (e.g. English "it" appears in DefArt, ImpersCl, it_Pron, …)
    case concr_string args of
      []  -> return ()
      str -> do putStrLn $ "### The following functions contain the string '" ++ str ++ "':"
                putStr "==> "
                putStrLn $ intercalate ", " $ nub [ name s | s <- hasConcrString gr str]

    -- Show empty fields
    when (empty_fields args) $ do
      putStrLn "### Empty fields:"
      printStats $ intersectConcrCats (emptyFields gr) S.intersection
      putStrLn ""

    -- Show erased trees
    when (erased_trees args) $ do
      putStrLn "* Erased trees:"
      sequence_
        [ do putStrLn ("** " ++ intercalate "," erasedTrees ++ " : " ++ uncoerceAbsCat gr c)
             sequence_
               [ do putStrLn ("- Tree:  " ++ showTree t)
                    putStrLn ("- Lin:   " ++ s)
                    putStrLn $ unlines
                      [ "- Trans: "++linearize tgr t
                      | tgr <- grTrans ]
               | t <- ts
               , let s = linearize gr t
               , let erasedSymbs = [ sym | sym <- flatten t, c==snd (ctyp sym) ]
               ]
        | top <- take 1 $ ccats gr startcat
        , (c,ts) <- forgets gr top
        , let erasedTrees =
                concat [ [ showTree subtree
                         | sym <- flatten t
                         , let csym = snd (ctyp sym)
                         , c == csym || coerces gr c csym
                         , let Just subtree = subTree sym t ]
                       | t <- ts ]
        ]
      putStrLn ""

    -- Show unused fields
    when (unused_fields args) $ do

      let unused =
           [ (c,S.fromList notUsed)
           | tp <- ccats gr startcat
           , (c,is) <- reachableFieldsFromTop gr tp
           , let ar = head $
                  [ length (seqs f)
                  | f <- symbols gr, snd (ctyp f) == c ] ++
                  [ length (seqs f)
                  | (b,a) <- coercions gr, a == c
                  , f <- symbols gr, snd (ctyp f) == b ]
                 notUsed = [ i | i <- [0..ar-1], i `notElem` (S.toList is) ]
           , not (null notUsed)
           ]
      putStrLn "### Unused fields:"
      printStats $ intersectConcrCats unused S.intersection
      putStrLn ""

    -- Show equal fields
    let tab = intersectConcrCats (equalFields gr) (/\)
    when (equal_fields args) $ do
      putStrLn "### Equal fields:"
      sequence_
       [ putStrLn ("==> " ++ c ++ ":\n" ++ cl)
       | (c,eqr) <- M.toList tab
       , let fs = fieldNames gr c
       , cl <- case eqr of
                 Top -> ["TOP"]
                 Classes xss -> [ unlines (map (fs!!) xs)
                                | xs@(_:_:_) <- xss ]
       ]
      putStrLn ""

    case count_trees args of
      Nothing -> return ()
      Just n  -> do let start = head $ ccats gr startcat
                    let i = featCard gr start n
                    let iTot = sum [ featCard gr start m | m <- [1..n] ]
                    putStr   $ "There are "++show iTot++" trees up to size "++show n
                    putStrLn $ ", and "++show i++" of exactly size "++show n++".\nFor example: "
                    putStrLn $ "* " ++ show (featIth gr start n 0)
                    putStrLn $ "* " ++ show (featIth gr start n (i-1))


-------------------------------------------------------------------------------
-- Read trees from treebank.

    treebank' <-
      case treebank args of
        Nothing -> return []
        Just fp -> do
          tb <- readFile fp
          return [ readTree gr s
                 | s <- lines tb ]
    mapM_ print treebank'

-------------------------------------------------------------------------------
-- Comparison with old grammar

    case old_grammar args of
      Nothing -> return ()
      Just fp -> do
        oldgr <- readGrammar langName (stripPGF fp ++ ".pgf")
        let ogr = oldgr { concrLang = concrLang oldgr ++ "-OLD" }
            difcats = diffCats ogr gr -- (acat, [#o, #n], olabels, nlabels)

        --------------------------------------------------------------------------
        -- generate statistics of the changes in the concrete categories
        let ccatChangeFile = langName ++ "-ccat-diff.org"
        writeFile ccatChangeFile ""
        sequence_
          [ appendFile ccatChangeFile $ unlines
             [ "* " ++ acat
             , show o ++ " concrete categories in the old grammar,"
             , show n ++ " concrete categories in the new grammar."
             , "** Labels only in old (" ++ show (length ol) ++ "):"
             , intercalate ", " ol
             , "** Labels only in new (" ++ show (length nl) ++ "):"
             , intercalate ", " nl ]
          | (acat, [o,n], ol, nl) <- difcats ]
        when (debug args) $
          sequence_
            [ appendFile ccatChangeFile $
              unlines $
                ("* All concrete cats in the "++age++" grammar:"):
                [ show cts | cts <- concrCats g ]
            | (g,age) <- [(ogr,"old"),(gr,"new")] ]

        putStrLn $ "Created file " ++ ccatChangeFile

      --------------------------------------------------------------------------
      -- Print out tests for all functions:
      -- If --only-changed-cats given, test only the changed cats.
      -- If --only-lexicon given, test only lexical functions.
      -- If -f, -c or --treebank specified, use them.

        let f cat = (cat, treesUsingFun gr $ functionsByCat gr cat)

            byCat   = [ f cat | cat <- cats ] -- from command line arg -c
            changed = [ f cat | (cat,_,_,_) <- difcats
                      , only_changed_cats args ]
            lexical = [ (cat, treesUsingFun gr [s])
                      | s <- symbols gr
                      , only_lexicon args -- the flag --only-lexicon was given
                      , arity s == 0  -- is a lexical category
                      , let cat = snd $ Grammar.typ s ]
            byFun   = [ (cat, treesUsingFun gr fs)
                      | funName <- funs -- comes from command line arg -f
                      , let fs@(s:_) = lookupSymbol gr funName
                      , let cat = snd $ Grammar.typ s ]
            fromTb  = [ (cat,[tree]) | tree <- treebank'++trees
                      , let (CC (Just cat) _) = ccatOf tree ]

            treesToTest =
              case concat [byFun, byCat, changed, fromTb, lexical] of
                [] -> [ f cat  -- nothing else specified -> test all functions
                      | (cat,_,_,_) <- concrCats gr ]
                xs -> S.toList $ S.fromList xs

            writeLinFile file grammar otherGrammar = do
              writeFile file ""
              putStrLn "Testing functions in… "
              diff <- concat `fmap`                  -- print every item
                sequence [ do let cs = [ compareTree (only_lexicon args) grammar otherGrammar grTrans startcat t
                                       | t <- ttrees ]
                              putStr $ cat ++ "                \r"
                              -- prevent lazy evaluation; make printout accurate
                              appendFile ("/tmp/"++file) (unwords $ map show cs)
                              let nub' = S.toList . S.fromList
                              return [ Comparison f (nub' ex) | Comparison f ex@(x:xs) <- cs ]
                         | (cat,ttrees) <- treesToTest ]

              let shorterTree c1 c2 = length (funTree c1) `compare` length (funTree c2)
              writeFile file $ unlines
                [ show comp
                | comp <- sortBy shorterTree diff ]

        writeLinFile (langName ++ "-lin-diff.org") gr ogr
        putStrLn $ "Created file " ++ (langName ++ "-lin-diff.org")

        ---------------------------------------------------------------------------
        -- Print statistics about the functions: e.g., in the old grammar,
        -- all these 5 functions used to be in the same category:
        -- [DefArt,PossPron,no_Quant,this_Quant,that_Quant]
        -- but in the new grammar, they are split into two:
        -- [DefArt,PossPron,no_Quant] and [this_Quant,that_Quant].
        let groupFuns grammar = -- :: Grammar -> [[Symbol]]
              concat [ groupBy sameCCat $ sortBy compareCCat funs
                     | (cat,_,_,_) <- difcats
                     , let funs = functionsByCat grammar cat ]

            sortByName = sortBy (\s t -> name s `compare` name t)
            writeFunFile groupedFuns file grammar = do
              writeFile file ""
              sequence_ [ do appendFile file "---\n"
                             appendFile file $ unlines
                               [ showConcrFun gr fun
                               | fun <- sortByName funs ]
                        | funs <- groupedFuns ]

        writeFunFile (groupFuns ogr) (langName ++ "-old-funs.org") ogr
        writeFunFile (groupFuns gr)  (langName ++ "-new-funs.org") gr

        putStrLn $ "Created files " ++ langName ++ "-(old|new)-funs.org"


 where

  nub = S.toList . S.fromList

  sameCCat :: Symbol -> Symbol -> Bool
  sameCCat s1 s2 = snd (ctyp s1) == snd (ctyp s2)

  compareCCat :: Symbol -> Symbol -> Ordering
  compareCCat s1 s2 = snd (ctyp s1) `compare` snd (ctyp s2)

  stripPGF :: String -> String
  stripPGF s = case reverse s of
                'f':'g':'p':'.':name -> reverse name
                name                 -> s
