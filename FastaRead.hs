-- germline_clone_mutation_count_source
-- FastaRead Module
-- By G.W. Schwartz

-- Collects all functions pertaining to the reading of an input fasta in
-- the format of ">>Germline header\nGermline sequence\n>Mutant
-- header\nMutant sequence...etc".

module FastaRead where

-- Built in
import qualified Data.Map as M

-- Cabal
import qualified Data.List.Split as Split

-- Local
import Types

-- Takes a fasta file string of the format ">>[Germline header]\n[Germline
-- sequence]\n>[Mutant header]\n[Mutant
-- sequence]\n>[MH]\n[MS]...\n>>[GH]\n[GS]\n>[MH]\n[MS]...etc" and returns
-- a CloneMap in order to generate the basic building block for the
-- mutation counting.  Note: Several repeating germlines, so they need
-- a unique identifier (an integer in this case).
generateCloneMap :: String -> CloneMap
generateCloneMap = M.fromList . map getCodonSplit . getSequences
  where
    getCodonSplit ((x1, x2), xs)   = ((x1, fullCodon . Split.chunksOf 3 $ x2),
                                      map (fullCodon . Split.chunksOf 3) xs)
    fullCodon                      = filter ((==3) . length)
    getSequences                   = map filterAssocList . assocList
    filterAssocList ((x1, x2), xs) = ((x1, last . filterHeaders $ x2),
                                      filterHeaders xs)
    filterHeaders                  = filter (\x -> head x /= '>')
    assocList                      = map assocMap . germlineSplit
    assocMap (x, y)                = ((x, germline y), clones y)
    germlineSplit                  = zip [0..]
                                   . filter (/= "")
                                   . Split.splitOn ">>"
    germline                       = take 2 . lines
    clones                         = drop 2 . lines
