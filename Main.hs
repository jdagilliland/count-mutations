-- count-mutations
-- Main
-- By G.W. Schwartz

-- Takes a fasta file in the format of ">>Germline header\nGermline
-- sequence\n>Mutant header\nMutant sequence...etc" and generates the
-- mutation counts of the clones from each listed germline in the file per
-- position in a dataframe type format. Contains flags to bias for or
-- against silent or replacement mutations, along with only including
-- codons with certain number of mutations.

-- Compiling instructions:
-- Requires the Haskell Platform (sudo apt-get install haskell-platform)
-- Requires Options.Applicative (cabal update && cabal install
-- optparse-applicative)
-- Requires Data.List.Split (cabal update && cabal install split)
-- Compiling command: ghc --make /path/to/Main.hs -o ./mutation_count -O2

-- Built in
import qualified Data.Map as M

-- Cabal
import Options.Applicative

-- Local
import Types
import FastaRead
import MutationCount

-- | Command line arguments
data Options = Options { inputMutType  :: MutationType
                       , inputBias     :: Bias
                       , inputCodonMut :: CodonMut
                       , inputMutCount :: MutCount
                       , removeN       :: Bool
                       , input         :: String
                       , output        :: String
                       }

-- | Command line options
options :: Parser Options
options = Options
      <$> option
          ( long "inputMutType"
         <> short 't'
         <> metavar "[Silent]|Replacement"
         <> value Silent
         <> help "The type of mutation to be counted" )
      <*> option
          ( long "inputBias"
         <> short 'b'
         <> metavar "[Silent]|Replacement"
         <> value Silent
         <> help "The type of mutation to bias for when calculating\
                 \ intermediate codons" )
      <*> option
          ( long "inputCodonMut"
         <> short 'c'
         <> metavar "[0]|1|2|3"
         <> value 0
         <> help "Only count mutations from codons with this many mutations\
                 \ (0 is the same as include all codons)" )
      <*> option
          ( long "inputMutCount"
         <> short 'm'
         <> metavar "[1]|2|3|..."
         <> value 1
         <> help "Only count a unique mutation if it appears this many\
                 \ or more times" )
      <*> switch
          ( long "removeN"
         <> short 'N'
         <> help "Whether to remove N or n in the sequence" )
      <*> strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> value ""
         <> help "The name of the input fasta file" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value ""
         <> help "The name of the output file with the counts" )

mutationCounts :: Options -> IO ()
mutationCounts opts = do
    contents    <- readFile . input $ opts
    let mutType  = inputMutType opts
    let bias     = inputBias opts
    let codonMut = inputCodonMut opts
    let mutCount = inputMutCount opts

    -- Get rid of carriages
    let contentsNoCarriages  = filter (/= '\r') $ contents
    -- No newlines in sequence
    let contentsNoNewlines  = joinSeq (removeN opts) contentsNoCarriages

    let cloneMap  = generateCloneMap contentsNoNewlines

    -- Generate the germline to clone map
    let cloneMutMap         = generateCloneMutMap cloneMap

    -- Generate the germline to clone map separated by clones
    let positionCloneMap   = generatePositionCloneMap cloneMutMap

    -- Generate the position to clone map
    let combinedCloneMutMap = M.unionsWith (++) .
                              map snd           .
                              M.toAscList       $
                              cloneMutMap

    -- Save the counts
    writeFile (output opts) $ printMutCounts
        mutType bias codonMut mutCount combinedCloneMutMap positionCloneMap

main :: IO ()
main = execParser opts >>= mutationCounts
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Return the mutation counts with certain biases from the \
                 \ germline to the mutants within clones"
     <> header "count-mutations, Gregory W. Schwartz" )
