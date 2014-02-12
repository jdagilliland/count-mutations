== count-mutations ==

Gregory W. Schwartz

Takes a fasta file in the format of ">>Germline header\nGermline
sequence\n>Mutant header\nMutant sequence...etc" and generates the
mutation counts of the clones from each listed germline in the file per
position in a dataframe type format. Contains flags to bias for or
against silent or replacement mutations, along with only including
codons with certain number of mutations.

Compiling instructions:
Requires the Haskell Platform (sudo apt-get install haskell-platform)
Requires Options.Applicative (cabal update && cabal install
optparse-applicative)
Requires Data.List.Split (cabal update && cabal install split)
Compiling command: ghc --make /path/to/Main.hs -o ./mutation_count -O2

Help: ./mutation_count -h
