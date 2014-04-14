# count-mutations

**Gregory W. Schwartz**

Takes a fasta file in the format of ">>Germline header\nGermline
sequence\n>Mutant header\nMutant sequence...etc" and generates the
mutation counts of the clones from each listed germline in the file per
position in a dataframe type format. Contains flags to bias for or
against silent or replacement mutations, along with only including
codons with certain number of mutations.

To install:
```
cabal update
cabal install
```

```
count-mutations, Gregory W. Schwartz

Usage: count-mutations [-t|--inputMutType [Silent]|Replacement] [-b|--inputBias [Silent]|Replacement] [-c|--inputCodonMut [0]|1|2|3] [-m|--inputMutCount [1]|2|3|...] [-N|--removeN] [-i|--input FILE] [-o|--output FILE]
  Return the mutation counts with certain biases from the  germline to the mutants within clones

Available options:
  -h,--help                Show this help text
  -t,--inputMutType [Silent]|Replacement The type of mutation to be counted
  -b,--inputBias [Silent]|Replacement The type of mutation to bias for when calculating intermediate codons
  -c,--inputCodonMut [0]|1|2|3 Only count mutations from codons with this many mutations (0 is the same as include all codons)
  -m,--inputMutCount [1]|2|3|... Only count a unique mutation if it appears this many or more times
  -N,--removeN             Whether to remove N or n in the sequence
  -i,--input FILE          The name of the input fasta file
  -o,--output FILE         The name of the output file with the counts
```
