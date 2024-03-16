# Untitled

``` r
library(bio3d)

s <- read.fasta("A59021295_mutant_seq.fa")


mismatches_pos <- which(s$ali [1,] != s$ali[2,])
aa <- s$ali[,mismatches_pos]


paste(aa[1,],mismatches_pos,aa[2,])
```

    [1] "F 113 V" "H 115 E" "S 121 R" "A 189 Y"
