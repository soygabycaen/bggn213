# class06_hw
Georgina Canto-Encalada (A59021295)

Function for the analysis of protein drug interactions

``` r
library(bio3d)
#Define a function named 'protein'. Inputs: ID and chain_name with a default 
#chain set as "A"
protein <- function(ID, chain_name="A") {
  # Read the PDB (Protein Data Bank) file specified by the ID
  s <- read.pdb(ID)

  # Trim the PDB structure to only include the specified chain and only CA 
  #(alpha carbon) atoms
  s.chain <- trim.pdb(s, chain=chain_name, elety="CA")

  # Extract the B-factor (atomic displacement parameter) values from the 
  # trimmed PDB structure
  s.b <- s.chain$atom$b

  # Plot the B-factor values along the protein sequence
  # 'sse' argument stands for secondary structure elements, which are derived 
  # from the trimmed chain
  # 'typ="l"' specifies the type of plot as a line plot
  # 'ylab="Bfactor"' sets the label for the y-axis as "Bfactor"
  plotb3(s.b, sse=s.chain, typ="l", ylab="Bfactor")
}
```

``` r
ID="4AKE"
protein(ID,"A")
```

      Note: Accessing on-line PDB file

![](class06_hw_files/figure-commonmark/unnamed-chunk-2-1.png)

``` r
protein(ID,"B")
```

      Note: Accessing on-line PDB file

    Warning in get.pdb(file, path = tempdir(), verbose = FALSE):
    C:\Users\GABYCA~1\AppData\Local\Temp\RtmpoX8Bm8/4AKE.pdb exists. Skipping
    download

![](class06_hw_files/figure-commonmark/unnamed-chunk-2-2.png)