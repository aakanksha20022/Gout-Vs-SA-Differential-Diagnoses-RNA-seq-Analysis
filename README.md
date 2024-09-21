#GoutVSSepticArthritis
---
Gout and Septic Arthritis have very similar clinical representations, making it difficult to differentially diagnose one from the other. This analysis attempts to establish a genetic basis for 
this and discover genes which are differentially expressed in both conditions. The dataset includes:

•	The table of expression values by Gene (row) and Sample (column)
•	A sample information sheet, listing important clinical information about each sample
•	An annotation file – linking gene ID to gene name.
•	Two differential files (log2fold, p, adjusted p), comparing Healthy to Gout and Healthy to SA. 

The code performs summary statitics on major variables such as Neutrophil levels, Monocyte levels, Age and Sex and using simple parametric tests like Anova, t-tests and linear regression and 
between healthy, Gout and SA samples.

It also performs differential gene expression analysis using the RNA-seq expression data.

The information is visualised with a combination of PCA plots, density plots, scatter plots, bar graphs etc.

