# CodeBook

This code book describes the data and the computational environment.

## Data

TODO: Add description

Data is included in git repo because it is light-weight and in git-friendly format.

## R Variables

TODO: Add main R variables

## Code

TODO: Code file descriptions

* All analysis code files are located at `./analysis/`

## Libraries

The following R packages are required for running the code or paper generation.

* [bookdown](https://bookdown.org/): Tex generation from R markdown files.
* [tidyverse](https://www.tidyverse.org/): R packages for data science
  * ggplot2
  * dplyr

## R Session Info

Provides an example of the environment used to generate the analysis.

```R
R version 3.5.1 (2018-07-02)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.3 LTS

Matrix products: default
BLAS: /home/stuart/anaconda3/lib/R/lib/libRblas.so
LAPACK: /home/stuart/anaconda3/lib/R/lib/libRlapack.so

locale:
[1] en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] caret_6.0-80         lattice_0.20-35      ggthemes_4.0.0       corrplot_0.84        gplots_3.0.1        
 [6] RColorBrewer_1.1-2   gridExtra_2.3        GGally_1.4.0         forcats_0.3.0        stringr_1.3.1       
[11] dplyr_0.7.6          purrr_0.2.5          readr_1.1.1          tidyr_0.8.1          tibble_1.4.2        
[16] ggplot2_3.0.0        tidyverse_1.2.1      knitr_1.20           RevoUtils_11.0.1     RevoUtilsMath_11.0.0

loaded via a namespace (and not attached):
 [1] nlme_3.1-137       bitops_1.0-6       lubridate_1.7.4    dimRed_0.1.0       httr_1.3.1         rprojroot_1.3-2   
 [7] tools_3.5.1        backports_1.1.2    R6_2.2.2           rpart_4.1-13       KernSmooth_2.23-15 lazyeval_0.2.1    
[13] colorspace_1.3-2   nnet_7.3-12        withr_2.1.2        tidyselect_0.2.4   compiler_3.5.1     cli_1.0.0         
[19] rvest_0.3.2        xml2_1.2.0         labeling_0.3       caTools_1.17.1.1   scales_0.5.0       sfsmisc_1.1-2     
[25] DEoptimR_1.0-8     robustbase_0.93-2  digest_0.6.15      rmarkdown_1.10     pkgconfig_2.0.1    htmltools_0.3.6   
[31] highr_0.7          rlang_0.2.1        readxl_1.1.0       ddalpha_1.3.4      rstudioapi_0.7     bindr_0.1.1       
[37] jsonlite_1.5       gtools_3.8.1       ModelMetrics_1.1.0 magrittr_1.5       Matrix_1.2-14      Rcpp_0.12.18      
[43] munsell_0.5.0      abind_1.4-5        stringi_1.2.4      yaml_2.2.0         MASS_7.3-50        plyr_1.8.4        
[49] recipes_0.1.3      grid_3.5.1         pls_2.6-0          gdata_2.18.0       crayon_1.3.4       splines_3.5.1     
[55] haven_1.1.2        hms_0.4.2          pillar_1.3.0       stats4_3.5.1       reshape2_1.4.3     codetools_0.2-15  
[61] CVST_0.2-2         magic_1.5-8        glue_1.3.0         evaluate_0.11      modelr_0.1.2       foreach_1.4.4     
[67] cellranger_1.1.0   gtable_0.2.0       reshape_0.8.7      kernlab_0.9-26     assertthat_0.2.0   DRR_0.0.3         
[73] gower_0.1.2        prodlim_2018.04.18 broom_0.5.0        survival_2.42-6    rsconnect_0.8.8    class_7.3-14      
[79] geometry_0.3-6     timeDate_3043.102  RcppRoll_0.3.0     iterators_1.0.10   lava_1.6.2         bindrcpp_0.2.2    
[85] ipred_0.9-6       
```

## Other Dependencies

Paper was generated with TeX (requires MiKTeX on Windows, MacTeX 2013+ on OS X, or Tex Live 2013+ on Linux)
