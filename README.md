# stockCIs
Bootstrapping for CIs of Fish Stocks

`stockCIs` is an R package that performs simple bootstrapping of fish stock data (perhaps obtained from PBT or GSI) to generate confidence intervals around fish stock proportion point estimates. 

## Getting Started

To install `stockCIs` you can use Hadley Wickham's `devtools` package. To install and load the `devtools` package use:
```
install.packages("devtools")
library(devtools)
```
NOTE: To use `devtools`, you may also have to download and install Rtools (although you shouldn't). The latest version on Rtools can be found at
https://cran.r-project.org/bin/windows/Rtools/

Once `devtools` is successfully installed, use the following to install stockCIs:
```
devtools::install_github("mackerman44/stockCIs")
```

If you are interested in making contributions to stockCIs, consider getting a GitHub account, fork this repository, clone to a local directory, modify, and send me a pull request. I can then review any changes and merge.

For examples of how `resampit()` and `bootstock()` have been used to generate CIs for stock composition estimates please refer to the following publications.

Byrne, A., J. Hymer, S. Ellis, R. Dick II, K. Keller, C.A. Steele, J.E. Hess, M. Begay, and T. Miller. 2016. A Genetic Analysis of the Summer Stock Composition in the Columbia River and Snake River Tribal and Sport Fisheries. IDFG Report Number 16-104. Available at: https://collaboration.idfg.idaho.gov/FisheriesTechnicalReports/16-104ResSteelhead%20stock%20comp%202014.pdf

Hess, J.E., M.W. Ackerman, J.K. Fryer, D.J. Hasselman, C.A. Steele, J.J. Stephenson, J.M. Whiteaker, and S.R. Narum. 2016. Differential Adult Migration-Timing and Stock-Specific Abundance of Steelhead in Mixed Stock Assemblages. ICES Journal of Marine Science. doi:10.1093/icesjms/fsw138

For questions or comments please contact mackerman@qcinc.org

Enjoy!
