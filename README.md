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

Enjoy!
