
## Install standard packages
install.packages(c('devtools', 'data.table', 'pals', 'openxlsx', 'archive'))

## Install custom packages from github
devtools::install_github('tbrycekelly/Plankline-Processing-Scripts')
devtools::install_github('tbrycekelly/TheSource')


# test that the new packages load:
library(TheSource)
library(PlanklinePS)