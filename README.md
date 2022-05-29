# chromViewer

## Overview

chromViewer is a shiny gadget for interactively viewing 3D chromatograms. It is mainly meant as an add-on for [chromatographR](https://ethanbass.github.io/chromatographR).

## Installation

`chromViewer` can be installed from, using `devtools` as follows:

```
install.packages("devtools")
devtools::install_github("https://github.com/ethanbass/chromViewer/")
```

## Example

```
library(chromatographR)
library(chromViewer)
data(Sa_warp)
data(pk_tab)
chrom_viewer(pk_tab)
```
