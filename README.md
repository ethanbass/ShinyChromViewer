# ShinyChromViewer

## Overview

ShinyChromViewer is a shiny gadget for interactively viewing 3D chromatograms. It is mainly meant to be used in conjunction with [chromatographR](https://ethanbass.github.io/chromatographR) as an add-on.

## Installation

`ShinyChromViewer` can be installed from GitHub as follows:

```
install.packages("devtools")
devtools::install_github("https://github.com/ethanbass/ShinyChromViewer/")
```

## Example

```
library(chromatographR)
library(ShinyChromViewer)
data(Sa_warp)
data(pk_tab)
chrom_viewer(pk_tab)
```
