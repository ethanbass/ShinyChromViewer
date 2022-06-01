# ShinyChromViewer

## Overview

ShinyChromViewer is an Shiny gadget for interactive viewing and exploration of 3D chromatographic data (e.g. from HPLC-DAD). It is mainly meant to be used in conjunction with [chromatographR](https://ethanbass.github.io/chromatographR) as an add-on.

## Installation

`ShinyChromViewer` can be installed from GitHub as follows:

```
install.packages("devtools")
devtools::install_github("https://github.com/ethanbass/ShinyChromViewer/")
```

## Usage

To run `ShinyChromConverter` you should have a list of chromatograms in `matrix` format. You can also provide `peak_table` object produced by `chromatographR`. The script below loads the example data from `chromatographR` and launches the `ShinyChromViewer`. 

```
library(chromatographR)
library(ShinyChromViewer)
data(Sa_warp)
data(pk_tab)
chrom_viewer(pk_tab)
```

To view spectra, click on the trace plot (*lower left panel*) or the peak table (*right panel*).

<img src="man/images/shinyChromViewerUI.png"></img>
