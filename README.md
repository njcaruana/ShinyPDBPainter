
<p align="center">
  <h1><strong>ShinyPDBPainter</strong> - Package to 'Paint' PDB structures with proteomics logfc intensities. 
</h1>
</p>


## Description

**ShinyPDBPainter** is an add on to ShinyNGLVieweR by [nvelden](https://github.com/nvelden) provides an R interface to the [NGL.js](http://nglviewer.org/ngl/api/) JavaScript library. It can be used to visualize and interact with protein data bank (PDB) and structural files in R and Shiny applications. It includes a set of API functions to manipulate the viewer after creation and makes it possible to retrieve data from the visualization into R.

Please note: This is a version of nveldens NGLVieweR with an updated NGL.js file due to compatibility issues with the removal of the PDB legacy files. 
This to ensure the package ShinyPDBPainter correctly works and this version of NGLvieweR will be removed once the original is updated. 

## Installation

Current R version tested - 4.5.1 

Install the current development version from [GitHub](https://github.com/) with:

``` {.r}
install.packages("remotes")
remotes::install_github("njcaruana/ShinyPDBPainter")
```

## Basics

To run the app, run the code: 
``` {.r}
library(ShinyPDBPainter)
run_app()
```

