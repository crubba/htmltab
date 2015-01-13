## htmltab: Hassle-free HTML tables in R
htmltab is a package for extracting structured information from HTML tables. It is similar to readHTMLTable() of the XML package but provides two major advantages. First, the package automatically expands row and column spans in the header and body cells. Second, users are given more control over the identification of header and body rows which will end up in the R table. Additionally, the function preprocesses table code, removes unneeded parts and so helps to alleviate the need for tedious post-processing.

## Installation 
The package is available from CRAN and Github. For the stable release version, download from CRAN:

```
install.packages("htmltab")
```

For the developer version, download from my GitHub repo. You can install the package directly from inside R:

```
install.packages("devtools")
devtools::install_github("crubba/htmltab")
```

## Usage
To see *htmltab* in action, take a look at the case studies in the [package vignette](http://r-datacollection.com/blog/Hassle-free-data-from-HTML-tables-with-the-htmltable-package/), this [blog post](http://r-datacollection.com/blog/htmltab-Next-version-and-CRAN-release/) or consult the package manual.

## Report issues
If you experience problems with *htmltab*, I would like to hear about it to improve the project. Please use [my github repo](https://github.com/crubba/htmltab/issues) to report the issue.
