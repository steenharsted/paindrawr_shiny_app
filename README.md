# PainDrawR

A Shiny application demonstrating an interactive pain drawing tool.

**Live demo:** https://shinylab.sdu.dk/paindrawr/

## About

This project provides a complete example of building a pain drawing application in Shiny. It demonstrates solutions for:

-   Touch/pointer-enabled canvas drawing
-   Multiple drawing tools (spray, pen)
-   Client-side state management
-   Data export in JSON and PNG formats
-   Schema validation for data integrity

## Installation

``` r
# Install required packages
install.packages(c(
    "shiny", "shinyWidgets", "shinyvalidate", "bslib", "here", 
    "jsonlite", "base64enc", "zip", "jsonvalidate", "rlang"))

## Usage

Run locally with:

```r
shiny::runApp()
```

## Environment Variables

-   `PSK_SAVE_DIR`: Directory for saving files (default: "storage")

## Citation

If you use PainDrawR in your research, please cite:

Harsted, S. (2025). PainDrawR: Interactive Pain Drawing Application. R package version 0.2.0. [https://github.com/steenharsted/paindrawr]