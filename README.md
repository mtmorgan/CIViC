# CIViCdb / R interface

This provides a start on a low-level (not so user friendly) API to the
[CIViC][] Clinical Interpretations of Variants in Cancer data base.

Install with

    devtools::install_github("mtmorgan/CIViC")

Use with

    library(CIViC)
    example(genes)
    example(variants)
    
and actually, at the moment, `genes()` and `variants()` are the only
functions implemented.

[CIViC]: https://civic.genome.wustl.edu/#/home
