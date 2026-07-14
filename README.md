# README

Repository accompanying the paper "If you build it, will they come? Modeling digital language use: a longitudinal panel analysis of demographic demand and content availability".

The code facilitates reproducability of the results of the paper. The data used for the analysis where accessed at: 

- Wikipedia Pageview data: https://zenodo.org/records/18526766
- Finland speaker data: https://zenodo.org/records/20133764
- Wikipedia Support data: https://zenodo.org/records/18742003


First restore the environment using renv::restore(). The data wrangling script is in `processing`. Scripts for visualization and modelling are in `analysis`. The scripts are not dependent on one another.
