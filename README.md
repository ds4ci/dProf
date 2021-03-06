
<!-- README.md is generated from README.Rmd. Please edit that file -->
dProf - A Data Quality Profiler
-------------------------------

"Don't waste my time with garbage data sets!" When you are given a new data set, the first thing to do is a quick data quality scan. This package is inspired by Jack Olson's *Data Quality: The Accuracy Dimension*; as was in initial attempt presented at useR! 2004. This version leverages the advances in R and the Hadleyverse. More importantly the profiling and presentation functionality has been decoupled giving us the ability to optimize each for particular data sources and needs.

This version initially implements column level profiling. In other words, each column is profiled independently. Upcoming releases will add between column profiling. Also on the roadmap are a DBMS SQL optmized profiling module and a compact profile presentation following what was done in 2004.

See the vignette, `dProf_Workflow.Rmd` to get started.

Version 0.1.0 - an early beta!
