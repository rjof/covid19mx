#!/bin/bash
cd ../
/usr/local/bin/R CMD build covid19mx
/usr/local/bin/R CMD check covid19mx_0.1.0.tar.gz 
/usr/local/bin/R CMD build --resave-data
## Or in R
R -e "
setwd('../');
paths <- sort(Sys.glob(c('./covid19mx/data/*.rda')));
tools::resaveRdaFiles(paths = paths);
devtools::build(pkg = 'covid19mx');
devtools::check(pkg = 'covid19mx');
"