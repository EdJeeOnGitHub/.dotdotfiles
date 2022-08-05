#!/bin/bash


FAKEDATA="--fake-data"



# Clean Data
Rscript ./code/clean-raw-data.R ${FAKEDATA} &
Rscript ./code/clean-town-randomisation.R 
wait
# Merge in data
Rscript ./code/merge-zm-town-data.R ${FAKEDATA}

# 
Rscript ./code/create-raw-data-plots.R ${FAKEDATA}
