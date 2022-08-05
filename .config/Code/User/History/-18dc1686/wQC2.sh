#!/bin/bash


FAKEDATA="--fake-data"


echo ${FAKEDATA}
# Clean Data
Rscript ./code/clean-raw-data.R ${FAKEDATA} &
Rscript ./code/clean-town-randomisation.R 
wait
# Merge in data
Rscript ./code/merge-zm-town-data.R ${FAKEDATA}

# Raw plots
Rscript ./code/create-raw-data-plots.R ${FAKEDATA} --time-aggregation-level=month &
Rscript ./code/create-raw-data-plots.R ${FAKEDATA} --time-aggregation-level=day &
Rscript ./code/create-raw-data-plots.R ${FAKEDATA} --time-aggregation-level=week
wait

# Estimate ATTs
Rscript ./code/estimate-att-gt.R ${FAKEDATA} --time-aggregation-level=month & 
Rscript ./code/estimate-att-gt.R ${FAKEDATA} --time-aggregation-level=week 
wait

# Plot 