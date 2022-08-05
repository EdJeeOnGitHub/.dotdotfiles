#!/bin/bash


FAKEDATA="--fake-data"


echo ${FAKEDATA}
# Clean Data
Rscript ./code/clean-raw-data.R ${FAKEDATA} &
Rscript ./code/clean-town-randomisation.R 
wait

if [ $FAKEDATA = "--fake-data" ]
then
    Rscript ./scratch/create-fake-data.R 
fi 

# Merge in data
Rscript ./code/merge-zm-town-data.R ${FAKEDATA}

# Raw plots
Rscript ./code/create-raw-data-plots.R ${FAKEDATA} --time-aggregation-level=month &
Rscript ./code/create-raw-data-plots.R ${FAKEDATA} --time-aggregation-level=day &
Rscript ./code/create-raw-data-plots.R ${FAKEDATA} --time-aggregation-level=week
wait

# Estimate ATTs
Rscript ./code/estimate-att-gt.R ${FAKEDATA} \
         --time-aggregation-level=month \
         --unit-aggregation-level=town & 
Rscript ./code/estimate-att-gt.R ${FAKEDATA} \
         --time-aggregation-level=week \
         --unit-aggregation-level=town
wait

# Postprocess ATTs
Rscript ./code/generate-es-estimates.R ${FAKEDATA} --time-aggregation-level=week \
    --unit-aggregation-level=town 