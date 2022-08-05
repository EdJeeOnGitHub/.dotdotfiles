#!/bin/bash


FAKEDATA="--fake-data"
UNITAGGREGATIONLEVEL="town"


echo "Running with ${FAKEDATA} at the $UNITAGGREGATIONLEVEL level."
# Clean Data
# Rscript ./code/clean-raw-data.R ${FAKEDATA} &
# Rscript ./code/clean-town-randomisation.R 
# wait

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
         --unit-aggregation-level=${UNITAGGREGATIONLEVEL} & 
Rscript ./code/estimate-att-gt.R ${FAKEDATA} \
         --time-aggregation-level=week \
         --unit-aggregation-level=${UNITAGGREGATIONLEVEL}
wait

# Postprocess ATTs
Rscript ./code/generate-es-estimates.R ${FAKEDATA} --time-aggregation-level=week \
    --unit-aggregation-level=${UNITAGGREGATIONLEVEL} 