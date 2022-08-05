library(did)



# generate dataset with 4 time periods
time.periods <- 4

# generate dynamic effects
te.e <- time.periods:1

# generate data set with these parameters
# (main thing: it generates a dataset that satisfies
# parallel trends in all periods...including pre-treatment)
data <- build_sim_dataset()

head(data)