# Create DGP functions for simulation based calibration


meta_gen_data <- function(gen_data_args) {
    function(seed) {
        set.seed(seed + 189)
        num_obs = gen_data_args$num_obs
        num_clusters = gen_data_args$num_clusters
        num_counties = gen_data_args$num_counties
        num_treatments = gen_data_args$num_treatments
        num_discrete_dist = gen_data$num_discrete_dist

        # Note implies clusters approx equal size - no skewing here.
        obs_cluster_id = sample(1:num_clusters, size = num_obs, replace = TRUE)
        clust_county_id = sample(1:num_counties, size = num_counties, replace = TRUE)


    }

}
