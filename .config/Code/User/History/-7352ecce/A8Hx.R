# Create DGP functions for simulation based calibration


meta_gen_data = function(gen_data_args) {
    function(seed) {
        set.seed(seed + 189)
        num_obs = gen_data_args$num_obs
        num_clusters = gen_data_args$num_clusters
        num_counties = gen_data_args$num_counties
        num_treatments = gen_data_args$num_treatments
        num_discrete_dist = gen_data$num_discrete_dist

        # Note implies clusters approx equal size - no skewing here.
        obs_cluster_id = sample(1:num_clusters, size = num_obs, replace = TRUE)
        cluster_county_id = sample(1:num_counties, size = num_clusters, replace = TRUE)
        obs_county_id = clust_county_id[obs_cluster_id]

        cluster_treatment_map = matrix(
            NA,
            ncol = 2,
            nrow = num_treatments*num_discrete_dist
            )
        cluster_treatment_map[, 1] = rep(1:num_treatments, num_discrete_dist)
        cluster_treatment_map[, 2] = rep(1:num_discrete_dist, each = num_treatments)

        cluster_assigned_dist_group_treatment = sample(
            1:(num_treatments*num_discrete_dist),
            size = num_clusters,
            replace = TRUE)

        cluster_standard_dist = rnorm(
            n = num_clusters,
            mean = gen_data_args$mean,
            sd = 1
        )

        return(lst(
            num_obs,
            num_clusters,
            num_counties,
            num_treatments,
            num_discrete_dist,
            obs_cluster_id,
            cluster_county_id,
            obs_county_id,
            cluster_treatment_map,
            cluster_assigned_dist_group_treatment,
            cluster_standard_dist
        ))
    }
}
