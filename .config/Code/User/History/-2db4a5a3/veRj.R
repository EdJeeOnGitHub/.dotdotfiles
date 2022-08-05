cluster_treatment_map = matrix(
    NA,
    ncol = 2,
    nrow = 8
)


cluster_treatment_map[, 1] = rep(1:4, 2)
cluster_treatment_map[, 2] = rep(1:2, each = 4)

cluster_treatment_map

cluster_assigned_dist_group_treatment = sample(
    1:()
)