library(tidyverse)

# circles only
raw_rural_df = read_csv("power_rural.csv") %>%
    janitor::clean_names() %>%
    rename(locality = charges) %>%
    mutate(pop = str_remove_all(pop, ",")) %>%
    mutate(
        across(c(pop, lit), as.numeric),
    ) %>%
    filter(!is.na(pop)) %>%
    filter(!is.na(lit)) %>%
    select(
        district,
        locality,
        pop,
        lit
    ) %>%
    filter(!(district ==  "09523"))


# has non-circles
raw_urban_df = read_csv("power_urban.csv") %>%
    janitor::clean_names() %>%
    rename(locality = charges) %>%
    filter(str_detect(locality, "CIRCLE")) %>%
    select(
        district,
        locality,
        pop,
        lit
    )
read_csv("power_urban.csv") %>%
    select(district) %>%
    unique()


joint_df %>%
    select(district, place) %>%
    unique()
joint_df = bind_rows(
    raw_rural_df %>% select(district, locality, pop, lit) %>% mutate(place = "rural"),
    raw_urban_df %>% select(district, locality, pop, lit) %>% mutate(place = "urban")
) %>%
    mutate(
        unique_id = 1:n(), 
        lit = lit/100,
        lit = pmin(lit + 0.3, 1)
        ) %>%
    mutate(
        locality_var = lit*(1 - lit)*pop
    )

mean_literacy = joint_df %>%
    summarise(mean_lit = mean(lit)) %>%
    pull(mean_lit)


joint_df %>%
    select(district, place) %>%
    unique()


between_variance = joint_df %>%
    group_by(district, place) %>%
    summarise( 
        between_variance = mean((lit - weighted.mean(lit, w = pop))^2)
    ) 

within_variance = joint_df %>%
    group_by(district, place) %>%
    summarise(within_variance = sum(locality_var)/(sum(pop) - n())) 


var_df = inner_join(
    between_variance,
    within_variance
) %>%
    mutate(
        rho = between_variance / (between_variance + within_variance)
    )
var_df %>%
    select(rho)    

rho


## MDE_k

# MDE_k = sqrt(1 + rho(m-1)) * (t_{1-k}, + t_{\alpha/2})(1/p_c + 1/p_t)^0.5 * \sigma/\sqrt(N)
# MDE_k = sqrt(1 + rho(m(CV^2 + 1) - 1)) * (t_{1-k}, + t_{\alpha/2})(1/p_c + 1/p_t)^0.5 * \sigma/\sqrt(N)

pct_child = 27/220*(3/5)
rural_mean_sujawal_block_size = 1950.87*pct_child
rural_mean_karachi_west_block_size = 21186*pct_child






urban_mean_sujawal_block_size = 1424.9*pct_child*6
urban_mean_karachi_west_block_size = 1203.256*pct_child*6
baseline_zero_dose = 0.07
 
# 2 blocks per circle

CV_sujawal = 0.71
CV_karachi_west = 1.62



calculate_simple_MDE = function(rho, 
                                m,
                                kappa,
                                alpha,
                                p_c,
                                p_t,
                                sigma,
                                N){
    DE = sqrt(1 + rho*(m - 1))
    t_bit = (qt(kappa, N - 1) + qt(1 - alpha/2, N - 1 ))
    prop_bit = 1 # ((1/p_c) + (1/p_t))^0.5

    MDE = DE*t_bit*prop_bit*sigma/sqrt(N)
    return(MDE)
}

calculate_cv_mde = function(rho,
                            m,
                            kappa,
                            alpha,
                            p_c,
                            p_t,
                            sigma,
                            N,
                            CV) {
    DE = sqrt(1 + rho*(m*(CV^2 + 1) - 1))
    t_bit = (qt(kappa, N - 1) + qt(1 - alpha/2, N - 1 ))
    prop_bit = 1 # ((1/p_c) + (1/p_t))^0.5

    MDE = DE*t_bit*prop_bit*sigma/sqrt(N)

                            }


#### Urban






urban_CV_karachi_mde = map_dfr(
    seq(from = 1000, to = 5000, length.out = 100),
    ~
    tibble(
        mde = calculate_cv_mde(
            rho = rho,
            m = urban_mean_karachi_west_block_size,
            kappa = 0.8,
            alpha = 0.05,
            p_c = 0.5,
            p_t = 0.5,
            sigma = baseline_zero_dose*(1 - baseline_zero_dose),
            N = .x,
            CV = CV_karachi_west),
        N = .x
    )
)


urban_CV_sujawal_mde = map_dfr(
    seq(from = 1000, to = 5000, length.out = 100),
    ~
    tibble(
        mde = calculate_cv_mde(
            rho = rho,
            m = urban_mean_sujawal_block_size,
            kappa = 0.8,
            alpha = 0.05,
            p_c = 0.5,
            p_t = 0.5,
            sigma = baseline_zero_dose*(1 - baseline_zero_dose),
            N = .x,
            CV = CV_sujawal),
        N = .x
    )
)


urban_simple_karachi_mde = map_dfr(
    seq(from = 1000, to = 5000, length.out = 100),
    ~
    tibble(
        mde = calculate_simple_MDE(
            rho = rho,
            m = urban_mean_karachi_west_block_size,
            kappa = 0.8,
            alpha = 0.05,
            p_c = 0.5,
            p_t = 0.5,
            sigma = baseline_zero_dose*(1 - baseline_zero_dose),
            N = .x),
        N = .x
    )
)


urban_simple_sujawal_mde = map_dfr(
    seq(from = 1000, to = 5000, length.out = 100),
    ~
    tibble(
        mde = calculate_simple_MDE(
            rho = rho,
            m = urban_mean_sujawal_block_size,
            kappa = 0.8,
            alpha = 0.05,
            p_c = 0.5,
            p_t = 0.5,
            sigma = baseline_zero_dose*(1 - baseline_zero_dose),
            N = .x),
        N = .x
    )
)


urban_df = bind_rows(
    urban_simple_karachi_mde %>% 
        mutate(
            place = "karachi",
            ICC = "simple"
        ),
    urban_simple_sujawal_mde %>% 
        mutate(
            place = "sujawal",
            ICC = "simple"
        ),
    urban_CV_sujawal_mde %>% 
        mutate(
            place = "sujawal",
            ICC = "CV"
        ),
    urban_CV_karachi_mde %>% 
        mutate(
            place = "karachi",
            ICC = "CV"
        )
) %>%
    mutate(type = "urban")


urban_df %>%
    ggplot(aes( 
        x = N, 
        y = mde, 
        colour = ICC
    )) +
    geom_point() +
    facet_wrap(~place, ncol = 1) +
    theme_bw() 

urban_simple_karachi_mde %>%
    ggplot(aes(x = N, y = mde)) +
    geom_point() +
    labs(title = "urban karachi simple rho")

urban_simple_sujawal_mde %>%
    ggplot(aes(x = N, y = mde)) +
    geom_point() +
    labs(title = "urban sujawal simple rho")

urban_CV_karachi_mde %>%
    ggplot(aes(x = N, y = mde)) +
    geom_point() +
    labs(title = "urban karachi CV rho")

urban_CV_sujawal_mde %>%
    ggplot(aes(x = N, y = mde)) +
    geom_point() +
    labs(title = "urban sujawal CV rho")





rural_CV_karachi_mde = map_dfr(
    seq(from = 1000, to = 5000, length.out = 100),
    ~
    tibble(
        mde = calculate_cv_mde(
            rho = rho,
            m = rural_mean_karachi_west_block_size,
            kappa = 0.8,
            alpha = 0.05,
            p_c = 0.5,
            p_t = 0.5,
            sigma = 0.05*(1 - 0.05),
            N = .x,
            CV = CV_karachi_west),
        N = .x
    )
)


rural_CV_sujawal_mde = map_dfr(
    seq(from = 1000, to = 5000, length.out = 100),
    ~
    tibble(
        mde = calculate_cv_mde(
            rho = rho,
            m = rural_mean_sujawal_block_size,
            kappa = 0.8,
            alpha = 0.05,
            p_c = 0.5,
            p_t = 0.5,
            sigma = 0.05*(1 - 0.05),
            N = .x,
            CV = rural_CV_sujawal),
        N = .x
    )
)

rural_simple_karachi_mde = map_dfr(
    seq(from = 1000, to = 5000, length.out = 100),
    ~
    tibble(
        mde = calculate_simple_MDE(
            rho = rho,
            m = rural_mean_karachi_west_block_size,
            kappa = 0.8,
            alpha = 0.05,
            p_c = 0.5,
            p_t = 0.5,
            sigma = 0.05*(1 - 0.05),
            N = .x),
        N = .x
    )
)


rural_simple_sujawal_mde = map_dfr(
    seq(from = 1000, to = 5000, length.out = 100),
    ~
    tibble(
        mde = calculate_simple_MDE(
            rho = rho,
            m = rural_mean_sujawal_block_size,
            kappa = 0.8,
            alpha = 0.05,
            p_c = 0.5,
            p_t = 0.5,
            sigma = 0.05*(1 - 0.05),
            N = .x),
        N = .x
    )
)


rural_simple_karachi_mde %>%
    ggplot(aes(x = N, y = mde)) +
    geom_point()

rural_simple_sujawal_mde %>%
    ggplot(aes(x = N, y = mde)) +
    geom_point()

rural_CV_karachi_mde %>%
    ggplot(aes(x = N, y = mde)) +
    geom_point()

rural_CV_sujawal_mde %>%
    ggplot(aes(x = N, y = mde)) +
    geom_point()
