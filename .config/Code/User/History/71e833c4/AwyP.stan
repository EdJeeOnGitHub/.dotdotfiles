cov_matrix[2] Sigma;
cholesky_factor_cov[2] L;
vector<lower=0>[2] tau = 2.5*tan(tau_unif);
L = diag_pre_multiply(tau, L_Omega);
Sigma = L * L';