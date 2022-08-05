cov_matrix[d+1] Sigma;
cholesky_factor_cov[d+1] L;
vector<lower=0>[d+1] tau = 2.5*tan(tau_unif);
L = diag_pre_multiply(tau, L_Omega);
Sigma = L * L';