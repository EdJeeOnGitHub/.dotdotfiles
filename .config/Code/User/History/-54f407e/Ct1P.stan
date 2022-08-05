cov_matrix[2] Sigma;
Sigma[1, 1] = sigma_y^2;
Sigma[2, 2] = sigma_h^2;
Sigma[1, 2] = sigma_y*sigma_h*rho;
Sigma[2, 1] = sigma_y*sigma_h*rho;
