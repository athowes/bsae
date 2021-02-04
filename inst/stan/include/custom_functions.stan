real multi_normal_prec_improper_lpdf(vector x, vector mu, matrix Q) {
  return(- 0.5 * (x - mu)' * Q * (x - mu));
}

real xbinomial_logit_lpdf(real y, real m, real eta) {
  real dens = lchoose(m, y) + y * log(inv_logit(eta)) + (m - y) * log(1 - inv_logit(eta));
  real jacobian = log(inv_logit(eta)) + log(inv_logit(-eta)); // Alternative: eta - 2 * log(1 + exp(eta))
  return dens + jacobian;
}

matrix cov_matern32(matrix D, real l) {
  int n = rows(D);
  matrix[n, n] K;
  real norm_K;
  real sqrt3;
  sqrt3 = sqrt(3.0);
  
  for(i in 1:(n - 1)){
    // Diagonal entries (apart from the last)
    K[i, i] = 1;
    for(j in (i + 1):n){
      // Off-diagonal entries
      norm_K = D[i, j] / l;
      K[i, j] = (1 + sqrt3 * norm_K) * exp(-sqrt3 * norm_K); // Fill lower triangle
      K[j, i] = K[i, j]; // Fill upper triangle
    }
  }
  K[n, n] = 1;
  
  return(K);
}

matrix cov_sample_average(matrix S, real l, int n, int[] start_index, int[] sample_lengths, int total_samples) {
  matrix[n, n] K;
  matrix[total_samples, total_samples] kS = cov_matern32(S, l);
  
  for(i in 1:(n - 1)) {
    // Diagonal entries (apart from the last)
    int start_i = start_index[i];
    int length_i = sample_lengths[i];
    K[i, i] = mean(block(kS, start_i, start_i, length_i, length_i));
    for(j in (i + 1):n) {
      // Off-diagonal entries
      K[i, j] = mean(block(kS, start_i, start_index[j], length_i, sample_lengths[j]));
      K[j, i] = K[i, j];
    }
  }
  K[n, n] = mean(block(kS, start_index[n], start_index[n], sample_lengths[n], sample_lengths[n]));
  
  return(K);
}