// generated with brms 2.16.1
functions {
  /* zero-inflated binomial log-PDF of a single response 
   * Args: 
   *   y: the response value 
   *   trials: number of trials of the binomial part
   *   theta: probability parameter of the binomial part
   *   zi: zero-inflation probability
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real zero_inflated_binomial_lpmf(int y, int trials, 
                                   real theta, real zi) {
    if (y == 0) { 
      return log_sum_exp(bernoulli_lpmf(1 | zi), 
                         bernoulli_lpmf(0 | zi) + 
                         binomial_lpmf(0 | trials, theta)); 
    } else { 
      return bernoulli_lpmf(0 | zi) +  
             binomial_lpmf(y | trials, theta); 
    } 
  }
  /* zero-inflated binomial log-PDF of a single response 
   * logit parameterization of the zero-inflation part
   * Args: 
   *   y: the response value 
   *   trials: number of trials of the binomial part
   *   theta: probability parameter of the binomial part 
   *   zi: linear predictor for zero-inflation part 
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real zero_inflated_binomial_logit_lpmf(int y, int trials, 
                                         real theta, real zi) {
    if (y == 0) { 
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi), 
                         bernoulli_logit_lpmf(0 | zi) + 
                         binomial_lpmf(0 | trials, theta)); 
    } else { 
      return bernoulli_logit_lpmf(0 | zi) +  
             binomial_lpmf(y | trials, theta); 
    } 
  }
  /* zero-inflated binomial log-PDF of a single response 
   * logit parameterization of the binomial part
   * Args: 
   *   y: the response value 
   *   trials: number of trials of the binomial part
   *   eta: linear predictor for binomial part 
   *   zi: zero-inflation probability
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real zero_inflated_binomial_blogit_lpmf(int y, int trials, 
                                          real eta, real zi) {
    if (y == 0) { 
      return log_sum_exp(bernoulli_lpmf(1 | zi), 
                         bernoulli_lpmf(0 | zi) + 
                         binomial_logit_lpmf(0 | trials, eta)); 
    } else { 
      return bernoulli_lpmf(0 | zi) +  
             binomial_logit_lpmf(y | trials, eta); 
    } 
  }
  /* zero-inflated binomial log-PDF of a single response 
   * logit parameterization of the binomial part
   * logit parameterization of the zero-inflation part
   * Args: 
   *   y: the response value 
   *   trials: number of trials of the binomial part
   *   eta: linear predictor for binomial part 
   *   zi: linear predictor for zero-inflation part 
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real zero_inflated_binomial_blogit_logit_lpmf(int y, int trials, 
                                                real eta, real zi) {
    if (y == 0) { 
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi), 
                         bernoulli_logit_lpmf(0 | zi) + 
                         binomial_logit_lpmf(0 | trials, eta)); 
    } else { 
      return bernoulli_logit_lpmf(0 | zi) +  
             binomial_logit_lpmf(y | trials, eta); 
    } 
  }
  // zero-inflated binomial log-CCDF and log-CDF functions
  real zero_inflated_binomial_lccdf(int y, int trials, real theta, real zi) { 
    return bernoulli_lpmf(0 | zi) + binomial_lccdf(y | trials, theta); 
  }
  real zero_inflated_binomial_lcdf(int y, int trials, real theta, real zi) { 
    return log1m_exp(zero_inflated_binomial_lccdf(y | trials, theta, zi));
  }
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int trials[N];  // number of trials
  vector<lower=0>[N] weights;  // model weights
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> K_zi;  // number of population-level effects
  matrix[N, K_zi] X_zi;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_zi_1;
  vector[N] Z_2_zi_2;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  int Kc_zi = K_zi - 1;
  matrix[N, Kc_zi] Xc_zi;  // centered version of X_zi without an intercept
  vector[Kc_zi] means_X_zi;  // column means of X_zi before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
  for (i in 2:K_zi) {
    means_X_zi[i - 1] = mean(X_zi[, i]);
    Xc_zi[, i - 1] = X_zi[, i] - means_X_zi[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  vector[Kc_zi] b_zi;  // population-level effects
  real Intercept_zi;  // temporary intercept for centered predictors
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_1] r_1_2;  // actual group-level effects
  vector[N_2] r_2_zi_1;  // actual group-level effects
  vector[N_2] r_2_zi_2;  // actual group-level effects
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_1_2 = (sd_1[2] * (z_1[2]));
  r_2_zi_1 = (sd_2[1] * (z_2[1]));
  r_2_zi_2 = (sd_2[2] * (z_2[2]));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + Xc * b;
    // initialize linear predictor term
    vector[N] zi = Intercept_zi + Xc_zi * b_zi;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      zi[n] += r_2_zi_1[J_2[n]] * Z_2_zi_1[n] + r_2_zi_2[J_2[n]] * Z_2_zi_2[n];
    }
    for (n in 1:N) {
      target += weights[n] * (zero_inflated_binomial_blogit_logit_lpmf(Y[n] | trials[n], mu[n], zi[n]));
    }
  }
  // priors including constants
  target += student_t_lpdf(Intercept | 3, 0, 2.5);
  target += logistic_lpdf(Intercept_zi | 0, 1);
  target += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 2 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_1[2]);
  target += student_t_lpdf(sd_2 | 3, 0, 2.5)
    - 2 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_2[2]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // actual population-level intercept
  real b_zi_Intercept = Intercept_zi - dot_product(means_X_zi, b_zi);
}

