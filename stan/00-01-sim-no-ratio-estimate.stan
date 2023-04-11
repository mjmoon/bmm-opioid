data {
  // lengths
  int n_time; // number of time periods
  // observations
  int X[n_time]; // observed mortality counts due to opioid
  int Y[n_time]; // observed (survey) treatment admissions from opioid misuse
  int W[n_time]; // observed (survey) illicit opioid initiates
  // priors
  vector[2] gamma0; // log(r0)
  vector[3] delta0; // logit(p0)
  real<lower=0, upper=1> u; // other mortality rate
  // misc
  real s0[2]; // counts at 0
}
parameters {
  vector<lower=0>[2] gamma[n_time]; // {log(r)}
  vector[3] delta[n_time]; // {logit(p)}
  real<lower=0> siggamma;
  real<lower=0> sigdelta;
}
transformed parameters {
  real<lower=0> w_hat[n_time];
  vector<lower=0>[2] r_cnt[n_time];
  vector<lower=0, upper=1>[3] p_mean[n_time];
  matrix<lower=0, upper=1>[4, 4] lambda_trn[n_time]; // state transition probabilities
  vector<lower=0>[4] lambda_cnt[n_time + 1]; // state count means
  for (n in 1:n_time) { 
    r_cnt[n] = exp(gamma[n]);
    p_mean[n] = inv_logit(delta[n]);
  }
  // multi-state model
  for (n in 1:n_time) {
    lambda_trn[n] = rep_matrix(0, 4, 4);
    lambda_trn[n][1, 1] = (1 - p_mean[n][1]) * (1 - u);
    lambda_trn[n][2, 2] = (1 - p_mean[n][2]) * (1 - p_mean[n][3]) * (1 - u);
    lambda_trn[n][1, 2] = p_mean[n][1] * (1 - u);
    lambda_trn[n][2, 3] = p_mean[n][2] * (1 - u);
    lambda_trn[n][2, 4] = (1 - p_mean[n][2]) * p_mean[n][3] * (1 - u);
  }
  // set initial state prevalence
  lambda_cnt[1][1] = s0[1];
  lambda_cnt[1][2] = s0[2];
  lambda_cnt[1][3] = 0;
  lambda_cnt[1][4] = 0;
  for (n in 1:n_time) {
    w_hat[n] = lambda_trn[n][1, 2] * lambda_cnt[n][1] + r_cnt[n][2];
    lambda_cnt[n + 1] = lambda_trn[n]' * lambda_cnt[n] 
                    + append_row(r_cnt[n], rep_vector(0, 2));
  }
}
model {
  // priors
  {}
  // likelihoods
  for (m in 1:n_time) {
    gamma[m] ~ normal(m == 1 ? gamma0 : gamma[m - 1], siggamma);
    delta[m] ~ normal(m == 1 ? delta0 : delta[m - 1], sigdelta);
    target += poisson_lpmf(W[m] | w_hat[m]);
    target += poisson_lpmf(X[m] | lambda_cnt[m + 1][4]);
    target += poisson_lpmf(Y[m] | lambda_cnt[m + 1][3]);
  }
}
generated quantities {
  real x_hat[n_time];
  real y_hat[n_time];
  real r1_hat[n_time];
  real r2_hat[n_time];
  real p1_hat[n_time];
  real p2_hat[n_time];
  real p3_hat[n_time];
  
  for (n in 1:n_time) {
    x_hat[n] = lambda_cnt[n + 1][4];
    y_hat[n] = lambda_cnt[n + 1][3];
    r1_hat[n] = r_cnt[n][1];
    r2_hat[n] = r_cnt[n][2];
    p1_hat[n] = p_mean[n][1];
    p2_hat[n] = p_mean[n][2];
    p3_hat[n] = p_mean[n][3];
  }
}
