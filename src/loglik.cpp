// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

//' Loglikelihood function for Escribano2012 model a
//' @param theta A parameter vector
//' @param price A price vector
//' @param week_dum A week dummy matrix
//' @export
// [[Rcpp::export]]
double loglik_a(arma::vec theta, arma::vec price, arma::mat week_dum) {
  
  int n = price.size();
    
  arma::vec t = arma::zeros<vec>(n);
  for (int i = 0; i < n; ++i) {
    t[i] = i + 1;
  }
  
  arma::vec s = theta[0] + theta[1] * t +
    theta[2] * sin((t + theta[3]) * 2 * datum::pi / 365) +
    theta[4] * sin((t + theta[5]) * 4 * datum::pi / 365) +
    week_dum * theta(arma::span(6, 11));
  
  arma::vec x = price - s;
  
  arma::vec x_sum = arma::zeros<vec>(n);
  arma::vec h = arma::zeros<vec>(n);
  h[6] = theta(19);
  arma::vec e = arma::zeros<vec>(n);
  arma::vec e_1 = arma::zeros<vec>(n);
  arma::vec e_2 = arma::zeros<vec>(n);
  
  double part_1 = 0;
  double part_2 = 0;
  
  arma::vec log_l = arma::zeros<vec>(n);

  for (int j = 7; j < n; j++) {
    x_sum[j] = arma::as_scalar(theta(arma::span(12, 18)).t() *
      x(arma::span(j - 7, j - 1)));
    h[j] = theta[19] + theta[20] * h[j - 1] +
      theta[21] * pow(e[j - 1], 2);
    e_1[j] = (x[j] / (1 - theta[22]) - x_sum[j]) / sqrt(h[j]);
    e_2[j] = (x[j] / theta[22] - x_sum[j] - sqrt(h[j]) *
      e_1[j] - theta[23]) / theta[24];
    e[j] = (1 - theta[22]) * e_1[j] + theta[22] * e_2[j];
    
    part_1 = exp(-pow(price[j] - s[j] - x_sum[j] - theta[23], 2) /
                   (2 * (h[j] + theta[24])) *
                     1 / sqrt(2 * datum::pi * (h[j] + theta[24])));
    part_2 = exp(-pow(price[j] - s[j] - x_sum[j], 2) / (2 * h[j]) * 1 / 
                   sqrt(2 * datum::pi * h[j]));
    log_l[j] = log(theta[22] * part_1 + (1 - theta[22]) * part_2);
  }
  
  arma::vec ret_log_l = log_l.subvec(7, n - 1);
  double sum_log_l = -arma::sum(ret_log_l);
  
  return sum_log_l;
}

//' Loglikelihood function for Escribano2012 model b
//' @param theta A parameter vector
//' @param price A price vector
//' @param week_dum A week dummy matrix
//' @export
// [[Rcpp::export]]
arma::vec loglik_a_sim(arma::vec theta, arma::vec price, arma::vec week_dum) {
  
  int n = price.size();
  
  double B0 = theta[0];
  double BT = theta[1];
  double C1 = theta[2];
  double C2 = theta[3];
  double C3 = theta[4];
  double C4 = theta[5];
  double  D1 = theta[6];
  
  arma::vec phi = theta(arma::span(7, 13));
  
  double  omega = theta[14];
  double  alpha = theta[15];
  double  beta = theta[16];
  
  double lambda = theta[17];
  double mu = theta[18];
  double sigma = theta[19];
  
  arma::vec t = arma::zeros<vec>(n);
  for (int i = 0; i < n; ++i) {
    t[i] = i + 1;
  }
  
  arma::vec s = B0 + BT * t + C1 * sin((t + C2) * 2 * datum::pi / 365) +
    C3 * sin((t + C4) * 4 * datum::pi / 365) + D1 * week_dum;
  
  arma::vec x = price - s;
  
  arma::vec x_sum = arma::zeros<vec>(n);
  arma::vec h = arma::zeros<vec>(n);
  h[6] = omega;
  arma::vec e = arma::zeros<vec>(n);
  arma::vec e_1 = arma::zeros<vec>(n);
  arma::vec e_2 = arma::zeros<vec>(n);
  
  double part_1 = 0;
  double part_2 = 0;
  
  arma::vec log_l = arma::zeros<vec>(n);
  
  for (int j = 7; j < n; j++) {
    x_sum[j] = arma::as_scalar(phi.t() * x(arma::span(j - 7, j - 1)));
    h[j] = omega + alpha * pow(e[j - 1], 2) + beta * h[j - 1];
    e_1[j] = (x[j] / (1 - lambda) - x_sum[j]) / sqrt(h[j]);
    e_2[j] = (x[j] / lambda - x_sum[j] - sqrt(h[j]) * e_1[j] - mu) / sigma;
    e[j] = (1 - lambda) * e_1[j] + lambda * e_2[j];
    
    part_1 = exp(-pow(price[j] - s[j] - x_sum[j] - mu, 2) /
                   (2 * (h[j] + pow(sigma, 2)))) *
      1 / sqrt(2 * datum::pi * (h[j] + pow(sigma, 2)));
    part_2 = exp(-pow(price[j] - s[j] - x_sum[j], 2) / (2 * h[j])) * 
      1 / sqrt(2 * datum::pi * h[j]);
    log_l[j] = log(lambda * part_1 + (1 - lambda) * part_2);
  }
  
  arma::vec ret_log_l = log_l.subvec(7, n - 1);
  //double sum_log_l = arma::sum(ret_log_l);
  
  return ret_log_l;
}