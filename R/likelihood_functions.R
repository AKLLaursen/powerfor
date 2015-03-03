#' Model estimation for Escribano2012 model a
#' 
#' @param theta An atomic vector of coefficients
#' @param price An atomic vector with the price series
#'
log_lik_model_a <- function(input_frame) {
  
  set_data_time()
  
  # Construct week dummies
  week_dum <- input_frame %>%
    transmute(week_day = date %>% as.Date %>% format("%a") %>% as.factor) %>%
    model.matrix(~ week_day - 1,
                 data = .) %>%
    extract(, -2)
  
  # Construct weekend dummies
  week_dum <- input_frame %>%
    transmute(weekend = date %>%
                as.Date %>%
                format("%a")) %>%
    mutate(weekend = ifelse(weekend == "Sat" | weekend == "Sun", 1, 0)) %>%
    as.matrix
  
  theta <- rep(0.1, 20)
  
  loglik_a_sim(theta, input_frame$price, week_dum)
  
  b = rep(0, 5)
  A = matrix(0, 5, length(theta))
  A[1, 16] = 1
  A[2, 17] = 1
  A[3, 18] = 1
  A[4, 17:18] = -1
  A[5, 17:18] = 1
  b[4] = 1
  
  max1 <- maxLik(loglik_a_sim, start = max$estimate, price = input_frame$price, week_dum = week_dum,
         constraints = list(ineqA = A,
                            ineqB = b),
         method = "BFGS", iterlim = 100000, print.level = 2, tol = 1e-20) %>% summary
  
  max <- maxLik(loglik_a_sim, start = theta, price = input_frame$price, week_dum = week_dum,
         method = "BHHH", iterlim = 100000, print.level = 2, steptol = 1e-20) %>% summary
  
  constrOptim(theta, loglik_a, A, b, control = list(trace = TRUE, maxit = 100000))
  
  maxLik(loglik_a, start = theta, price = input_frame$price, week_dum = week_dum,
         method = "NM", iterlim = 100000, print.level = 2)
  
  return(short_seas_fit)
}