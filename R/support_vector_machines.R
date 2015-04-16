#' Function basically performing the same task as e1071::tune, but here made
#' clear.
#' @param input_frame A data frame containing a date and a price.
#' @param max.cost An integer containing the maximum allowed cost. If left as
#' NULL, it will default to floor(max(abs(price)) + 3 * sd(price))
#' @param max.epsilon An interger being the max allowed epsilon parameter. If 
#' left as null, it will default to 4, being the value where no support vectors
#' are returned for the base spot price for Germany.
#' @param cachesize cache memory in MB (default 4000)
#' 
#' @export
select_svm <- function(input_frame, input_frame_exp, kernel = "linear",
                       cachesize = 500, cores = 8L, diff = FALSE, exp_v = FALSE,
                       eps_grid = 10 ^ c(0.5, 0.75, 1, 1.25, 1.5)) {
  
  input_frame_exp %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load)) %>% 
    {
      if(diff == TRUE) {
        (.) %>% 
          mutate(residual_load = residual_load - lag(residual_load, 1))
      } else return((.))
    }
  
  input_frame %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  
  cost <- max(input_frame$price)
  
  if (exp_v == FALSE) {
    train_frame <- data.frame(y = input_frame$price,
                              x1 = lag(input_frame$price, 1),
                              x2 = lag(input_frame$price, 2),
                              x3 = lag(input_frame$price, 3),
                              x4 = lag(input_frame$price, 4),
                              x5 = lag(input_frame$price, 5),
                              x6 = lag(input_frame$price, 6),
                              x7 = lag(input_frame$price, 7)) %>%
      na.omit
  } else {
    train_frame <- data.frame(y = input_frame$price,
                              x1 = lag(input_frame$price, 1),
                              x2 = lag(input_frame$price, 2),
                              x3 = lag(input_frame$price, 3),
                              x4 = lag(input_frame$price, 4),
                              x5 = lag(input_frame$price, 5),
                              x6 = lag(input_frame$price, 6),
                              x7 = lag(input_frame$price, 7),
                              x8 = input_frame$residual_load) %>%
      na.omit
  }
  
  cl <- makeCluster(getOption("cl.cores", cores), type = "PSOCK")
  clusterExport(cl,
                varlist = c("train_frame",
                            "cost",
                            "cachesize",
                            "kernel",
                            "eps_grid"),
                envir = environment())
  clusterEvalQ(cl,
               {
                 library(magrittr)
                 library(dplyr)
                 library(e1071)
               })
  cat(paste("\nSent to clusters at:", Sys.time()))
  
  if (kernel == "linear") {
    svm_out <- 
      parLapply(cl, eps_grid, function(e) {
        cat(paste0("Calculating svm regression with linear kernel, ", 7,
                       " lags, ", cost, " cost, and ", e, " epsilon"))
        
        out <- try(svm(y ~ .,
                       data = train_frame,
                       kernel = kernel,
                       scale = TRUE,
                       type = "eps-regression",
                       cost = cost,
                       epsilon = e,
                       cachesize = cachesize)) %>%
                       {
                         if (inherits(., "try-error") %>% `!`) {
                           data.frame(mse = fitted(.) %>%
                                        as.numeric %>%
                                        subtract(train_frame[, 1], .) %>%
                                        raise_to_power(2) %>%
                                        mean,
                                      support_vectors = use_series(., SV) %>%
                                        length)
                         } else {
                           data.frame(mse = NA,
                                      support_vectors = NA)
                         }
                       } %>%
          transmute(kernel = kernel,
                    lags = 7,
                    cost = cost,
                    epsilon = e,
                    mse,
                    support_vectors)
        
        cat(" ... Done\n")
        return(out)
      }) %>% rbind_all
  } else if (kernel == "polynomial") {
    svm_out <- 
      parLapply(cl, eps_grid, function(e) {
        cat(paste0("Calculating svm regression with polynomial kernel ",
                   "of degree, ", 2, " with ", 7, " lags, ", cost,
                   " cost, and ", e, " epsilon"))
        
        out <- try(svm(y ~ .,
                       data = train_frame,
                       kernel = kernel,
                       scale = TRUE,
                       type = "eps-regression",
                       cost = cost,
                       epsilon = e,
                       gamma = 1,
                       coef0 = 1,
                       degree = 2,
                       cachesize = cachesize)) %>%
                       {
                         if (inherits(., "try-error") %>% `!`) {
                           data.frame(mse = fitted(.) %>%
                                        as.numeric %>%
                                        subtract(train_frame[, 1], .) %>%
                                        raise_to_power(2) %>%
                                        mean,
                                      support_vectors = use_series(., SV) %>%
                                        length)
                         } else {
                           data.frame(mse = NA,
                                      support_vectors = NA)
                         }
                       } %>%
          transmute(kernel = kernel,
                    lags = 7,
                    cost = cost,
                    epsilon = e,
                    degree = 2,
                    mse,
                    support_vectors)
        
        cat(" ... Done\n")
        return(out)
      }) %>% rbind_all
  }
  
  print(paste("\nCluster calculations done at:", Sys.time()))
  stopCluster(cl)
  
  return(svm_out)
}