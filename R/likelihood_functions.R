#' Model estimation for Escribano2012 model a
#' 
#' @export
#'
log_lik_model_a <- function(input_frame_s, input_frame_i,
                            path = NULL,
                            country = "de") {
  
  input_frame_s %<>% 
    filter(date < "2013-01-01") %>%
    filter_only_outliers() %>% 
    mutate(week_dum = ifelse(format(date, "%a") == "Sat" |
                               format(date, "%a") == "Sun", 0, 1))
  input_frame_i %<>% 
    filter(date < "2013-01-01") %>%
    filter_only_outliers() %>% 
    mutate(week_dum = ifelse(format(date, "%a") == "Sat" |
                               format(date, "%a") == "Sun", 0, 1))
  
  tmp_file_s <- paste0(tempfile(), ".csv")
  tmp_file_i <- paste0(tempfile(), ".csv")
  
  write.table(input_frame_s[, 2:3] %>% round(4), tmp_file_s, sep = ",", row.names = FALSE,
              col.names = FALSE)
  write.table(input_frame_i[, 2:3] %>% round(4), tmp_file_i, sep = ",", row.names = FALSE,
              col.names = FALSE)
  
  Matlab$startServer()
  matlab <- Matlab()
  open(matlab)
  
  setOption(matlab, "readResult/interval", 10);
  setOption(matlab, "readResult/maxTries", 30*(60/10));
  
  evaluate(matlab, paste0("input_frame_s = csvread('", tmp_file_s, "');"))
  evaluate(matlab, paste0("input_frame_i = csvread('", tmp_file_i, "');"))
  evaluate(matlab, "theta_s = ones(20, 1) * 0.2;")
  evaluate(matlab, "theta_s(1) = mean(input_frame_s(:, 1));")
  if (country == "fr") evaluate(matlab, "theta_s(20) = 2;")
  evaluate(matlab, "theta_i = ones(20, 1) * 0.2;")
  evaluate(matlab, "theta_i(1) = mean(input_frame_i(:, 1));")
  evaluate(matlab, "theta_i(20) = 2;")
  
  evaluate(matlab,
           "[param_s, hessian_s] = optim_model_1(theta_s, input_frame_s);")
  evaluate(matlab,
           "[param_i, hessian_i] = optim_model_1(theta_i, input_frame_i);")
  
  mat_out <- getVariable(matlab, c("param_s", "hessian_s", "param_i",
                                   "hessian_i"))
  
  close(matlab)
  
  out_s <- data.frame(Parameter = c("$\\kappa_0$", "$\\kappa_1$","$\\kappa_2$",
                                   "$\\kappa_3$", "$\\kappa_4$", "$\\kappa_5$",
                                   "$\\kappa_6$", "$\\phi_1$", "$\\phi_2$",
                                   "$\\phi_3$", "$\\phi_4$", "$\\phi_5$",
                                   "$\\phi_6$", "$\\phi_7$", "$\\omega$",
                                   "$\\alpha$", "$\\beta$", "$\\lambda$",
                                   "$\\mu$", "$\\sigma$"),
                      Estimate = mat_out$param.s,
                      Std.Error = sqrt(diag(solve(mat_out$hessian.s)))) %>%
    mutate(t.statistic = Estimate / Std.Error,
           p.value = (2 * pt(abs(t.statistic),
                            nrow(input_frame_s) - length(mat_out$param.s) - 1,
                            lower = FALSE)) %>% round(4))
  
  out_i <- data.frame(Parameter = c("$\\kappa_0$", "$\\kappa_1$","$\\kappa_2$",
                                    "$\\kappa_3$", "$\\kappa_4$", "$\\kappa_5$",
                                    "$\\kappa_6$", "$\\phi_1$", "$\\phi_2$",
                                    "$\\phi_3$", "$\\phi_4$", "$\\phi_5$",
                                    "$\\phi_6$", "$\\phi_7$", "$\\omega$",
                                    "$\\alpha$", "$\\beta$", "$\\lambda$",
                                    "$\\mu$", "$\\sigma$"),
                      Estimate = mat_out$param.i,
                      Std.Error = sqrt(diag(solve(mat_out$hessian.i)))) %>%
    mutate(t.statistic = Estimate / Std.Error,
           p.value = (2 * pt(abs(t.statistic),
                             nrow(input_frame_i) - length(mat_out$param.i) - 1,
                             lower = FALSE)) %>% round(4))
  
  out <- rbind(out_s, out_i)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_escribano2012_a_fit.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' Model estimation for Escribano2012 model b
#' 
#' @export
#'
log_lik_model_b <- function(input_frame_s, input_frame_i,
                            path = NULL,
                            country = "de") {
  
  input_frame_s %<>% 
    filter(date < "2013-01-01") %>%
    filter_only_outliers() %>% 
    mutate(week_dum = ifelse(format(date, "%a") == "Sat" |
                               format(date, "%a") == "Sun", 0, 1),
           seas_1 = ifelse(as.numeric(format(date, "%m")) %in% c(9, 10, 11), 1, 0),
           seas_2 = ifelse(as.numeric(format(date, "%m")) %in% c(3, 4, 5), 1, 0),
           seas_3 = ifelse(as.numeric(format(date, "%m")) %in% c(6, 7, 8), 1, 0)) %>%
    left_join(input_frame_exp, by = c("date")) %>%
    left_join(input_frame_exp_1, by = c("date"))
  
  input_frame_i %<>% 
    filter(date < "2013-01-01") %>%
    filter_only_outliers() %>% 
    mutate(week_dum = ifelse(format(date, "%a") == "Sat" |
                               format(date, "%a") == "Sun", 0, 1),
           seas_1 = ifelse(as.numeric(format(date, "%m")) %in% c(9, 10, 11), 1, 0),
           seas_2 = ifelse(as.numeric(format(date, "%m")) %in% c(3, 4, 5), 1, 0),
           seas_3 = ifelse(as.numeric(format(date, "%m")) %in% c(6, 7, 8), 1, 0)) %>%
    left_join(input_frame_exp, by = c("date")) %>%
    left_join(input_frame_exp_1, by = c("date"))
  
  tmp_file_s <- paste0(tempfile(), ".csv")
  tmp_file_i <- paste0(tempfile(), ".csv")
  
  write.table(input_frame_s[, 2:6], tmp_file_s, sep = ",", row.names = FALSE,
              col.names = FALSE)
  write.table(input_frame_i[, 2:6], tmp_file_i, sep = ",", row.names = FALSE,
              col.names = FALSE)
  
  Matlab$startServer()
  matlab <- Matlab()
  open(matlab)
  
  setOption(matlab, "readResult/interval", 10);
  setOption(matlab, "readResult/maxTries", 30*(60/10));
  
  evaluate(matlab, paste0("input_frame_s = csvread('", tmp_file_s, "');"))
  evaluate(matlab, paste0("input_frame_i = csvread('", tmp_file_i, "');"))
  evaluate(matlab, "theta_s = ones(23, 1) * 0.2;")
  if (country == "fr") evaluate(matlab, "theta_s(23) = 2;")
  evaluate(matlab, "theta_i = ones(23, 1) * 0.2;")
  evaluate(matlab, "theta_i(1) = mean(input_frame_i(:, 1));")
  evaluate(matlab, "theta_i(23) = 2;")
  
  evaluate(matlab,
           "[param_s, hessian_s] = optim_model_2(theta_s, input_frame_s);")
  evaluate(matlab,
           "[param_i, hessian_i] = optim_model_2(param_s, input_frame_i);")
  
  mat_out <- getVariable(matlab, c("param_s", "hessian_s", "param_i",
                                   "hessian_i"))
  
  test <- getVariable(matlab, "theta_s")
  
  close(matlab)
  
  out_s <- data.frame(Parameter = c("$\\kappa_0$", "$\\kappa_1$","$\\kappa_2$",
                                    "$\\kappa_3$", "$\\kappa_4$", "$\\kappa_5$",
                                    "$\\kappa_6$", "$\\phi_1$", "$\\phi_2$",
                                    "$\\phi_3$", "$\\phi_4$", "$\\phi_5$",
                                    "$\\phi_6$", "$\\phi_7$", "$\\omega$",
                                    "$\\alpha$", "$\\beta$", "$\\eta_1$",
                                    "$\\eta_2$", "$\\eta_3$", "$\\eta_4$",
                                    "$\\mu$", "$\\sigma$"),
                      Estimate = mat_out$param.s,
                      Std.Error = sqrt(diag(solve(mat_out$hessian.s)))) %>%
    mutate(t.statistic = Estimate / Std.Error,
           p.value = (2 * pt(abs(t.statistic),
                             nrow(input_frame_s) - length(mat_out$param.s) - 1,
                             lower = FALSE)) %>% round(4))
  
  out_i <- data.frame(Parameter = c("$\\kappa_0$", "$\\kappa_1$","$\\kappa_2$",
                                    "$\\kappa_3$", "$\\kappa_4$", "$\\kappa_5$",
                                    "$\\kappa_6$", "$\\phi_1$", "$\\phi_2$",
                                    "$\\phi_3$", "$\\phi_4$", "$\\phi_5$",
                                    "$\\phi_6$", "$\\phi_7$", "$\\omega$",
                                    "$\\alpha$", "$\\beta$", "$\\eta_1$",
                                    "$\\eta_2$", "$\\eta_3$", "$\\eta_4$",
                                    "$\\mu$", "$\\sigma$"),
                      Estimate = mat_out$param.i,
                      Std.Error = sqrt(diag(solve(mat_out$hessian.i)))) %>%
    mutate(t.statistic = Estimate / Std.Error,
           p.value = (2 * pt(abs(t.statistic),
                             nrow(input_frame_i) - length(mat_out$param.i) - 1,
                             lower = FALSE)) %>% round(4))
  
  out <- rbind(out_s, out_i)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_escribano2012_a_fit.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' Model estimation for Escribano2012 model c
#' 
#' @export
#'
log_lik_model_c <- function(input_frame_s, input_frame_i,
                            path = NULL,
                            country = "de") {
  
  input_frame_exp %<>% 
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load)) %>%
    mutate(residual_load = residual_load - lag(residual_load, 1))
  
  data_frame <- data_de_spot %>%
    filter(date < "2013-01-01") %>%
    filter_only_outliers() %>% 
    mutate(week_dum = ifelse(format(date, "%a") == "Sat" |
                               format(date, "%a") == "Sun", 0, 1),
           seas_1 = ifelse(format(date, "%a") == "Sat" |
                             format(date, "%a") == "Sun", 0, 1)) %>%
    left_join(input_frame_exp, by = c("date"))
  
  tmp_file <- paste0(tempfile(), ".csv")
  write.table(data_frame[, 2:3], tmp_file, sep = ",", row.names = FALSE,
              col.names = FALSE)
  
  Matlab$startServer()
  matlab <- Matlab()
  open(matlab)
  
  setOption(matlab, "readResult/interval", 10);
  setOption(matlab, "readResult/maxTries", 30*(60/10));
  
  evaluate(matlab, paste0("data_de_spot = csvread('", tmp_file, "')"))
  evaluate(matlab, "theta = ones(20, 1) * 0.2;")
  evaluate(matlab, "theta(1) = mean(data_de_spot(:, 1));")
  
  evaluate(matlab, "[param, hessian] = optim_model_1(theta, data_de_spot)")
  mat_out <- getVariable(matlab, c("param", "hessian"))
  
  close(matlab)
  
  out <- data.frame(Parameter = c("$\\kappa_0$", "$\\kappa_1$","$\\kappa_2$",
                                  "$\\kappa_3$", "$\\kappa_4$", "$\\kappa_5$",
                                  "$\\kappa_6$", "$\\phi_1$", "$\\phi_2$",
                                  "$\\phi_3$", "$\\phi_4$", "$\\phi_5$",
                                  "$\\phi_6$", "$\\phi_7$", "$\\omega$",
                                  "$\\alpha$", "$\\beta$", "$\\lambda$",
                                  "$\\mu$", "$\\sigma$"),
                    Estimate = mat_out$param,
                    Std.Error = sqrt(diag(solve(mat_out$hessian)))) %>%
    mutate(t.statistic = Estimate / Std.Error,
           p.value = (2 * pt(abs(t.statistic),
                             nrow(data_frame) - length(mat_out$param) - 1,
                             lower = FALSE)) %>% round(4))
  
  
}