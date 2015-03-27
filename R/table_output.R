#' @export
ar_model_bic <- function(input_frame_s, input_frame_i, max.p = 7,
                     path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                     country = "de") {
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  
  calc_bic <- function(model, p) {
    data.frame(p = p,
               aic = -2 * model$loglik + 2 * length(model$coef),
               bic = -2 * model$loglik + length(model$coef) *
                 log(length(model$residuals)),
               hic = -2 * model$loglik + 2 * length(model$coef) *
                 log(log(length(model$residuals))))     
  }
  
  out_s <- lapply(1:max.p, function(x) {
    arima(input_frame_s$price,
          order = c(x, 0, 0),
          optim.control = list(maxit = 2000)) %>% calc_bic(x)
  }) %>%
  rbind_all %>%
  select(p, bic) %>%
  rename(`Spot BIC` = bic)
    
  
  out_i <- lapply(1:max.p, function(x) {
    arima(input_frame_i$price,
          order = c(x, 0, 0),
          optim.control = list(maxit = 2000)) %>% calc_bic(x)
  }) %>%
  rbind_all %>%
  select(bic) %>%
  rename(`Intraday BIC` = bic)
  
  out <- cbind(out_s, out_i)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, paste0("/", country, "_bic_ar.tex")),
            floating = FALSE,
            include.rownames = FALSE,
            sanitize.text.function = function(x){x})
  }
}

#' @export
ar_model_fit <- function(input_frame_s, input_frame_i,
                         path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                         country = "de") {
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  
  arma_model_s <- arima(input_frame_s$price,
                      order = c(7, 0, 0),
                      optim.control = list(maxit = 2000)) %>%
    tidy %>%
    {
      rbind(tail(., 1), head(., -1))
    } %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = (Estimate / Std.Error) %>% round(4),
              p.value = 2 * pt(abs(t.statistic),
                               nrow(input_frame_s) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  arma_model_i <- arima(input_frame_i$price,
                        order = if (country == "de") c(3, 0, 0) else c(1, 0, 0),
                        optim.control = list(maxit = 2000)) %>%
    tidy %>%
    {
      rbind(tail(., 1), head(., -1))
    } %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = (Estimate / Std.Error) %>% round(4),
              p.value = 2 * pt(abs(t.statistic),
                               nrow(input_frame_i) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  arma_model <- rbind(arma_model_s, arma_model_i)
  
  if (!is.null(path)) {
    xtable(arma_model,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_fit_ar.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' @export
arma_model_bic <- function(input_frame_s, input_frame_i, max.p = 7, max.q = 7,
                         path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                         country = "de") {
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  
  calc_bic <- function(model, p, q) {
    data.frame(p = p,
               q = q,
               aic = -2 * model$loglik + 2 * length(model$coef),
               bic = -2 * model$loglik + length(model$coef) *
                 log(length(model$residuals)),
               hic = -2 * model$loglik + 2 * length(model$coef) *
                 log(log(length(model$residuals))))     
  }
  
  out_s <- lapply(1:max.p, function(x) {
    lapply(1:max.q, function(y) {
      arima(input_frame_s$price,
            order = c(x, 0, y),
            method = "ML",
            optim.control = list(maxit = 2000)) %>% calc_bic(x, y)
    }) %>%
      rbind_all 
  }) %>%
    rbind_all %>%
    select(p, q, bic) %>%
    rename(`p/q` = p) %>%
    spread(q, bic)
  
  
  out_i <- lapply(1:max.p, function(x) {
    lapply(1:max.q, function(y) {
      arima(input_frame_i$price,
            order = c(x, 0, y),
            method = "ML",
            optim.control = list(maxit = 2000)) %>% calc_bic(x, y)
    }) %>%
      rbind_all 
  }) %>%
    rbind_all %>%
    select(p, q, bic) %>%
    rename(`p/q` = p) %>%
    spread(q, bic)
  
  out <- rbind(out_s, out_i)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, paste0("/", country, "_bic_arma.tex")),
            floating = FALSE,
            include.rownames = FALSE,
            sanitize.text.function = function(x){x})
  }
}

#' @export
arma_model_fit <- function(input_frame_s, input_frame_i,
                         path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                         country = "de") {
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  
  arma_model_s <- arima(input_frame_s$price,
                        order = c(7, 0, 6),
                        optim.control = list(maxit = 2000)) %>%
    tidy %>%
    {
      rbind(tail(., 1), head(., -1))
    } %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = (Estimate / Std.Error) %>% round(4),
              p.value = 2 * pt(abs(t.statistic),
                               nrow(input_frame_s) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  arma_model_i <- arima(input_frame_i$price,
                        order = if (country == "de") c(7, 0, 7) else c(7, 0, 6),
                        optim.control = list(maxit = 2000)) %>%
    tidy %>%
    {
      rbind(tail(., 1), head(., -1))
    } %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = (Estimate / Std.Error) %>% round(4),
              p.value = 2 * pt(abs(t.statistic),
                               nrow(input_frame_i) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  arma_model <- rbind(arma_model_s, arma_model_i)
  
  if (!is.null(path)) {
    xtable(arma_model,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_fit_arma.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' @export
arx_1_model_bic <- function(input_frame_s, input_frame_i, input_frame_exp, max.p = 7,
                           path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                           country = "de") {
  input_frame_exp %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load))
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  
  calc_bic <- function(model, p) {
    data.frame(p = p,
               aic = -2 * model$loglik + 2 * length(model$coef),
               bic = -2 * model$loglik + length(model$coef) *
                 log(length(model$residuals)),
               hic = -2 * model$loglik + 2 * length(model$coef) *
                 log(log(length(model$residuals))))     
  }
  
  out_s <- lapply(1:max.p, function(x) {
    arima(input_frame_s$price,
          order = c(x, 0, 0),
          xreg = input_frame_s$residual_load,
          method = "ML",
          optim.control = list(maxit = 2000)) %>% calc_bic(x)
  }) %>%
    rbind_all %>%
    select(p, bic) %>%
    rename(`DAy Ahead BIC` = bic)
  
  out_i <- lapply(1:max.p, function(x) {
    arima(input_frame_i$price,
          order = c(x, 0, 0),
          xreg = input_frame_i$residual_load,
          method = "ML",
          optim.control = list(maxit = 2000)) %>% calc_bic(x)
  }) %>%
    rbind_all %>%
    select(bic) %>%
    rename(`Intraday BIC` = bic)
  
  out <- cbind(out_s, out_i)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, paste0("/", country, "_bic_arx_1.tex")),
            floating = FALSE,
            include.rownames = FALSE,
            sanitize.text.function = function(x){x})
  }
}

#' @export
arx_1_model_fit <- function(input_frame_s, input_frame_i, input_frame_exp,
                           path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                           country = "de") {
  
  input_frame_exp %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load))
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  
  arma_model_s <- arima(input_frame_s$price,
                        order = c(7, 0, 0),
                        method = "ML",
                        xreg = input_frame_s$residual_load,
                        optim.control = list(maxit = 2000)) %>% 
    tidy %>%
    {
      rbind(tail(., 2), head(., -2))
    } %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = (Estimate / Std.Error) %>% round(4),
              p.value = 2 * pt(abs(t.statistic),
                               nrow(input_frame_s) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(Parameter == "input_frame_s$residual_load", "$\\eta_1$",
                                      ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                             ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), Parameter))))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  arma_model_i <- arima(input_frame_i$price,
                        order = c(7, 0, 0),
                        method = "ML",
                        xreg = input_frame_i$residual_load,
                        optim.control = list(maxit = 2000)) %>%
    tidy %>%
    {
      rbind(tail(., 2), head(., -2))
    } %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = (Estimate / Std.Error) %>% round(4),
              p.value = 2 * pt(abs(t.statistic),
                               nrow(input_frame_i) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(Parameter == "input_frame_i$residual_load", "$\\eta_1$",
                                      ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                             ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), Parameter))))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  arma_model <- rbind(arma_model_s, arma_model_i)
  
  if (!is.null(path)) {
    xtable(arma_model,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_fit_arx_1.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' @export
arx_2_model_bic <- function(input_frame_s, input_frame_i, input_frame_exp, max.p = 7,
                            path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                            country = "de") {
  input_frame_exp %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load)) %>%
    mutate(residual_load = residual_load - lag(residual_load, 1))
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date")) %>%
    tail(-1)
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date")) %>%
    tail(-1)
  
  calc_bic <- function(model, p, q) {
    data.frame(p = p,
               aic = -2 * model$loglik + 2 * length(model$coef),
               bic = -2 * model$loglik + length(model$coef) *
                 log(length(model$residuals)),
               hic = -2 * model$loglik + 2 * length(model$coef) *
                 log(log(length(model$residuals))))     
  }
  
  out_s <- lapply(1:max.p, function(x) {
    arima(input_frame_s$price,
          order = c(x, 0, 0),
          xreg = input_frame_s$residual_load,
          method = "ML",
          optim.control = list(maxit = 2000)) %>% calc_bic(x)
  }) %>%
    rbind_all %>%
    select(p, bic) %>%
    rename(`DAy Ahead BIC` = bic)
  
  out_i <- lapply(1:max.p, function(x) {
    arima(input_frame_i$price,
          order = c(x, 0, 0),
          xreg = input_frame_i$residual_load,
          method = "ML",
          optim.control = list(maxit = 2000)) %>% calc_bic(x)
  }) %>%
    rbind_all %>%
    select(bic) %>%
    rename(`Intraday BIC` = bic)
  
  out <- cbind(out_s, out_i)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, paste0("/", country, "_bic_arx_2.tex")),
            floating = FALSE,
            include.rownames = FALSE,
            sanitize.text.function = function(x){x})
  }
}

#' @export
arx_2_model_fit <- function(input_frame_s, input_frame_i, input_frame_exp,
                            path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                            country = "de") {
  
  input_frame_exp %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load)) %>%
    mutate(residual_load = residual_load - lag(residual_load, 1))
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date")) %>%
    tail(-1)
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date")) %>%
    tail(-1)
  
  arma_model_s <- arima(input_frame_s$price,
                        order = c(7, 0, 0),
                        method = "ML",
                        xreg = input_frame_s$residual_load,
                        optim.control = list(maxit = 2000)) %>% 
    tidy %>%
    {
      rbind(tail(., 2), head(., -2))
    } %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = (Estimate / Std.Error) %>% round(4),
              p.value = 2 * pt(abs(t.statistic),
                               nrow(input_frame_s) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(Parameter == "input_frame_s$residual_load", "$\\eta_1$",
                                     ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                            ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), Parameter))))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  arma_model_i <- arima(input_frame_i$price,
                        order = c(7, 0, 0),
                        method = "ML",
                        xreg = input_frame_i$residual_load,
                        optim.control = list(maxit = 2000)) %>%
    tidy %>%
    {
      rbind(tail(., 2), head(., -2))
    } %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = (Estimate / Std.Error) %>% round(4),
              p.value = 2 * pt(abs(t.statistic),
                               nrow(input_frame_i) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(Parameter == "input_frame_i$residual_load", "$\\eta_1$",
                                     ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                            ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), Parameter))))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  arma_model <- rbind(arma_model_s, arma_model_i)
  
  if (!is.null(path)) {
    xtable(arma_model,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_fit_arx_2.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' @export
garch_model_bic <- function(input_frame_s, input_frame_i, max.p = 7, max.q = 7,
                            path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                            country = "de") {
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  
  arima_start_val_s <- arima(input_frame_s$price,
                             order = c(7, 0, 6),
                             optim.control = list(maxit = 2000)) %>%
    coef %>%
    as.list
  
  names(arima_start_val_s) <- c("ar1", "ar2", "ar3", "ar4", "ar5", "ar6", "ar7", "ma1",
                                "ma2", "ma3", "ma4", "ma5", "ma6", "mu")
  
  arima_start_val_i <- arima(input_frame_i$price,
                             order = if (country == "de") c(7, 0, 7) else c(7, 0, 6),
                             optim.control = list(maxit = 2000)) %>%
    coef %>%
    as.list
  
  if (country == "de") {
    names(arima_start_val_i) <- c("ar1", "ar2", "ar3", "ar4", "ar5", "ar6", "ar7", "ma1",
                                  "ma2", "ma3", "ma4", "ma5", "ma6", "ma7", "mu")
  } else {
    names(arima_start_val_i) <- c("ar1", "ar2", "ar3", "ar4", "ar5", "ar6", "ar7", "ma1",
                                  "ma2", "ma3", "ma4", "ma5", "ma6", "mu")
  }
  
  out_s <- lapply(1:max.p, function(p) {
    lapply(1:max.q, function(q) {
      cat(paste0("Estimating garch model with p ", p, " and q ", q, "\n"))
      model <- ugarchspec(
        variance.model = list(model = "sGARCH",
                              garchOrder = c(p, q)),
        mean.model = list(armaOrder = c(7, 6)),
        start.pars = arima_start_val_s,
        distribution = "norm")
      ugarchfit(spec = model,
                data = input_frame_s$price) %>%
        infocriteria %>% 
        as.data.frame %>%
        add_rownames %>% 
        transmute(info_crit = rowname,
                  bic = V1,
                  p = p,
                  q = q) %>%
        filter(info_crit == "Bayes")
    }) %>% rbind_all
  }) %>% rbind_all %>%
    select(p, q, bic) %>%
    rename(`p/q` = p) %>%
    spread(q, bic)
  
  out_i <- lapply(1:max.p, function(p) {
    lapply(1:max.q, function(q) {
      cat(paste0("Estimating garch model with p ", p, " and q ", q, "\n"))
      model <- ugarchspec(
        variance.model = list(model = "sGARCH",
                              garchOrder = c(p, q)),
        mean.model = list(armaOrder = if (country == "de") c(7, 7) else c(7, 6)),
        start.pars = arima_start_val_i,
        distribution = "norm")
      ugarchfit(spec = model,
                data = input_frame_i$price,
                solver = "hybrid") %>%
        infocriteria %>% 
        as.data.frame %>%
        add_rownames %>% 
        transmute(info_crit = rowname,
                  bic = V1,
                  p = p,
                  q = q) %>%
        filter(info_crit == "Bayes") 
    }) %>% rbind_all
  }) %>% rbind_all %>%
    select(p, q, bic) %>%
    rename(`p/q` = p) %>%
    spread(q, bic)
  
  out <- rbind(out_s, out_i)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_bic_garch.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' @export
garch_model_fit <- function(input_frame_s, input_frame_i,
                            path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                            country = "de") {
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing()
  
  arima_start_val_s <- arima(input_frame_s$price,
                             order = c(7, 0, 6),
                             optim.control = list(maxit = 2000)) %>%
    coef %>%
    as.list
  
  names(arima_start_val_s) <- c("ar1", "ar2", "ar3", "ar4", "ar5", "ar6", "ar7", "ma1",
                                "ma2", "ma3", "ma4", "ma5", "ma6", "mu")
  
  arima_start_val_i <- arima(input_frame_i$price,
                             order = if (country == "de") c(7, 0, 7) else c(7, 0, 6),
                             optim.control = list(maxit = 2000)) %>%
    coef %>%
    as.list
  
  if (country == "de") {
    names(arima_start_val_i) <- c("ar1", "ar2", "ar3", "ar4", "ar5", "ar6", "ar7", "ma1",
                                  "ma2", "ma3", "ma4", "ma5", "ma6", "ma7", "mu")
  } else {
    names(arima_start_val_i) <- c("ar1", "ar2", "ar3", "ar4", "ar5", "ar6", "ar7", "ma1",
                                  "ma2", "ma3", "ma4", "ma5", "ma6", "mu")
  }
  
  model_s <- ugarchspec(
    variance.model = list(model = "sGARCH",
                          garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(7, 6)),
    start.pars = arima_start_val_s,
    distribution = "norm")
  
  garch_model_s <- ugarchfit(spec = model_s,
                           data = input_frame_s$price,
                           solver = "hybrid",
                           solver.control = list(
                             ftol_rel = 1e-10,
                             xtol_rel = 1e-10)
                           ) %>%
    `@`(fit) %>%
    `$`(matcoef) %>%
    as.data.frame %>%
    add_rownames("Parameter") %>%
    #     filter(!grepl("mu", Parameter),
    #            !grepl("ar", Parameter),
    #            !grepl("ma", Parameter)) %>%
    rename(Estimate = ` Estimate`,
           Std.Error = ` Std. Error`,
           t.value = ` t value`,
           p.value = `Pr(>|t|)`) %>%
    mutate(Parameter = ifelse(Parameter == "omega", "$\\omega$",
                              ifelse(Parameter == "mu", "$\\phi_0$",
                                     ifelse(grepl("alpha", Parameter), paste0("$\\", gsub("alpha", "alpha_", Parameter), "$"),
                                            ifelse(grepl("beta", Parameter), paste0("$\\", gsub("beta", "beta_", Parameter), "$"),
                                                   ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                                          ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA))))))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  model_i <- ugarchspec(
    variance.model = list(model = "sGARCH",
                          garchOrder = c(1, 1)),
    mean.model = list(armaOrder = if (country == "de") c(7, 7) else c(7, 6)),
    start.pars = arima_start_val_i,
    distribution = "norm")
  
  garch_model_i <- ugarchfit(spec = model_i,
                             data = input_frame_i$price,
                             solver = "hybrid") %>%
    `@`(fit) %>%
    `$`(matcoef) %>%
    as.data.frame %>%
    add_rownames("Parameter") %>%
    #     filter(!grepl("mu", Parameter),
    #            !grepl("ar", Parameter),
    #            !grepl("ma", Parameter)) %>%
    rename(Estimate = ` Estimate`,
           Std.Error = ` Std. Error`,
           t.value = ` t value`,
           p.value = `Pr(>|t|)`) %>%
    mutate(Parameter = ifelse(Parameter == "omega", "$\\omega$",
                              ifelse(Parameter == "mu", "$\\phi_0$",
                                     ifelse(grepl("alpha", Parameter), paste0("$\\", gsub("alpha", "alpha_", Parameter), "$"),
                                            ifelse(grepl("beta", Parameter), paste0("$\\", gsub("beta", "beta_", Parameter), "$"),
                                                   ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                                          ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA))))))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  garch_model <- rbind(garch_model_s, garch_model_i)
  
  if (!is.null(path)) {
    xtable(garch_model,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_fit_garch.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' @export
garch_model_exogen_bic_1 <- function(input_frame_s, input_frame_i, input_frame_exp, max.p = 7, max.q = 7,
                            path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                            country = "de") {
  
  input_frame_exp %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load))
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))

  out_s <- lapply(1:max.p, function(p) {
    lapply(1:max.q, function(q) {
      cat(paste0("Estimating garch model with p ", p, " and q ", q, "\n"))
      model <- ugarchspec(
        variance.model = list(model = "sGARCH",
                              garchOrder = c(p, q)),
        mean.model = list(armaOrder = c(7, 0),
                          external.regressors = input_frame_s$residual_load %>% as.matrix),
        distribution = "norm")
      ugarchfit(spec = model,
                data = input_frame_s$price) %>%
        infocriteria %>% 
        as.data.frame %>%
        add_rownames %>% 
        transmute(info_crit = rowname,
                  bic = V1,
                  p = p,
                  q = q) %>%
        filter(info_crit == "Bayes")
    }) %>% rbind_all
  }) %>% rbind_all %>%
    select(p, q, bic) %>%
    rename(`p/q` = p) %>%
    spread(q, bic)
  
  out_i <- lapply(1:max.p, function(p) {
    lapply(1:max.q, function(q) {
      cat(paste0("Estimating garch model with p ", p, " and q ", q, "\n"))
      model <- ugarchspec(
        variance.model = list(model = "sGARCH",
                              garchOrder = c(p, q)),
        mean.model = list(armaOrder = c(7, 0),
                          external.regressors = input_frame_s$residual_load %>% as.matrix),
        distribution = "norm")
      ugarchfit(spec = model,
                data = input_frame_i$price,
                solver = "hybrid") %>%
        infocriteria %>% 
        as.data.frame %>%
        add_rownames %>% 
        transmute(info_crit = rowname,
                  bic = V1,
                  p = p,
                  q = q) %>%
        filter(info_crit == "Bayes") 
    }) %>% rbind_all
  }) %>% rbind_all %>%
    select(p, q, bic) %>%
    rename(`p/q` = p) %>%
    spread(q, bic)
  
  out <- rbind(out_s, out_i)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_bic_garch_exo_1.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' @export
garch_model_exogen_fit_1 <- function(input_frame_s, input_frame_i,
                            path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                            country = "de") {
  
  input_frame_exp %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load))
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  
  model_s <- ugarchspec(
    variance.model = list(model = "sGARCH",
                          garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(7, 0),
                      external.regressors = input_frame_s$residual_load %>% as.matrix),
    distribution = "norm")
  
  garch_model_s <- ugarchfit(spec = model_s,
                             data = input_frame_s$price) %>%
    `@`(fit) %>%
    `$`(matcoef) %>%
    as.data.frame %>%
    add_rownames("Parameter") %>%
    #     filter(!grepl("mu", Parameter),
    #            !grepl("ar", Parameter),
    #            !grepl("ma", Parameter)) %>%
    rename(Estimate = ` Estimate`,
           Std.Error = ` Std. Error`,
           t.value = ` t value`,
           p.value = `Pr(>|t|)`) %>%
    mutate(Parameter = ifelse(Parameter == "omega", "$\\omega$",
                              ifelse(Parameter == "mu", "$\\phi_0$",
                                     ifelse(grepl("alpha", Parameter), paste0("$\\", gsub("alpha", "alpha_", Parameter), "$"),
                                            ifelse(grepl("beta", Parameter), paste0("$\\", gsub("beta", "beta_", Parameter), "$"),
                                                   ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                                          ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA))))))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  model_i <- ugarchspec(
    variance.model = list(model = "sGARCH",
                          garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(7, 0),
                      external.regressors = input_frame_s$residual_load %>% as.matrix),
    distribution = "norm")
  
  garch_model_i <- ugarchfit(spec = model_i,
                             data = input_frame_i$price) %>%
    `@`(fit) %>%
    `$`(matcoef) %>%
    as.data.frame %>%
    add_rownames("Parameter") %>%
    #     filter(!grepl("mu", Parameter),
    #            !grepl("ar", Parameter),
    #            !grepl("ma", Parameter)) %>%
    rename(Estimate = ` Estimate`,
           Std.Error = ` Std. Error`,
           t.value = ` t value`,
           p.value = `Pr(>|t|)`) %>%
    mutate(Parameter = ifelse(Parameter == "omega", "$\\omega$",
                              ifelse(Parameter == "mu", "$\\phi_0$",
                                     ifelse(grepl("alpha", Parameter), paste0("$\\", gsub("alpha", "alpha_", Parameter), "$"),
                                            ifelse(grepl("beta", Parameter), paste0("$\\", gsub("beta", "beta_", Parameter), "$"),
                                                   ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                                          ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA))))))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  garch_model <- rbind(garch_model_s, garch_model_i)
  
  if (!is.null(path)) {
    xtable(garch_model,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_fit_garch.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}