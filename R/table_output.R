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
                               nrow(input_frame) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, "**",
                                     ifelse(0.05 <= p.value & p.value < 0.1, "***", paste0(Parameter, "")))))
  
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
                               nrow(input_frame) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, "**",
                                     ifelse(0.05 <= p.value & p.value < 0.1, "***", paste0(Parameter, "")))))
  
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
                               nrow(input_frame) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, "**",
                                     ifelse(0.05 <= p.value & p.value < 0.1, "***", paste0(Parameter, "")))))
  
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
                               nrow(input_frame) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, "**",
                                     ifelse(0.05 <= p.value & p.value < 0.1, "***", paste0(Parameter, "")))))
  
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
armax_1_model_bic <- function(input_frame_s, input_frame_i, input_frame_exp, smax.p = 7, max.q = 7,
                           path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                           country = "de") {
  input_frame_exp %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(res_load_forecast = mean(res_load_forecast)) %>%
    ungroup
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    pre_model_processing() %>%
    left_join(input_frame_exp, by = c("date"))
  
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
            xreg = input_frame_s$res_load_forecast,
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
            xreg = input_frame_i$res_load_forecast,
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
            file = paste0(path, paste0("/", country, "_bic_armax_1.tex")),
            floating = FALSE,
            include.rownames = FALSE,
            sanitize.text.function = function(x){x})
  }
}

#' @export
armax_1_model_fit <- function(input_frame_s, input_frame_i, input_frame_exp,
                           path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Tables",
                           country = "de") {
  
  input_frame_exp %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(res_load_forecast = mean(res_load_forecast)) %>%
    ungroup
  
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
                        xreg = input_frame_s$res_load_forecast,
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
                               nrow(input_frame) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, "**",
                                     ifelse(0.05 <= p.value & p.value < 0.1, "***", paste0(Parameter, "")))))
  
  arma_model_i <- arima(input_frame_i$price,
                        order = if (country == "de") c(7, 0, 7) else c(7, 0, 6),
                        method = "ML",
                        xreg = input_frame_i$res_load_forecast,
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
                               nrow(input_frame) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "$\\phi_0$",
                              ifelse(grepl("ar", Parameter), paste0("$\\", gsub("ar", "phi_", Parameter), "$"),
                                     ifelse(grepl("ma", Parameter), paste0("$\\", gsub("ma", "theta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, "**",
                                     ifelse(0.05 <= p.value & p.value < 0.1, "***", paste0(Parameter, "")))))
  
  arma_model <- rbind(arma_model_s, arma_model_i)
  
  if (!is.null(path)) {
    xtable(arma_model,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path, "/", country, "_fit_armax_1.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}