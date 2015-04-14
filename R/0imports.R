#' @importFrom magrittr %>% %<>% %$% add use_series extract set_names subtract 
#'    raise_to_power multiply_by

#' @importFrom rvest html html_nodes html_text

#' @importFrom dplyr rbind_all group_by ungroup summarise mutate n select filter 
#'    transmute left_join add_rownames rename

#' @importFrom tidyr gather spread

#' @importFrom broom tidy

#' @importFrom e1071 svm

#' @importFrom ggplot2 ggplot geom_point scale_color_manual geom_abline ggsave 
#'    stat_function geom_hline geom_vline annotate xlab ylab geom_line 
#'    geom_segment aes_string aes ylim ggtitle

#' @importFrom gridExtra arrangeGrob

#' @importFrom tseries adf.test

#' @importFrom xtable xtable

#' @importFrom rugarch ugarchspec ugarchfit infocriteria residuals

#' @importFrom e1071 svm

#' @importFrom parallel parLapply makeCluster clusterExport clusterEvalQ
#'    stopCluster

#' @useDynLib powerfor

#' @importFrom Rcpp sourceCpp

#' @importFrom jsonlite fromJSON

#' @importFrom httr GET content add_headers

#' @importFrom ensurer ensure_that ensures_that

#' @importFrom R.matlab Matlab
NULL