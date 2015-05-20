#' @export
set_date_time <- function () {
  if (.Platform$OS.type == "windows") {
    Sys.setlocale("LC_TIME", "English")
  } else if (.Platform$OS.type == "unix") {
    Sys.setlocale("LC_TIME", "en_US.UTF-8")
  }
}

ewma <- function() {
  .Call("ewma", PACKAGE = "powerfor")
}

loglik_a <- function() {
  .Call("loglik_a", PACKAGE = "powerfor")
}

loglik_a_sim <- function() {
  .Call("loglik_a_sim", PACKAGE = "powerfor")
}

loglik_a_sim_sum <- function() {
  .Call("loglik_a_sim_sum", PACKAGE = "powerfor")
}

loglik_a_sim_sum_neg <- function() {
  .Call("loglik_a_sim_sum_neg", PACKAGE = "powerfor")
}