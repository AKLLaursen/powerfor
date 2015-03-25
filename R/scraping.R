#' Scrape EEX transparancy
#' @param date_from A date string of format "yyyy-mm-dd"
#' @param date_to A date string of format "yyyy-mm-dd"
#' 
#' @export
scrape_trans_eex <- function (date_from = "2010-01-01", date_to = "2011-07-07") {
  date_from %<>% as.Date()
  date_to %<>% as.Date()
  
  dates <- seq(date_from, date_to, by = "1 day")
  
  headers <-
    c(Referer = "http://www.eex-transparency.com/homepage/power/germany/production/usage/expected-solar-power-generation-/expected-solar-power-generation-table-")
  
  out <- lapply(1:length(dates), function(i) {
    url <- sprintf("http://www.eex-transparency.com/static/tem-6-solar/de/%s.json",
                   format(dates[i], "%Y/%m/%d"))
    cat(i)
    result <-
      url %>%
      GET(add_headers(headers)) %>%
      content(as = "text") %>%
      fromJSON %$%
      data %>%
      ensure_that(length(.) > 0 ~ "Error scraping data.") %>%
      {
        if (nrow(.) < 192) {
          (.) %>%
            transmute(date = dates[i] ,
                      hour = rep(c(1, 2, 4:24), each = n() / 23),
                      quarter = rep(c(1:8, 13:96), each = n() / 92),
                      connecting_area,
                      expected_production = energy) %>%
            left_join(data.frame(date = dates[i], 
                                 quarter = rep(1:96, each = 2)),
                      .,
                      by = c("date", "quarter"))
        }
        else if (nrow(.) == 376) {
          (.) %>%
            {
              rbind(slice(., 1:3), slice(., 28),  slice(., 4:6), slice(., 28),
                    slice(., 7:9), slice(., 28),  slice(., 10:12), slice(., 28),
                    slice(., 13:15), slice(., 28),  slice(., 16:18),
                    slice(., 28), slice(., 19:21), slice(., 28),
                    slice(., 22:25), slice(., 28), slice(., 26:376))
            } %>%
            transmute(date = dates[i],
                      hour = rep(1:24, each = n() / 24),
                      quarter = rep(1:96, each = n() / 96),
                      connecting_area,
                      expected_production = energy)
        } else if (nrow(.) > 192 & nrow(.) < 384 & nrow(.) != 288) {
          (.) %>%
            transmute(date = dates[i] ,
                      hour = rep(c(1, 2, 4:24), each = n() / 23),
                      quarter = rep(c(1:8, 13:96), each = n() / 92),
                      connecting_area,
                      expected_production = energy) %>%
            left_join(data.frame(date = dates[i], 
                                 quarter = rep(1:96, each = 4)),
                      .,
                      by = c("date", "quarter"))
        } else if (nrow(.) == 400) {
          (.) %>%
            transmute(date = dates[i],
                      hour = rep(c(1:3, 3, 4:24), each = n() / 25),
                      quarter = rep(c(1:12, 9:12, 13:96), each = n() / 100),
                      connecting_area,
                      expected_production = energy)
        } else {
          (.) %>%
          transmute(date = dates[i],
                    hour = rep(1:24, each = n() / 24),
                    quarter = rep(1:96, each = n() / 96),
                    connecting_area,
                    expected_production = energy)
        }
      }
    
    return(result)
  }) %>%
    rbind_all
  
  return(out)
}