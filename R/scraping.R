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

#' Function scraping various spotprices and volumes from the Epexspot website
#'
#' @param from_date A string with the start date of the desired series
#' @param to_date A string with the end date of the desired series
#' @param country A string with the desired country iso id. Takes the values DE, 
#' FR and CH. Default is DE.
#' @param market A string indicating market type. Takes the values Spot and
#' Intraday. Deafult is Spot.
#' @param contract A string indicating contract type for intraday. Take the
#' values H and Q. Default is H.
#' @param filter_missing A bolean, determining wether or not to replace missing
#' values.
#' @return a dataframe containing the dates, hours, spot and volumes
#' @export
scrape_epex <- function(from_date, to_date, country = "DE", market = "Spot",
                        contract = "H", filter_missing = TRUE)
{
  cat("Initialising scraping routine, getting dates ...\n")
  time_stamp <- Sys.time()
  
  if (market == "Spot") {
    date_scr <- seq(as.Date(from_date) -6, as.Date(to_date) -6, by = 1) %>%
      as.character
  } else if (market == "Intraday") {
    date_scr <- seq(as.Date(from_date), as.Date(to_date), by = 1) %>%
      as.character
  } else {
    stop("Market not recognised")
  }
  
  data_out <- lapply(1:length(date_scr), function(ii) {
    if (market == "Spot") {
      
      epex_sp <-
        html(paste0("http://www.epexspot.com/en/market-data/dayaheadauction/au",
                    "ction-table/",
                    date_scr[ii],
                    "/",
                    country))
      
      data_scr <- epex_sp %>%
        html_nodes("#tab_de td:nth-child(9)") %>%
        html_text %>%
        gsub(",", "", .) %>% 
        as.numeric
      
      # Summarise to handle October daylights savings, i.e. two hour 3.
      data_out <- data.frame(
        date = date_scr[ii] %>% as.Date %>% add(6),
        hour = if (length(data_scr) > 48) c(1:3, 3:24) else 1:24,
        spot = data_scr[c(TRUE, FALSE)],
        volume = data_scr[c(FALSE, TRUE)],
        created = time_stamp) %>%
        group_by(date, hour) %>%
        summarise(spot = mean(spot, na.rm = TRUE),
                  volume = mean(volume, na.rm = TRUE)) %>%
        ungroup
      
      cat(paste0(as.character(as.Date(date_scr[ii]) + 6), " ...\n"))
      
    } else if (market == "Intraday") {
      
      epex_sp <-
        html(paste0("http://www.epexspot.com/en/market-data/intradaycontinuous",
                    "/intraday-table/",
                    date_scr[ii],
                    "/",
                    country))
      
      data_scr <- epex_sp %>%
        html_nodes("td:nth-child(6)") %>%
        html_text %>%
        gsub(",", "", .) %>% 
        as.numeric %>%
        head(-1)
      
      if (contract == "H") {
        
        if (country == "DE") {
          data_scr <- data_scr[seq(1, length(data_scr), 5)]
        }
        
        # Summarise to handle October daylights savings, i.e. two hour 3.
        data_out <- data.frame(
          date = date_scr[ii] %>% as.Date,
          hour = if (length(data_scr) > 24) c(1:3, 3:24) else 1:24,
          vwap = data_scr,
          created = time_stamp) %>%
        group_by(date, hour) %>%
        summarise(vwap = mean(vwap, na.rm = TRUE)) %>%
        ungroup
        
        cat(paste0(date_scr[ii], " ...\n"))
        
      } else if (contract == "Q") {
        
        data_scr <- data_scr[-seq(1, length(data_scr), 5)]
        
        # Summarise to handle October daylights savings, i.e. two hour 3.
        data_out <- data.frame(
          date = date_scr[ii] %>% as.Date,
          quarter = if (length(data_scr) > 96) c(1:12, 9:12, 13:96) else 1:96,
          vwap = data_scr,
          created = time_stamp) %>%
          group_by(date, quarter) %>%
          summarise(vwap = mean(vwap, na.rm = TRUE)) %>%
          ungroup
        
        cat(paste0(date_scr[ii], " ...\n"))
        
      } else {
        stop("Contract not recognised")
      }
      
    } else {
      stop("Market not recognised\n")
    }
    return(data_out)
  })
        if (market == "Spot") {
          data_out <- data_out %>%
            rbind_all %>%
            transmute(date,
                      hour,
                      price = if (filter_missing == TRUE) na_filter(spot) else spot,
                      volume = if (filter_missing == TRUE) na_filter(volume) else volume)
        } else if (market == "Intraday") {
          data_out <- data_out %>%
            rbind_all %>%
            transmute(date,
                      hour,
                      price = if (filter_missing == TRUE) na_filter(vwap) else vwap)
        } else {
          stop("Please specify correct market type.")
        }
        
        cat("Data downloaded, exiting function\n")
        return(data_out)
}