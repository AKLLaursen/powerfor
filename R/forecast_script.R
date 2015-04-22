# forecast_type <- c("ar", "arima", "arx_1", "arx_2", "garch", "garchx_1",
#                    "garchx_2", "svm_linear", "svm_linearx_1", "svm_linearx_2",
#                    "svm_polynomial", "svm_polynomialx_1", "svm_polynomialx_2")
# 
# country <- c("de", "fr")
# 
# market <- c("spot", "intraday")
# 
# hour = c(4, 10)
# 
# lapply(forecast_type, function(x) {
#   lapply(country, function(y) {
#     lapply(market, function(q) {
#       lapply(hour, function(z))
#         if (y == "de") {
#           input_frame_exp <- readRDS("C:/git/r/powerfor/inst/rds/data_de_exogen.rds")
#           if(q == "spot") {
#             input_frame <- readRDS("C:/git/r/powerfor/inst/rds/data_de_spot.rds")
#           } else if (q == "intraday") {
#             input_frame <- readRDS("C:/git/r/powerfor/inst/rds/data_de_intraday.rds")
#           }
#         } else if (y == "fr") {
#           input_frame_exp <- readRDS("C:/git/r/powerfor/inst/rds/data_fr_exogen.rds")
#           if(q == "spot") {
#             input_frame <- readRDS("C:/git/r/powerfor/inst/rds/data_fr_spot.rds")
#           } else if (q == "intraday") {
#             input_frame <- readRDS("C:/git/r/powerfor/inst/rds/data_fr_intraday.rds")
#           }
#         }
#         
#         oos_forecast_hourly(input_frame, input_frame_exp, test_start = "2013-01-01", h = 1,
#                  cores = 8L, forecast_type = x,
#                  market = q, country = y, hour = z)
#     })
#   })
# })