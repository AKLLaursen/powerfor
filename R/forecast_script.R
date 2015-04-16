# forecast_type <- c("svm_linearx_1", "svm_linearx_2", "svm_polynomial",
#                    "svm_polynomialx_1", "svm_polynomialx_2")
# 
# country <- c("de", "fr")
# 
# market <- c("spot", "intraday")
# 
# lapply(forecast_type, function(x) {
#   lapply(country, function(y) {
#     lapply(market, function(q) {
#       if (y == "de") {
#         input_frame_exp <- readRDS("C:/git/r/powerfor/inst/rds/data_de_exogen.rds")
#         if(q == "spot") {
#           input_frame <- readRDS("C:/git/r/powerfor/inst/rds/data_de_spot.rds")
#         } else if (q == "intraday") {
#           input_frame <- readRDS("C:/git/r/powerfor/inst/rds/data_de_intraday.rds")
#         }
#       } else if (y == "fr") {
#         input_frame_exp <- readRDS("C:/git/r/powerfor/inst/rds/data_fr_exogen.rds")
#         if(q == "spot") {
#           input_frame <- readRDS("C:/git/r/powerfor/inst/rds/data_fr_spot.rds")
#         } else if (q == "intraday") {
#           input_frame <- readRDS("C:/git/r/powerfor/inst/rds/data_fr_intraday.rds")
#         }
#       }
#       
#       oos_forecast_daily(input_frame, input_frame_exp, test_start = "2013-01-01", h = 1,
#                cores = 8L, forecast_type = x,
#                market = q, country = y)
#     })
#   })
# })