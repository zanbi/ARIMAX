# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# .rs.restartR()

# packages
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(MASS)
library(RcppRoll)
library(parallel)
library(doSNOW)
library(doParallel)






Arimax_wh_fn <- function(Index = arimax_df$ARIMAX_YM[1:141], newIndex = arimax_df$ARIMAX_YM[142:144],
                         data = arimax_df$ARIMAX_CNT[1:141], testdata = arimax_df$ARIMAX_CNT[142:144],
                         xreg = as.matrix(x_df[1:141,2:3]), newxreg = as.matrix(x_df[142:144,2:3]),
                         order = c(3,3,3), orders = c(1,2,1), period = c(0,12),
                         error_type = c("aic","Last3M","Last6M","Last12M"),
                         predict_length = 3, transf_y = "log", parallel = "N") {


  ## Error messages


  # grid
  expand_df <- expand.grid(p = 0:order[1], d = 0:order[2], q = 0:order[3],
                           P = 0:orders[1], D = 0:orders[2], Q = 0:orders[3] , period = period )
  # index set
  Index    <- as.character(Index)
  newIndex <- as.character(newIndex)
  # y set
  y_df <- data
  if (transf_y == "log") { y_df <- log10(data) } else if (transf_y == "sqrt") { y_df <- sqrt(data) }

  if (parallel == "Y") {
    result_df <- do.call(rbind.fill , mclapply(1:nrow(expand_df), function(i) {
      # i = 1100
      order_df <- expand_df[i,]
      print(paste(i, " // ",  nrow(expand_df), "  /// ", order_df$p,order_df$d,order_df$q, order_df$P,order_df$D,order_df$Q, order_df$period))

      # 시계열
      WB.fit <- tryCatch(arima(y_df, order=c(order_df$p,order_df$d,order_df$q),
                               seasonal=list(order=c(order_df$P,order_df$D,order_df$Q), period=order_df$period),
                               method = c("CSS-ML", "ML", "CSS"), xreg = xreg),
                         error = function(e) print("NOT"),
                         warning = function(w) print("NOT"))

      if (WB.fit == "NOT") {
        RESULT_df <- data.frame(NO = i, order_df,  ARIMAX_YM = "9999-12-31", real = 0, pred = 0, se = 0, aic = 0,
                                m3 = 0, m6 = 0, m12 = 0)
        names(RESULT_df) <- c("NO", "p", "d", "q", "P", "D", "Q", "PERIOD", "ARIMAX_YM",  "real", "pred" , "se" ,"aic",
                              "Last3M","Last6M","Last12M")
        RESULT_df
      } else {

        # 추정값
        fitted_value <- y_df - WB.fit$residuals
        fitted_value <- as.vector(round(fitted_value))
        # 정확도 %
        fitted_perc  <- round( (y_df - abs(WB.fit$residuals))/y_df , 4)
        fitted_perc <- as.vector((fitted_perc))
        #
        avg_fit3  <- mean( rev( fitted_perc )[ 1:3  ] )
        avg_fit6  <- mean( rev( fitted_perc )[ 1:6  ] )
        avg_fit12 <- mean( rev( fitted_perc )[ 1:12  ] )
        #
        wb_fcast <- predict(WB.fit, n.ahead = predict_length , newxreg = newxreg)
        #
        RESULT_df <- cbind.data.frame(ARIMAX_YM = newIndex,
                                      real  = c(testdata, rep(NA, nrow(newxreg) - length(testdata))),
                                      pred  =
                                        if (transf_y == "log") {            round(10^as.numeric((wb_fcast$pred)))
                                        } else if (transf_y == "sqrt") {    round(as.numeric((wb_fcast$pred))^2)
                                        } else if (transf_y == "normal") {round(as.numeric((wb_fcast$pred)))},
                                      se    = if (transf_y == "log") {      round(10^as.numeric((wb_fcast$se)))
                                      } else if (transf_y == "sqrt") {      round(as.numeric((wb_fcast$se))^2)
                                      } else if (transf_y == "normal") {    round(as.numeric((wb_fcast$se)))},
                                      aic   = WB.fit$aic,
                                      m3 = avg_fit3,
                                      m6 = avg_fit6,
                                      m12 = avg_fit12)
        RESULT_df <- cbind.data.frame(NO = i, order_df, RESULT_df)
        names(RESULT_df) <- c("NO", "p", "d", "q", "P", "D", "Q", "PERIOD", "ARIMAX_YM",  "real", "pred" , "se" ,"aic",
                              "Last3M","Last6M","Last12M")
      }
      RESULT_df
    }, mc.cores = 11 ))
  } else {
    result_df <- do.call(rbind.fill , lapply(1:nrow(expand_df), function(i) {
      # i = 1100
      order_df <- expand_df[i,]
      print(paste(i, " // ",  nrow(expand_df), "  /// ", order_df$p,order_df$d,order_df$q, order_df$P,order_df$D,order_df$Q, order_df$period))

      # 시계열
      WB.fit <- tryCatch(arima(y_df, order=c(order_df$p,order_df$d,order_df$q),
                               seasonal=list(order=c(order_df$P,order_df$D,order_df$Q), period=order_df$period),
                               method = c("CSS-ML", "ML", "CSS"), xreg = xreg),
                         error = function(e) print("NOT"),
                         warning = function(w) print("NOT"))

      if (WB.fit == "NOT") {
        RESULT_df <- data.frame(NO = i, order_df,  ARIMAX_YM = "9999-12-31", real = 0, pred = 0, se = 0, aic = 0,
                                m3 = 0, m6 = 0, m12 = 0)
        names(RESULT_df) <- c("NO", "p", "d", "q", "P", "D", "Q", "PERIOD", "ARIMAX_YM",  "real", "pred" , "se" ,"aic",
                              "Last3M","Last6M","Last12M")
        RESULT_df
      } else {

        # 추정값
        fitted_value <- y_df - WB.fit$residuals
        fitted_value <- as.vector(round(fitted_value))
        # 정확도 %
        fitted_perc  <- round( (y_df - abs(WB.fit$residuals))/y_df , 4)
        fitted_perc <- as.vector((fitted_perc))
        #
        avg_fit3  <- mean( rev( fitted_perc )[ 1:3  ] )
        avg_fit6  <- mean( rev( fitted_perc )[ 1:6  ] )
        avg_fit12 <- mean( rev( fitted_perc )[ 1:12  ] )
        #
        wb_fcast <- predict(WB.fit, n.ahead = predict_length , newxreg = newxreg)
        #
        RESULT_df <- cbind.data.frame(ARIMAX_YM = newIndex,
                                      real  = c(testdata, rep(NA, nrow(newxreg) - length(testdata))),
                                      pred  =
                                        if (transf_y == "log") {            round(10^as.numeric((wb_fcast$pred)))
                                        } else if (transf_y == "sqrt") {    round(as.numeric((wb_fcast$pred))^2)
                                        } else if (transf_y == "normal") {round(as.numeric((wb_fcast$pred)))},
                                      se    = if (transf_y == "log") {      round(10^as.numeric((wb_fcast$se)))
                                      } else if (transf_y == "sqrt") {      round(as.numeric((wb_fcast$se))^2)
                                      } else if (transf_y == "normal") {    round(as.numeric((wb_fcast$se)))},
                                      aic   = WB.fit$aic,
                                      m3 = avg_fit3,
                                      m6 = avg_fit6,
                                      m12 = avg_fit12)
        RESULT_df <- cbind.data.frame(NO = i, order_df, RESULT_df)
        names(RESULT_df) <- c("NO", "p", "d", "q", "P", "D", "Q", "PERIOD", "ARIMAX_YM",  "real", "pred" , "se" ,"aic",
                              "Last3M","Last6M","Last12M")
      }
      RESULT_df
    }))
  }

  result_df <- result_df %>% dplyr::filter(ARIMAX_YM != "9999-12-31")
  result_df <- result_df %>% mutate(ARIMAX_YM = as.character(ARIMAX_YM))
  # 베스트 모델 찾기 aic 기준
  result_df$gap <- ifelse(is.na(result_df$real), NA, abs(result_df$real - result_df$pred) )
  result_df <- result_df %>% dplyr::select(NO, ARIMAX_YM, p,d,q,P,D,Q,PERIOD, aic, real, pred, gap, Last3M, Last6M, Last12M)

  result_df <- result_df %>% arrange(aic) ; result_df$aic_no <- 1:nrow(result_df)
  result_df <- result_df %>% arrange(Last3M) ; result_df$Last3M_no <- 1:nrow(result_df)
  result_df <- result_df %>% arrange(Last6M) ; result_df$Last6M_no <- 1:nrow(result_df)
  result_df <- result_df %>% arrange(Last12M) ; result_df$Last12M_no <- 1:nrow(result_df)

  #
  if(error_type == "aic") {            result_df <- result_df %>% arrange(aic_no)
  } else if (error_type == "Last3M") { result_df <- result_df %>% arrange(Last3M_no)
  } else if (error_type == "Last6M") { result_df <- result_df %>% arrange(Last6M_no)
  } else if (error_type == "Last12M") {result_df <- result_df %>% arrange(Last12M_no)}

  max_pt <- 20
  # 최종 선택 가중치
  sample_df  <- result_df %>% filter(NO %in% result_df$NO[1:max_pt])
  sample_df0 <- result_df %>% filter(NO %in% result_df$NO[1:5])
  sample_df1 <- result_df %>% filter(NO %in% result_df$NO[1:10])

  resulttop_df <- data.frame(
    topall = data.frame(tapply(sample_df$pred,  sample_df$ARIMAX_YM, mean)),
    top5  = data.frame(tapply(sample_df0$pred, sample_df0$ARIMAX_YM, mean)),
    top10 = data.frame(tapply(sample_df1$pred, sample_df1$ARIMAX_YM, mean))

  )
  names(resulttop_df) <- c("topall", "top5", "top10")

  if(error_type == "aic") {              names(resulttop_df) <- paste0("aic_", names(resulttop_df))
  } else if (error_type == "Last3M") {   names(resulttop_df) <- paste0("Last3M_", names(resulttop_df))
  } else if (error_type == "Last6M") {   names(resulttop_df) <- paste0("Last6M_", names(resulttop_df))
  } else if (error_type == "Last12M") {  names(resulttop_df) <- paste0("Last12M_", names(resulttop_df))}

  tmp <- data.frame(ARIMAX_YM = newIndex , no = 1:predict_length, real = testdata, resulttop_df)
  result = list(value = tmp,
                order = sample_df %>% dplyr::select(NO, p,d,q,P,D,Q,PERIOD) %>% unique %>% mutate(MDL_TOP = 1:n()))

return(result )
}



