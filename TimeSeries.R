
lapply(list('xts', 'rugarch', 'rmgarch', 
             'broom','sgt','fGarch',
            'PerformanceAnalytics'), require, character.only = TRUE)




fdccCorr <- function(stock_ts, indices_ts, groups_i){
  
  tickers = names(indices_ts)
  #groups_i = c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 7, 8)
  
  X <- na.omit(na.locf.default(merge.xts(stock_ts, indices_ts, join = 'inner')))
  
  
  specs <- ugarchspec(variance.model = list(model = "eGARCH",
                                            garchOrder = c(1,1),
                                            external.regressors = NULL,
                                            variance.targeting = FALSE),
                      mean.model = list(armaOrder = c(1,1), 
                                        include.mean = TRUE,
                                        archm = FALSE, archpow = 1, 
                                        arfima = FALSE, external.regressors = NULL, 
                                        archex = FALSE), 
                      distribution.model = 'ged')
  
  specm <- dccspec(multispec(replicate(length(names(X)), specs)), VAR = FALSE, robust = FALSE, lag = 1, lag.max =5, 
                   lag.criterion = c("BIC"), external.regressors = NULL, 
                   groups = groups_i, 
                   dccOrder = c(1,1), model = c("DCC", "aDCC", "FDCC")[3], 
                   distribution = c("mvnorm", "mvt", "mvlaplace")[1]) 
  
  
  corrObj <- dccfit(specm, X, out.sample = 40,
                solver = c("hybrid","solnp", "nlminb", "lbfgs", "gosolnp")[5], 
                fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)
  
  return(corrObj)
  
}


adccCorr <- function(stock_ts, indices_ts){
  
  tickers = names(indices_ts)


  X <- na.omit(na.locf.default(merge.xts(stock_ts, indices_ts, join = 'inner')))
  
  
  specs <- ugarchspec(variance.model = list(model = "eGARCH",
                                            garchOrder = c(1,1),
                                            external.regressors = NULL,
                                            variance.targeting = FALSE),
                      mean.model = list(armaOrder = c(1,1), 
                                        include.mean = TRUE,
                                        archm = FALSE, archpow = 1, 
                                        arfima = FALSE, external.regressors = NULL, 
                                        archex = FALSE), 
                      distribution.model = 'ged')
  
  specm <- dccspec(multispec(replicate(2, specs)), VAR = FALSE, robust = FALSE, lag = 1, lag.max =5, 
                   lag.criterion = c("BIC"), external.regressors = NULL,
                   dccOrder = c(1,1), model = c("DCC", "aDCC", "FDCC")[2], 
                   distribution = c("mvnorm", "mvt", "mvlaplace")[3]) 
  
  
  corrObj <- tryCatch(dccfit(specm, X, out.sample = 40,
                    solver = c("hybrid","solnp", "nlminb", "lbfgs", "gosolnp")[5], 
                    fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                    cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL), 
                    error=function(cond) NULL)
  
  
  return(corrObj)
  
}