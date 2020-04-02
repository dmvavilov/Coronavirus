.libPaths('C:/R_lib')
library(tseries)
library(xts)
install.packages('fGarch')
library(fGarch)
garchAutoTryFit = function(
  ll,
  data,
  trace=FALSE,
  forecast.length=1,
  with.forecast=TRUE,
  ic="AIC",
  garch.model="garch" )
{
  formula = as.formula(?paste( sep="",
                               "~ arma(", ll$order[1], ",", ll$order[2], ")+",
                               garch.model,
                               "(", ll$order[3], ",", ll$order[4], ")" ) )
  fit = tryCatch( garchFit( formula=formula?
                            data=data,
                            trace=FALSE,
                            cond.dist=ll$dist ),
                  error=function( err ) TRUE,
                  warning=function( warn ) FALSE )
  
  pp = NULL
  
  if( !is.l?gical( fit ) ) {
    if( with.forecast ) {
      pp = tryCatch( predict( fit,
                              n.ahead=forecast.length,
                              doplot=FALSE ),
                     error=function( err ) FALSE,
                     warnin?=function( warn ) FALSE )
      if( is.logical( pp ) ) {
        fit = NULL
      }
    }
  } else {
    fit = NULL
  }
  
  if( trace ) {
    if( is.null( fit ) ) {
      cat( paste( sep="",
                  "   Analyzing (", ll$order[1], ",", ll$order[2?,
                  ",", ll$order[3], ",", ll$order[4], ") with ",
                  ll$dist, " distribution done.",
                  "Bad model.\n" ) )
    } else {
      if( with.forecast ) {
        cat( paste( sep="",
                    "   Analyzing?(", ll$order[1], ",", ll$order[2], ",",
                    ll$order[3], ",", ll$order[4], ") with ",
                    ll$dist, " distribution done.",
                    "Good model. ", ic, " = ", round(fit@fit$ics[[ic]],6),
                    ", fore?ast: ",
                    paste( collapse=",", round(pp[,1],4) ), "\n" ) )
      } else {
        cat( paste( sep="",
                    "   Analyzing (", ll[1], ",", ll[2], ",", ll[3], ",", ll[4], ") with ",
                    ll$dist, " distribution ?one.",
                    "Good model. ", ic, " = ", round(fit@fit$ics[[ic]],6), "\n" ) )
      }
    }
  }
  
  return( fit )
}

garchAuto = function(
  xx,
  min.order=c(0,0,1,1),
  max.order=c(5,5,2,2),
  trace=FALSE,
  cond.dists="sged",
  with.foreca?t=TRUE,
  forecast.length=1,
  arma.sum=c(0,1e9),
  cores=1,
  ic="AIC",
  garch.model="garch" )
{
  require( fGarch )
  require( parallel )
  
  len = NROW( xx )
  
  models = list( )
  
  for( dist in cond.dists )
    for( p in min.order[1]:max.order[1] ?
      for( q in min.order[2]:max.order[2] )
        for( r in min.order[3]:max.order[3] )
          for( s in min.order[4]:max.order[4] )
          {
            pq.sum = p + q
            if( pq.sum <= arma.sum[2] && pq.sum >= arma.sum[1] )
            {?              models[[length( models ) + 1]] = list( order=c( p, q, r, s ), dist=dist )
            }
          }
  
  res = mclapply( models,
                  garchAutoTryFit,
                  data=xx,
                  trace=trace,
                  ic?ic,
                  garch.model=garch.model,
                  forecast.length=forecast.length,
                  with.forecast=TRUE,
                  mc.cores=cores )
  
  best.fit = NULL
  
  best.ic = 1e9
  for( rr in res )
  {
    if( !is.null( rr )?)
    {
      current.ic = rr@fit$ics[[ic]]
      if( current.ic < best.ic )
      {
        best.ic = current.ic
        best.fit = rr
      }
    }
  }
  
  if( best.ic < 1e9 )
  {
    return( best.fit )
  }
  
  return( NULL )
}
quant_tools_to_xts <- fu?ction(df, ...){
  df <- as.data.frame(df) 
  #df$date = as.Date(df$date) 
  ts = xts(df[,-1], order.by = df[,1]) 
  return(ts) }
Corona <- read_excel("C:/Users/Митя/Desktop/Corona.xlsx")
df <- Corona
Corona <- quant_tools_to_xts(Corona)
adf.test(Corona)
pl?t(Corona)
lag = 2
Corona <- diff(log(Corona), lag = lag)
Corona <- Corona[-c(1:lag),]
adf.test(Corona)
plot(Corona)
m1 <- garchAuto(Corona, with.forecast = TRUE, forecast.length = 10, trace = TRUE)
summary(m1)
PRED <- predict(m1, n.ahead = 10)[1:10,1]
PRED?<- as.data.frame(PRED)
rownames(Corona)
tom = as.Date('2020-04-02')
dates_for_pred <- NULL
dates_for_pred <- as.data.frame(dates_for_pred)
for (i in 1:10) {
  dates_for_pred[i,1] <- tom + i -1
}
PRED <- cbind(dates_for_pred, PRED)
colnames(PRED) <- c('Date?, 'Val')
PRED <- quant_tools_to_xts(PRED)
REAL_PRED <- rbind(Corona, PRED)
rows <- df[1:2,]
rows <- quant_tools_to_xts(rows)
REAL_PRED <- rbind(rows, REAL_PRED)
for (i in 3:nrow(REAL_PRED)) {
  REAL_PRED[i] <- as.numeric(REAL_PRED[(i-2)])*exp(as.numeric(RE?L_PRED[i]))
}
REAL_PRED <- round(REAL_PRED, digits = 0)
plot(REAL_PRED, col = 'white', ylab = 'Number of Cases', main = 'Coronavirus in Russia')
lines(REAL_PRED['::2020-04-01'], col = 'blue', lwd = 3)
lines(REAL_PRED['2020-04-01::'], col = 'red', lwd = 3)
?egend("topleft", c("REAL","PREDICTION"), fill=c("red","blue"))