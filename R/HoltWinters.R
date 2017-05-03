#' @export
NULL

#' Holt-Winters Filtering
#' 
#' Computes Holt-Winters Filtering of a given time series. Unknown parameters are determined by minimizing the squared prediction error.
#'
#' \description{ The additive Holt-Winters prediction function (for time series with period length p) is
#' Yhat[t+h] = a[t] + h * b[t] + s[t - p + 1 + (h - 1) mod p],
#' where a[t], b[t] and s[t] are given by
#' a[t] = α (Y[t] - s[t-p]) + (1-α) (a[t-1] + b[t-1])
#' b[t] = β (a[t] - a[t-1]) + (1-β) b[t-1]
#' s[t] = γ (Y[t] - a[t]) + (1-γ) s[t-p]
#' The multiplicative Holt-Winters prediction function (for time series with period length p) is
#' Yhat[t+h] = (a[t] + h * b[t]) * s[t - p + 1 + (h - 1) mod p],
#' where a[t], b[t] and s[t] are given by
#' a[t] = α (Y[t] / s[t-p]) + (1-α) (a[t-1] + b[t-1])
#' b[t] = β (a[t] - a[t-1]) + (1-β) b[t-1]
#' s[t] = γ (Y[t] / a[t]) + (1-γ) s[t-p]
#' The data in x are required to be non-zero for a multiplicative model, but it makes most sense if they are all positive.}
#'
#' @param object FLVector
#' @param alpha alpha parameter of Holt-Winters Filter.
#' @param beta beta parameter of Holt-Winters Filter. If set to FALSE, the function will do exponential smoothing.
#' @param gamma	gamma parameter used for the seasonal component. If set to FALSE, an non-seasonal model is fitted.
#' @param periodicity The period across which the data shows a seasonal trend.
#' @param forecastperiod The period for which the forecasted values are to be calculated.
#' @param normalization If set to 1, the mean is added to values to normalize the data.
#' @return An object containing details of the fitted model and forecasted values.
#' @return A list with class "htest" containing the following components:
#'
#' \describe{
#' \item{fitted}{the forecasted values}
#' \item{x}{the data}
#' \item{alpha}{the alpha value used for forecasting}
#' \item{beta}{the beta value used for forecasting}
#' \item{gamma}{the gamma value used for forecasting}
#' \item{seasonal}{the type of seasonal model used, additive or multiplicative}
#' \item{call}{The call used}
#' }
#'
#' @examples
#' x<-rnorm(1000)
#' flv<-as.FLVector(x)
#' flobj<-HoltWinters(object = flv, alpha = 0.5)

#' @export
HoltWinters<-function(object,...){
	UseMethod("HoltWinters",object)
}

#' @export
HoltWinters.default  <- function (object,...){
    return(stats::HoltWinters(object,...))
}

#' @export
HoltWinters.FLVector<-function(object,
						 	   alpha=0.5,
						 	   beta=0.5,
						  	   gamma=0.5,
						 	   periodicity=7,
						 	   forecastperiod=7,
						 	   normalization=0,...){ #browser()
	if(!is.FLVector(object)) stop("The class of the input object should be FLVector")
	if(!all(beta)) Beta<-0 else Beta<-beta
	if(!all(gamma)) Gamma<-0 else Gamma<-gamma
 	t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
                           pSelect = list(a = c(GroupID = 1,
                           						PeriodID="a.vectorIndexColumn",
                                                Num_Val = "a.vectorValueColumn",
                                                periodicity=periodicity,
                                                alpha=alpha,
                                                beta=Beta,
                                                gamma=Gamma,
                                                forecastperiod=forecastperiod)))
	temp1 <- createTable(pTableName=gen_unique_table_name("HoltWinters"),pSelect=t)

	if(!all(gamma) && all(beta)){
		pSelect<-paste0("Select GroupID, PeriodID, Num_Val, periodicity,
						alpha, beta, forecastperiod from ",temp1)
		pViewColnames<-c(pGroupID="GroupID",
						pNum_Val="Num_Val",
						pPeriodicity="periodicity",
						pAlpha="alpha",
						pBeta="beta",
						pNormal=normalization,
						pForecastPeriod=forecastperiod)
		pFuncName<-"FLExpSmooth2FactorUdt"
	}
	else if(!all(gamma) && !all(beta)){ 
		pSelect<-paste0("Select GroupID, PeriodID, Num_Val, periodicity,
						alpha, forecastperiod from ",temp1)
		pViewColnames<-c(pGroupID="GroupID",
						 pNum_Val="Num_Val",
						 pPeriodicity="periodicity",
						 pAlpha="alpha",
						 pNormal=normalization,
						 pForecastPeriod=forecastperiod)
		pFuncName<-"FLExpSmooth1FactorUdt"
	}
	else {
		pSelect<-paste0("Select GroupID, PeriodID, Num_Val, periodicity,
						alpha, beta, gamma, forecastperiod from ",temp1)
		pViewColnames<-c(pGroupID="GroupID",
						 pNum_Val="Num_Val",
			 			 pPeriodicity="periodicity",
						 pAlpha="alpha",
						 pBeta="beta",
						 pGamma="gamma",
						 pNormal=normalization,
						 pForecastPeriod=forecastperiod)
		pFuncName<-"FLHoltWintersUdt"
	}

	query<-constructUDTSQL(pViewColnames=pViewColnames,
						   pSelect=pSelect,
						   pOutColnames=c("a.*"),
						   pFuncName=pFuncName,
						   pLocalOrderBy=c("pGroupID"),
						   pNest=TRUE)
	temp2 <- createTable(pTableName=gen_unique_table_name("temp"),pSelect=query)
	ret <- sqlQuery(getFLConnection(),paste0("Select * from ",temp2," order by 2"))
	retobj<-list(fitted=ret,
				 x=object,
				 alpha=alpha,
				 beta=beta,
				 gamma=gamma,
				 coefficients=NULL,
				 seasonal="additive",
				 SSE=NULL,
				 call=match.call())
	class(retobj)<-"HoltWinters"
	return(retobj)
}
