require("compiler")


# Generates one or two exploratory variables linear model
# with first one is subject to error
generate.eive.data <- function (n, e.sd, delta.sd, seed = 12345, useotherx = FALSE){
	set.seed(seed)
	e<-rnorm(n,0,e.sd)
	x<-rnorm(n)
	x1 <- NULL
	y <- NULL
	if(useotherx){
		x1<-rnorm(n)
	}
	delta<-rnorm(n,0,delta.sd)
	xdelta<-x+delta
	if(useotherx){
		y<-5+5*x+5*x1+e
	}else{
		y<-5+5*x+e
	}
	data <- cbind (xdelta, x1, y)
	return(data)
}



eive.cga <- function(dirtyx, otherx=NULL, y, numdummies=10, popsize=20){

	ols.dirty <- NULL
	ols.proxy <- NULL
	ols.best <- NULL
	n <- length(y)
	f<-function(d){
		ols <- NULL
        m<-matrix(d,nrow=n)
	    ols.proxy <- lm(dirtyx ~ m)
	    x.proxy <- ols.proxy$fitted.values
		if(is.null(otherx)){
	    	ols <- lm(y ~ x.proxy)
		}else{
			ols <- lm(y ~ x.proxy + otherx)
		}
        return (sum (ols$residuals^2))
	}

	ga<-cga(evalFunc=f, chsize=n*numdummies, popsize=popsize)
	#best<-cga_generate_chromosome(ga)
    best <- as.integer(ga)
	if(is.null(otherx)){
		ols.dirty <- lm(y~dirtyx)
	}else{
		ols.dirty <- lm(y~dirtyx+otherx)
	}

	ols.proxy<-lm(dirtyx~matrix(best,nrow=n))

	if(is.null(otherx)){
		ols.best <- lm(y~ols.proxy$fitted.values)
	}else{
		ols.best <- lm(y~ols.proxy$fitted.values + otherx)
	}

	result <- list (ols = ols.dirty, eive = ols.best, proxy=ols.proxy)
	return(result)
}

