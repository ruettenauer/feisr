###################################
#### Expand Formula (from plm) ####
###################################

expand.formula <- function(x){
  oclass <- class(x)
  if (! any(class(x) == "Formula")) stop("not a Formula object")
  if (length(x)[2] != 2) stop("not a two part formula")
  xs <- structure(x, class = "formula")
  has.response <- attr(terms(xs),"response") == 1
  if (has.response){
    y <- x[[2]]
    rhs <- x[[3]]
  }
  else{
    y <- NULL
    rhs <- x[[2]]
  }
  firstpart <- rhs[[2]]
  secondpart <- rhs[[3]]
  if (has.response){
    one <- do.call("~", list(y,firstpart))
    two <- do.call("~", list(y,secondpart))
  }
  else{
    one <- do.call("~", list(firstpart))
    two <- do.call("~", list(secondpart))
  }
  two <- update(one, two)
  one <- paste(deparse(one), collapse = "")
  two <- paste(deparse(two[[3]]), collapse = "")
  result <- as.formula(paste(one, "|", two, collapse = ""));
  result <- Formula::as.Formula(result)
  #YC  class(result) <- c("pFormula", class(result))
  structure(result, class = oclass)
}



#################################
#### Function Residual maker ####
#################################

resm <- function(x, ...){
  x <- as.matrix(x)
  r <- diag(1, nrow(x)) - x %*% solve(t(x) %*% x) %*% t(x)
  return(r)
}

##################################
#### Function Predicted maker ####
##################################

hatm <- function(y, x, checkcol=TRUE, ...){
  x <- as.matrix(x)

  # Check for perfect collinearity within groups
  if(checkcol == TRUE){
    x.qr <- qr(x)
    if(x.qr$rank < ncol(x)){
      vars <- x.qr$pivot[1:x.qr$rank]
      x <- x[, vars]
    }
  }

  res <- x %*% solve( t(x) %*% x) %*% t(x) %*% as.matrix(y)
  return(res)
}


############################
#### Clean "AsIs" names ####
############################

cleani <- function(x, ...){
  x <- gsub(".*I\\(\\s*|\\).*", "", x)
  x <- gsub("[[:punct:]]", "_", x)
  x <- sub("_$", "", x)

}


#################################
#### Between variance slopes ####
#################################

nowithinvar <- function(x, mf, id, tol = 1e-12, ...){

  within <- lapply(colnames(x), FUN = function(u) mf - apply(mf, 2, FUN = function(z)
    ave(z, x[, u], FUN = function(w) mean(w)))
       - apply(mf, 2, FUN = function(v) ave(v, id, FUN=function(y) mean(y))))

  withinsd <- sapply(within, FUN = function(u) apply(u, 2, FUN = function(z) sd(z)))

  res <- which(apply(withinsd, 1, FUN = function(u) any(u < tol)))
  names(res) <- rownames(withinsd)[res]

  res
}


# betw_slp <- function(x, mf, ...){
#   sn <- colnames(x)
#   sn <- sn[-which(sn=="(Intercept)")]
#   res<-apply(mf, 2, FUN = function(z) ave(z, x[, "(Intercept)"], FUN=function(w) mean(w)))
#   for(i in sn){
#     res <- res - apply(mf, 2, FUN = function(z) ave(z, x[, i], FUN=function(w) mean(w)))
#   }
#   res
# }



##########################
#### Extract response ####
##########################

# this has to be the "demeaned" response var
# ATTENTION WITH ORDER!!

# Attach demeaned data to model output??

model.response.feis <- function(x, ...){
  res <- as.vector(x$response)
  return(res)
}



#################
#### Sum Res ####
#################

sumres <- function(x, ...){
  sr <- summary(unclass(resid(x)))
  srm <- sr["Mean"]
  if (abs(srm) < 1e-10){
    sr <- sr[c(1:3, 5:6)]
  }
  sr
}

rss.feis <- function(x, ...){
  rss <- sum(x$residuals^2)
}


#############
#### TSS ####
#############

tss.feis <- function(x, ...){
  var(model.response.feis(x)) * (length(model.response.feis(x)) - 1)
}


############################
#### Function R squared ####
############################

r.sq.feis <- function(object, adj=FALSE, df=NULL){
  z <- object
  r <- z$residuals
  n <- length(r)
  rss <- sum(r^2)
  f <- z$fitted.values
  if(is.null(df)){
    rdf <- z$df.residual
  } else{
    rdf <- df
  }

  if (attr(z$terms, "intercept")){
    mss <- sum((f - mean(f))^2)
    df.int <- 1L
  } else{
    mss <- sum(f^2)
    df.int <- 0L
  }
  r.squared <- mss/(mss + rss)
  adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int)/rdf) ##TR: Correct for slope parameters??

  if(adj){
    return(adj.r.squared)
  }else{
    return(r.squared)
  }

}




