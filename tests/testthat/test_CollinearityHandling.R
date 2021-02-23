######################################################################
#### Test feis / feistest creis / manual creis against each other ####
######################################################################

library(plm)
context("Handlig of collinearity / constants")


### Wages

data("Wages", package = "plm")

Wages$id<-rep(c(1:595), each = 7)
Wages$year<-rep(c(1976:1982), times = 595)


# Handling of contant vars
# ed + sex constant,  exp perfectly collinear with slope var year
expect_warning(
feis1.mod <- feis(lwage ~ ed + sex + exp + bluecol + ind + smsa +  married + wks | year,
                    data = Wages, id = "id", robust = F)
)

expect_warning(
feis1b.mod <- feis(lwage ~ ed + sex + exp + bluecol + ind + smsa +  married + wks | year,
                  data = Wages, id = "id", robust = F, intercept = T)
)

ht1 <- feistest(feis1.mod, robust=F, type = "all")
ht1b <- feistest(feis1b.mod, robust=F, type = "all")


vars <- c("bluecolyes", "ind", "smsayes", "marriedyes", "wks")

test_that("Drop (within-) constant vars (Wages)", {
  expect_identical(names(feis1.mod$coefficients), vars)
  expect_identical(names(feis1b.mod$coefficients), c("(Intercept)", vars))
  expect_identical(names(ht1$wald_feis$b[ht1$wald_feis$Terms]), paste(vars, "hat", sep = "_"))
})


# Handling of perfectly collinear slopes (error message)
# Make test QR rank specific (for handling chcks with alternative BLAS)
devtolwin <- 2.220446e-22
Xm <- feisr::detrend(data = Wages[, c("year", "exp")], slopes = 1, id = Wages$id)
rank <- qr(as.matrix(Xm), tol = devtolwin)$rank
test_that("Avoid collinearity by default tol value", {
  if(rank == 2){
    expect_warning(feis(lwage ~ ed + sex + bluecol + ind + smsa +  married + wks | year + exp,
                        data = Wages, id = "id", robust = F, tol = devtolwin))
  }
  if(rank < 2){
    expect_error(feis(lwage ~ ed + sex + bluecol + ind + smsa +  married + wks | year + exp,
                        data = Wages, id = "id", robust = F, tol = devtolwin))
  }
})

test_that("Stop if slopes perfectly collinear", {
  expect_error(feis(lwage ~ ed + sex + bluecol + ind + smsa +  married + wks | year + exp,
                      data = Wages, id = "id", robust = F, tol = 1e-04))
})


# Handling of (within-) constant slopes
feis2.mod <- feis(lwage ~ bluecol + ind + smsa +  married  | year + wks,
                  data = Wages, id = "id", robust = F, dropgroups = F)

expect_warning(feis3.mod <- feis(lwage ~ bluecol + ind + smsa +  married  | year + wks,
                  data = Wages, id = "id", robust = F, dropgroups = T))

test_that("Drop groups without within slope var", {
  expect_lt(length(feis3.mod$residuals), length(feis2.mod$residuals))
  expect_gt(length(feis3.mod$na.action), length(feis2.mod$na.action))
  expect_false(identical(feis3.mod$coefficients, feis2.mod$coefficients))
})





### NA coef handling


data("mwp", package = "feisr")
mwp$wts <- 0.3
mwp$wts[1:(nrow(mwp)/2)] <- 1
mwp$lnw[sample(1:nrow(mwp), 10)] <- NA
mwp$enrol2 <- mwp$enrol

wages.feis1 <- feis(lnw ~ marry + enrol  + yeduc
                    | exp, data = mwp, weights = mwp$wts, id = "id")

expect_warning(
  wages.feis2 <- feis(lnw ~ marry + enrol + enrol2 + yeduc
                      | exp, data = mwp, weights = mwp$wts, id = "id")
)

test_that("Results equal if NA coef dropped because of collinearity", {
  expect_equal(wages.feis1$coefficients, wages.feis2$coefficients, tolerance = 1e-10)
  expect_equal(vcov(wages.feis1, scale = TRUE), vcov(wages.feis2, scale = TRUE), tolerance = 1e-10)
})






### Hedonic

data("Hedonic", package = "plm")
# Fake year
Hedonic$fyear <- ave(Hedonic$townid, Hedonic$townid, FUN=function(x) 1:length(x))
Hedonic$pys <- ave(Hedonic$mv, Hedonic$townid, FUN = function(x) length(x[!is.na(x)]))


# ptratio, zn, tax constant
expect_warning(feis4.mod <- feis(mv ~ ptratio + crim + chas + zn + tax | lstat,
                                 data = Hedonic, id="townid", robust=F))


test_that("Drop (within-) constant vars (Wages)", {
  expect_identical(names(feis4.mod$coefficients), c("crim", "chasyes"))
})




### Test within variance

# Create time dummy
Wages$td <- 0
Wages$td[Wages$year > 1976 & Wages$year > 1979] <- 1

expect_warning(
feis5.mod <- feis(lwage ~ ed + sex + exp + bluecol + ind + smsa +  married + wks + td | year,
                  data = Wages, id = "id", robust = F)
)
test_that("Keep time-related dummy if linear time slope", {
  expect_identical(names(feis5.mod$coefficients), c(vars, "td"))
})
