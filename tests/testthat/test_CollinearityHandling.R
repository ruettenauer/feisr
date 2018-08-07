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
feis1.mod <- feis(lwage ~ ed + sex + exp + bluecol + ind + smsa +  married + wks | year,
                    data = Wages, id = "id", robust = F)

feis1b.mod <- feis(lwage ~ ed + sex + exp + bluecol + ind + smsa +  married + wks | year,
                  data = Wages, id = "id", robust = F, intercept = T)


ht1 <- feistest(feis1.mod, robust=F, type = "both")
ht1b <- feistest(feis1b.mod, robust=F, type = "both")


vars <- c("bluecolyes", "ind", "smsayes", "marriedyes", "wks")

test_that("Drop (within-) constant vars (Wages)", {
  expect_identical(names(feis1.mod$coefficients), vars)
  expect_identical(names(feis1b.mod$coefficients), c("(Intercept)", vars))
  expect_identical(names(ht1$wald_feis$b[ht1$wald_feis$Terms]), paste(vars, "hat", sep = "_"))
})


# Handling of perfectly collinear slopes (error message)
test_that("Stop if slopes perfectly collinear", {
  expect_error(feis(lwage ~ ed + sex + bluecol + ind + smsa +  married + wks | year + exp,
                    data = Wages, id = "id", robust = F))
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

