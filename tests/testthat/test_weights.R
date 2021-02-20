#########################################################
#### Compare Results to Stata ado xtfeis (V. Ludwig) ####
#########################################################
context("Test weighted estimation")

data("mwp", package = "feisr")


### Fake weights
mwp$wts <- 0.3
mwp$wts[1:(nrow(mwp)/2)] <- 1

mwp$wts2 <- 0
mwp$wts2[which(mwp$id %in% unique(mwp$id))[1:(length(unique(mwp$id))/2)]] <- 1


### Estimate model with subset and weights

wages.feis <- feis(lnw ~ marry + enrol + yeduc
                   | exp, data = mwp[mwp$wts2 == 1, ], id = "id")

expect_warning(
wages.feis2 <- feis(lnw ~ marry + enrol + yeduc
                    | exp, data = mwp, id = "id", weights = mwp$wts2)
)

wages.feis3 <- feis(lnw ~ marry + enrol + yeduc
                    | exp, data = mwp, id = "id", weights = mwp$wts, robust = FALSE)

wages.feis4 <- feis(lnw ~ marry + enrol + yeduc
                    | exp, data = mwp, id = "id", weights = mwp$wts, robust = TRUE)

# LSDV
wages.lsdv2 <- lm(lnw ~ marry + enrol + yeduc + exp*as.factor(id)
                 , data = mwp, weights = mwp$wts2)

wages.lsdv3 <- lm(lnw ~ marry + enrol + yeduc + exp*as.factor(id)
                  , data = mwp, weights = mwp$wts)



### Check weighted results = subset results
test_that("weighted results = subset results", {
  expect_equal(wages.feis$coefficients, wages.feis2$coefficients, tolerance = 1e-10)
  expect_equal(vcov(wages.feis, scale = TRUE), vcov(wages.feis2, scale = TRUE), tolerance = 1e-10)
})

### Check against lsdv approach
test_that("weighted results = lsdv results with weights", {
  expect_equal(wages.feis2$coefficients, wages.lsdv2$coefficients[2:4], tolerance = 1e-10)
  expect_equal(vcov(wages.feis2, scale = TRUE), vcov(wages.lsdv2, scale = TRUE)[2:4, 2:4], tolerance = 1e-10)
  expect_equal(wages.feis3$coefficients, wages.lsdv3$coefficients[2:4], tolerance = 1e-10)
  expect_equal(vcov(wages.feis3, scale = TRUE), vcov(wages.lsdv3, scale = TRUE)[2:4, 2:4], tolerance = 1e-10)
})










