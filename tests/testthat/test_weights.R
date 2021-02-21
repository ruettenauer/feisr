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




### Check ART and BSHT test results

wages.art1 <- feistest(wages.feis)
wages.art2 <- feistest(wages.feis2)

wages.bsht1 <- bsfeistest(wages.feis, type = "bs1", rep = 10, seed = 123, prog = FALSE)
wages.bsht2 <- bsfeistest(wages.feis2, type = "bs1", rep = 10, seed = 123, prog = FALSE)


test_that("ART CREIS with weights = weighted model", {
  expect_equal(wages.art2$CREIS$coefficients[2:4], wages.feis2$coefficients, tolerance = 1e-10)
  expect_equal(wages.art2$CREIS$coefficients, wages.art1$CREIS$coefficients, tolerance = 1e-10)
  expect_equal(wages.art2$wald_feis$result$chi2, wages.art1$wald_feis$result$chi2, tolerance = 1e-10)
})

test_that("BSHT same sample for subset and weights", {
  expect_equal(wages.bsht2$samples, wages.bsht1$samples, tolerance = 1e-10)
})

test_that("BSHT CREIS with weights = weighted model", {
  expect_equal(wages.bsht2$wald_feis, wages.bsht1$wald_feis, tolerance = 1e-10)
  expect_equal(wages.bsht2$bscoef.feis, wages.bsht1$bscoef.feis, tolerance = 1e-10)
  expect_equal(wages.bsht2$bscoef.fe, wages.bsht1$bscoef.fe, tolerance = 1e-10)

})


