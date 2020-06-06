#################################
#### feistest and bsfeistest ####
#################################

data("mwp", package = "feisr")

### Estimate model (subset of obs)

wages.feis <- feis(lnw ~ marry + enrol + yeduc
                   | exp, data = mwp[mwp$id <= 50, ], id = "id")


### Estimate art and bsht

art0 <- feistest(wages.feis, robust = TRUE)

art <- feistest(wages.feis, robust = TRUE, terms = c("marry", "enrol"))

bsht0 <- bsfeistest(wages.feis, rep = 10, seed = 133213, prog = FALSE)

bsht <- bsfeistest(wages.feis, rep = 10, seed = 133213, prog = FALSE, terms = c("marry", "enrol"))




### Check terms
test_that("Terms in feistest", {
  expect_equal(length(art0$wald_feis$Terms), 3, tolerance = 1e-10)
  expect_equal(length(art0$wald_fe$Terms), 4, tolerance = 1e-10)
  expect_equal(length(art$wald_feis$Terms), 2, tolerance = 1e-10)
  expect_equal(length(art$wald_fe$Terms), 3, tolerance = 1e-10)
})

test_that("Terms in bsfeistest", {
  expect_equal(length(bsht0$wald_feis$Terms), 3, tolerance = 1e-10)
  expect_equal(length(bsht0$wald_fe$Terms), 4, tolerance = 1e-10)
  expect_equal(length(bsht$wald_feis$Terms), 2, tolerance = 1e-10)
  expect_equal(length(bsht$wald_fe$Terms), 3, tolerance = 1e-10)
})



### Unit tests of feistest
art0_1 <- 0.93
art1_1 <- 0.89

art0_2 <- 14.34
art1_2 <- 8.70

art0_3 <- 1.54
art1_3 <- 1.05

test_that("Unit test for feistest", {
  expect_equal(art0$wald_feis$result$chi2[1], art0_1, tolerance = 2e-2, check.attributes = FALSE)
  expect_equal(art0$wald_fe$result$chi2[1], art0_2, tolerance = 2e-2, check.attributes = FALSE)
  expect_equal(art0$wald_re$result$chi2[1], art0_3, tolerance = 2e-2, check.attributes = FALSE)
  expect_equal(art$wald_feis$result$chi2[1], art1_1, tolerance = 2e-2, check.attributes = FALSE)
  expect_equal(art$wald_fe$result$chi2[1], art1_2, tolerance = 2e-2, check.attributes = FALSE)
  expect_equal(art$wald_re$result$chi2[1], art1_3, tolerance = 2e-2, check.attributes = FALSE)
})


### Unit tests of bsfeistest
bsht0_1 <- 1.03
bsht1_1 <- 0.99

bsht0_2 <- 5.01
bsht1_2 <- 4.05

bsht0_3 <- 1.69
bsht1_3 <- 0.94

test_that("Unit test for feistest", {
  expect_equal(bsht0$wald_feis$result$chi2[1], bsht0_1, tolerance = 2e-1, check.attributes = FALSE)
  expect_equal(bsht0$wald_fe$result$chi2[1], bsht0_2, tolerance = 2e-1, check.attributes = FALSE)
  expect_equal(bsht0$wald_re$result$chi2[1], bsht0_3, tolerance = 2e-1, check.attributes = FALSE)
  expect_equal(bsht$wald_feis$result$chi2[1], bsht1_1, tolerance = 5e-1, check.attributes = FALSE)
  expect_equal(bsht$wald_fe$result$chi2[1], bsht1_2, tolerance = 5e-1, check.attributes = FALSE)
  expect_equal(bsht$wald_re$result$chi2[1], bsht1_3, tolerance = 5e-1, check.attributes = FALSE)
})


### pvalue tests of feistest bsfeistest
test_that("P value for feistest and bsfeistest", {
  expect_equal(art$wald_feis$result$chi2[3], 0.64, tolerance = 2e-2, check.attributes = FALSE)
  expect_equal(art0$wald_feis$result$chi2[3], 0.82, tolerance = 2e-2, check.attributes = FALSE)
  expect_equal(bsht$wald_feis$result$chi2[3], 0.61, tolerance = 2e-2, check.attributes = FALSE)
  expect_equal(bsht0$wald_re$result$chi2[3], 0.64, tolerance = 1e-1, check.attributes = FALSE)
})

