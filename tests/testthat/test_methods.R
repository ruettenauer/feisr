#############################
#### S3 methods for feis ####
#############################

### Predict and fitted and residuals

wages.feis <- feis(lnw ~ marry + enrol + yeduc + yeargr
                   | exp + I(exp^2), data = mwp, id = "id")


newdata <- wages.feis$modeltrans[,-1]
pred1 <- fitted(wages.feis)
pred2 <- predict(wages.feis, newdata = newdata)

# Check identical results
test_that("Fitted vs predict on tranformed data", {
  expect_equal(pred1, pred2, tolerance = 1e-10)
})

# Check against response
resid <- residuals(wages.feis)
diff1 <- wages.feis$response - (pred1 + resid)

test_that("Check Response = predict + resid", {
  expect_equal(diff1, rep(0, length(resid)), tolerance = 1e-10, check.names = FALSE)
})


# No NA in response, predict and resid
test_that("No NA in response, predict and resid", {
  expect_false(any(is.na(pred1)))
  expect_false(any(is.na(pred2)))
  expect_false(any(is.na(resid)))
})


### Unit test for deviance, nobs, df.residuals
n <- nobs(wages.feis)
df <- df.residual(wages.feis)
dev <- deviance(wages.feis)
test_that("Unit test nobs, deviance, df.residuals", {
  expect_equal(n, 3100, tolerance = 1e-10, check.names = FALSE)
  expect_equal(df, (3100 - 268*3 - 4) , tolerance = 1e-10, check.names = FALSE)
  expect_equal(dev, 187.4197 , tolerance = 0.01, check.names = FALSE)
})

# - Added S3 methods for`feis()`: coef, deviance, df.residual, fitted, formula, nobs, predict, residuals, terms, vcov


### Test coef and vcov format
coefs <- coef(wages.feis)
vcov <- vcov(wages.feis)
test_that("Format coef and vcov S3 method", {
  expect_equal(length(names(coefs)), 4)
  expect_true(is.numeric(coefs))
  expect_identical(colnames(vcov), rownames(vcov))
  expect_identical(colnames(vcov), names(coefs))
  expect_true(is.numeric(vcov))
  expect_equal(dim(vcov), c(4, 4))
})



### Test formula and terms against model frame names
f <- formula(wages.feis)
tt1 <- terms(wages.feis,  rhs = 1, lhs = 1)
tt2 <- terms(wages.feis,  rhs = 2, lhs = 0)
tt3 <- terms(wages.feis,  rhs = 1, lhs = 1)
test_that("Formula and terms export equal character and model / modeltrans names", {
  expect_identical(as.character(f), c("~", "lnw", "marry + enrol + yeduc + yeargr | exp + I(exp^2)"))
  expect_identical(c(all.vars(tt1), attr(tt2,"term.labels")), names(wages.feis$model))
  expect_identical(all.vars(tt1), names(wages.feis$modeltrans))
})

