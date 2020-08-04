#############################
#### S3 methods for feis ####
#############################
context("Test methods")

data("mwp", package = "feisr")

### Predict and fitted and residuals

wages.feis <- feis(lnw ~ marry + enrol + yeduc + yeargr
                   | exp + I(exp^2), data = mwp, id = "id")
wages.feis.rob <- feis(lnw ~ marry + enrol + yeduc + yeargr
                   | exp + I(exp^2), data = mwp, id = "id", robust = TRUE)


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



### Prediction ses and intervals
mwp_trans <- data.frame(lnw = wages.feis$response, model.matrix(wages.feis))
wages.lmt <- lm(lnw ~ . -1, data = mwp_trans)

wages.lmt$df.residual <- df.residual(wages.feis)

wages.feis.pred1 <- predict(wages.feis, se.fit = TRUE,
                            interval = "prediction", level = 0.95)
suppressWarnings(
wages.lmt.pred1 <- predict.lm(wages.lmt, se.fit = TRUE,
                           interval = "prediction", level = 0.95,
                           df = df.residual(wages.feis))
)

wages.feis.pred2 <- predict(wages.feis, se.fit = TRUE,
                            interval = "confidence", level = 0.95)
wages.lmt.pred2 <- predict.lm(wages.lmt, se.fit = TRUE,
                           interval = "confidence", level = 0.95,
                           df = df.residual(wages.feis))

newdata <- data.frame(marry = rep(mean(mwp_trans$marry), 100),
                      enrol = rep(mean(mwp_trans$enrol), 100),
                      yeduc = seq(-0.1, 0.1, length.out = 100),
                      yeargr = rep(mean(mwp_trans$yeargr), 100))

wages.feis.pred3 <- predict(wages.feis, se.fit = TRUE, newdata = newdata,
                            interval = "confidence", level = 0.90)
wages.lmt.pred3 <- predict.lm(wages.lmt, se.fit = TRUE, newdata = newdata,
                           interval = "confidence", level = 0.90,
                           df = df.residual(wages.feis))

wages.feis.pred4 <- predict(wages.feis, se.fit = TRUE, newdata = newdata,
                            interval = "prediction", level = 0.90)
suppressWarnings(
wages.lmt.pred4 <- predict.lm(wages.lmt, se.fit = TRUE, newdata = newdata,
                           interval = "prediction", level = 0.90,
                           df = df.residual(wages.feis))
)

wages.feis.pred5 <- predict(wages.feis.rob, se.fit = TRUE, newdata = newdata,
                            interval = "confidence", level = 0.90)
wages.lmt.pred5 <- predict.lm(wages.lmt, se.fit = TRUE, newdata = newdata,
                           interval = "confidence", level = 0.90,
                           df = df.residual(wages.feis))

all.equal(wages.feis.pred1, wages.lmt.pred1)
all.equal(wages.feis.pred2, wages.lmt.pred2)
all.equal(wages.feis.pred3, wages.lmt.pred3)
all.equal(wages.feis.pred4, wages.lmt.pred4)
all.equal(wages.feis.pred5, wages.lmt.pred5)

test_that("Predicted se and inteval == predict.lm on transformed data", {
  expect_equal(wages.feis.pred1, wages.lmt.pred1, check.names = FALSE)
  expect_equal(wages.feis.pred2, wages.lmt.pred2, check.names = FALSE)
  expect_equal(wages.feis.pred3, wages.lmt.pred3)
  expect_equal(wages.feis.pred4, wages.lmt.pred4)
})



# ### Example plot
# library(ggplot2)
# wages.feis2 <- feis(lnw ~ marry + enrol + yeduc + age
#                    | exp + I(exp^2), data = mwp, id = "id")
# mwp_trans <- data.frame(lnw = wages.feis2$response, model.matrix(wages.feis2))
#
# newdata <- data.frame(marry = rep(mean(mwp_trans$marry), 100),
#                       enrol = rep(mean(mwp_trans$enrol), 100),
#                       yeduc = rep(mean(mwp_trans$yeduc), 100),
#                       age = seq(-3, 3, length.out = 100))
# wages.feis.pred4 <- predict(wages.feis2, se.fit = TRUE, newdata = newdata,
#                             interval = "confidence", level = 0.95)
# pred <- cbind(newdata, wages.feis.pred4$fit)
# fp <- ggplot(data = pred, aes(x = age, y = fit)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = "grey70")
# fp
#
#
# mwp_trans <- data.frame(lnw = wages.feis2$response, model.matrix(wages.feis2))
#
# newdata <- data.frame(marry = rep(mean(mwp$marry), 100),
#                       enrol = rep(mean(mwp$enrol), 100),
#                       yeduc = rep(mean(mwp$yeduc), 100),
#                       age = seq(-10, 10, length.out = 100))
# wages.feis.pred4 <- predict(wages.feis2, se.fit = TRUE, newdata = newdata,
#                             interval = "confidence", level = 0.95)
# pred <- cbind(newdata, wages.feis.pred4$fit)
# fp <- ggplot(data = pred, aes(x = age, y = fit)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = "grey70")
# fp
#
#
# newdata <- data.frame(marry = rep(mean(mwp_trans$marry), 100),
#                       enrol = rep(mean(mwp_trans$enrol), 100),
#                       yeduc = seq(-3, 3, length.out = 100),
#                       age = rep(mean(mwp_trans$yeduc), 100))
# wages.feis.pred4 <- predict(wages.feis2, se.fit = TRUE, newdata = newdata,
#                             interval = "confidence", level = 0.95)
# pred <- cbind(newdata, wages.feis.pred4$fit)
# fp <- ggplot(data = pred, aes(x = yeduc, y = fit)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = "grey70")
# fp
#
#
# wages.lm <- lm(lnw ~ marry + enrol + yeduc + as.factor(yeargr), data = mwp)
# newdata <- data.frame(marry = rep(mean(mwp$marry), 100),
#                       enrol = rep(mean(mwp$enrol), 100),
#                       yeduc = seq(7, 20, length.out = 100),
#                       yeargr = factor(as.character(median(mwp$yeargr)),
#                                       levels=c("1", "2", "3", "4", "5")))
# wages.lm.pred <- predict(wages.lm, se.fit = TRUE, newdata = newdata,
#                          interval = "confidence", level = 0.95)
#
# pred <- cbind(newdata, wages.lm.pred$fit)
# fp <- ggplot(data = pred, aes(x = yeduc, y = fit)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = "grey70")
# fp
#
# wages.lm.pred <- predict(wages.lm, se.fit = TRUE,
#                          interval = "confidence", level = 0.90)
#
# pred <- cbind(mwp, wages.feis.pred3$fit)
# pred <- aggregate(pred, list(educ = pred$yeduc), mean)
# fp <- ggplot(data = pred, aes(x = yeduc, y = fit)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = "grey70")
# fp

