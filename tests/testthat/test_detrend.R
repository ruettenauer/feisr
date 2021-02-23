##################################
#### Test for detrend results ####
##################################
context("Test detrend function")


data(mwp, package = "feisr")

# Reduce data
mwp <-  mwp[, c("id", "lnw", "marry", "enrol", "yeduc", "yeargr", "exp", "expq")]
mwp$yeargr <- as.factor(mwp$yeargr)

### Estimate FEIS and lm on detrended
wages.feis <- feis(lnw ~ marry + enrol + yeduc + yeargr
                   | exp + expq, data = mwp, id = "id")

mwp_det <- detrend(mwp, c("exp", "expq"), "id")

wages.lm <- lm(lnw ~ - 1 + marry + enrol + yeduc + yeargr2 + yeargr3 + yeargr4 + yeargr5,
               data = mwp_det)



### Test equal detrend and FEIS model frame
mf <- data.frame(model.matrix(wages.feis))
test_that("Detrend == FEIS model.frame", {
  expect_equal(mf, mwp_det[, - which(names(mwp_det) %in%  c("lnw", "yeargr1"))], tolerance = .000001)
  expect_equal(unname(wages.feis$response), mwp_det$lnw, tolerance = .000001)
})
test_that("Coefs lm Detrend == FEIS coefs", {
  expect_equal(coefficients(wages.feis), coefficients(wages.lm), tolerance = .000001)
})


### Compare to detrended in loop
mwp$det_lnw <- NA
for(i in unique(mwp$id)){
  oo <- which(mwp$id == i)
  tmp <- mwp[oo, ]
  fi <- fitted(lm(lnw ~ exp + expq, data = tmp))
  mwp$det_lnw[oo] <- mwp$lnw[oo] - fi
}

test_that("Detrend == x - fit(lm) in loop ", {
  expect_equal(mwp_det$lnw, mwp$det_lnw, tolerance = .000001)
})


### Overall detrend



### Check NA handling
set.seed(1234)

rownames(mwp) <- sample(1:10000, nrow(mwp))

mwp[sample(1:nrow(mwp), 200), "lnw"] <- NA
mwp[sample(1:nrow(mwp), 200), "exp"] <- NA

# Warning for small groups
test_that("Warning drop groups", {
  expect_warning(mwp_det <- detrend(mwp[, c("lnw", "enrol")], mwp$exp, mwp$id))
})
suppressWarnings(mwp_det <- detrend(mwp[, c("lnw", "enrol")], mwp$exp, mwp$id))

# Rownames equal
test_that("Keep rownames in detrend", {
  expect_identical(rownames(mwp), rownames(mwp_det))
})


na1 <- which(is.na(mwp$lnw))
na2 <- which(is.na(mwp$exp))

# NA returned in detrend
test_that("NA returned in detrend", {
  expect_true(all(is.na(mwp_det$lnw[na1])))
  expect_true(all(is.na(mwp_det$lnw[na2])))
  expect_true(all(is.na(mwp_det$enrol[na1])))
  expect_true(all(is.na(mwp_det$enrol[na2])))

  expect_false(all(is.na(mwp_det$lnw[-unique(c(na1, na2))])))
  expect_false(all(is.na(mwp_det$enrol[-unique(c(na1, na2))])))
})


# Na omit option
suppressWarnings(mwp_det2 <- detrend(mwp[, c("lnw", "enrol")], mwp$exp, mwp$id, na.action = "na.omit"))
test_that("na.omit option in detrend", {
  expect_equal(nrow(mwp_det2), 3100 - length(which(is.na(mwp_det$lnw))), tolerance = .000001)
  expect_false(any(is.na(mwp_det2)))
})


### Overall detrend
det_overall1 <- detrend(mwp[, c("lnw")], mwp$exp, 1)
det_overall2 <- detrend(mwp[, c("lnw")], mwp$exp, 1, intercept = FALSE)

det_fit1 <- mwp$lnw - fitted(lm(lnw ~ exp, data = mwp, na.action = na.exclude))
det_fit2 <- mwp$lnw - fitted(lm(lnw ~ exp - 1, data = mwp, na.action = na.exclude))
test_that("Overall detrend = X - fitted lm", {
  expect_equal(det_overall1, det_fit1, tolerance = .000001, check.names = FALSE)
  expect_equal(det_overall2, det_fit2, tolerance = .000001, check.names = FALSE)
})



### Check errors
test_that("NA returned in detrend", {
  expect_error(detrend(mwp[, c("lnw", "enrol")], mwp$exp, mwp$id[1:100]))
  expect_error(detrend(mwp, "exp3", mwp$id))
  expect_error(detrend(mwp, "exp", mwp$idbla))
  expect_error(detrend(mwp, "exp", "idbla"))
})




### Check demeaning
dem_id1 <- detrend(mwp[, c("lnw", "marry")], 1, mwp$id)
dem_id2 <- data.frame(apply(mwp[, c("lnw", "marry")], 2,
                      FUN = function(x) x - ave(x, mwp$id,
                      FUN = function(z) mean(z, na.rm = TRUE))))

dem_ov1 <- detrend(mwp[, c("lnw", "marry")], 1, 1)
dem_ov2 <- data.frame(apply(mwp[, c("lnw", "marry")], 2, FUN = function(x) x - mean(x, na.rm = TRUE)))

test_that("De-meaning (only consant in slopes)", {
  expect_equal(complete.cases(dem_id1), complete.cases(dem_id2), tolerance = .000001, check.names = FALSE)
  expect_equal(complete.cases(dem_ov1), complete.cases(dem_ov2), tolerance = .000001, check.names = FALSE)
})
