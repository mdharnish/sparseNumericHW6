test_that("check validity method exists", {
  validity_method <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity_method))
})

test_that("check validity method passes for valid object", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("check validity method catches errors", {
  # Helper: checks validity error message safely
  check_invalid <- function(obj, pattern) {
    # Force validity check
    err <- try(validObject(obj), silent = TRUE)

    # 1. Must fail
    if (!inherits(err, "try-error")) {
      fail(paste("Object should have failed validation but passed. Expected error containing:", pattern))
      return()
    }

    # 2. Must match pattern
    msg <- as.character(err)
    if (!grepl(pattern, msg)) {
      fail(paste0("Error message mismatch.\nExpected pattern: ", pattern, "\nActual message: ", msg))
    } else {
      succeed()
    }
  }

  # Create a valid base object
  base_x <- new("sparse_numeric", value = c(1), pos = c(1L), length = 5L)

  # 1. Test mismatch lengths
  x <- base_x
  x@value <- c(1, 2)
  check_invalid(x, "equal in size")

  # 2. Test NA values
  x <- base_x
  x@value <- as.numeric(c(NA)) # Force numeric NA
  check_invalid(x, "contain NA")

  # 3. Test index out of bounds
  x <- base_x
  x@pos <- c(6L)
  check_invalid(x, "index outside")

  # 4. Test strictly increasing pos
  x <- base_x
  x@value <- c(1, 2)
  x@pos <- c(2L, 1L)
  check_invalid(x, "strictly increasing")

  # 5. Test zeros in value
  x <- base_x
  x@value <- c(0)
  check_invalid(x, "exclude zeros")

  # 6. Test invalid length slot
  x <- base_x
  x@length <- -1L
  check_invalid(x, "positive integer")
})

test_that("check coercion return class", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
})

test_that("check coercion sparse to numeric", {
  orig <- c(0, 5, 0, 2)
  sp <- as(orig, "sparse_numeric")
  back <- as(sp, "numeric")
  expect_equal(orig, back)
})

test_that("check for show method", {
  expect_output(show(sn_vec(c(1,0,1))), "class")
})

test_that("check for plot method", {
  x <- sn_vec(c(1,0,1))
  pdf(NULL)
  expect_no_error(plot(x, x))
  expect_no_error(plot(x, sn_vec(c(0,0,0))))
  dev.off()

  expect_error(plot(sn_vec(c(1)), sn_vec(c(1,2))), "length mismatch")
})

test_that("Arithmetic methods", {
  x <- as(c(1, 0, 2, 0, 0), "sparse_numeric")
  y <- as(c(0, 1, 2, 0, 5), "sparse_numeric")

  res_add <- x + y
  expect_equal(as(res_add, "numeric"), c(1, 1, 4, 0, 5))

  res_sub <- x - y
  expect_equal(as(res_sub, "numeric"), c(1, -1, 0, 0, -5))

  res_mult <- x * y
  expect_equal(as(res_mult, "numeric"), c(0, 0, 4, 0, 0))

  expect_equal(sparse_crossprod(x, y), 4)

  z1 <- sn_vec(rep(0, 5))
  z2 <- sn_vec(rep(0, 5))
  expect_equal(as(z1 + z2, "numeric"), rep(0, 5))
  expect_equal(as(z1 - z2, "numeric"), rep(0, 5))
  expect_equal(as(z1 * z2, "numeric"), rep(0, 5))
  expect_equal(sparse_crossprod(z1, z2), 0)

  expect_error(x + sn_vec(c(1)), "must share the same `length`")
  expect_error(x - sn_vec(c(1)), "must share the same `length`")
  expect_error(x * sn_vec(c(1)), "must share the same `length`")
  expect_error(sparse_crossprod(x, sn_vec(c(1))), "must share the same `length`")
})

test_that("abs method", {
  x <- sn_vec(c(-1, 0, 2))
  expect_equal(as(abs(x), "numeric"), c(1, 0, 2))
})

test_that("mean method", {
  v <- c(1, 0, 2, 0, 5)
  sp <- sn_vec(v)
  expect_equal(mean(sp), mean(v))

  expect_equal(mean(sn_vec(c(0,0))), 0)

  x <- sn_vec(c(1))
  x@length <- 0L
  expect_error(mean(x), "non-positive length")
})

test_that("norm method", {
  v <- c(3, 4, 0)
  sp <- sn_vec(v)
  expect_equal(norm(sp), 5)

  expect_equal(norm(sn_vec(c(0,0))), 0)
})

test_that("standardize method", {
  v <- c(1, 2, 3, 0, 0)
  sp <- sn_vec(v)

  res <- standardize(sp)

  expect_s4_class(res, "sparse_numeric")

  expected <- as.vector(scale(v))
  expect_equal(as(res, "numeric"), expected)

  v_const <- c(2, 2, 2)
  sp_const <- sn_vec(v_const)
  expect_error(standardize(sp_const), "standard deviation is zero")

  expect_error(standardize(sn_vec(c(1))), "length <= 1")
})
