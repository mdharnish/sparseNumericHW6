# -------------------------------------------------------------------
# CLASS DEFINITION
# -------------------------------------------------------------------

#' sparse_numeric Class
#'
#' An S4 class for representing sparse numeric vectors, storing only non-zero
#' values and their positions.
#'
#' @slot value numeric. Non-zero values.
#' @slot pos integer. 1-based positions of the non-zero values.
#' @slot length integer. Total length of the vector.
#'
#' @importFrom methods as new show
#' @importFrom graphics points
#'
#' @exportClass sparse_numeric
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

setValidity("sparse_numeric", function(object) {
  errors <- character()
  if (!(length(object@length) == 1L && !is.na(object@length) && object@length > 0L))
    errors <- c(errors, "slot `length` must be one positive integer.")
  if (length(object@value) != length(object@pos))
    errors <- c(errors, "slots `value` and `pos` must be equal in size.")
  if (anyNA(object@pos) || anyNA(object@value))
    errors <- c(errors, "`pos` and `value` must not contain NA.")
  if (length(object@pos) && (min(object@pos) < 1L || max(object@pos) > object@length))
    errors <- c(errors, "slot `pos` contains an index outside 1..length.")
  if (length(object@pos) > 1L && !all(diff(object@pos) > 0L))
    errors <- c(errors, "`pos` must be strictly increasing (no repeats).")
  if (length(object@value) && !anyNA(object@value) && any(object@value == 0))
    errors <- c(errors, "stored `value` must exclude zeros.")
  if (length(errors)) errors else TRUE
})

# -------------------------------------------------------------------
# COERCION METHODS (DO NOT DOCUMENT OR EXPORT)
# -------------------------------------------------------------------

setAs("numeric", "sparse_numeric", function(from) {
  idx <- which(from != 0)
  new("sparse_numeric",
      value  = unname(as.numeric(from[idx])),
      pos    = as.integer(idx),
      length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  res <- numeric(from@length)
  if (length(from@pos)) res[from@pos] <- from@value
  res
})

# -------------------------------------------------------------------
# CONSTRUCTOR
# -------------------------------------------------------------------

#' Construct a sparse_numeric object
#'
#' @param x A numeric vector.
#'
#' @return A sparse_numeric object.
#' @export
sn_vec <- function(x) as(x, "sparse_numeric")

# -------------------------------------------------------------------
# GENERICS
# -------------------------------------------------------------------

#' Generic for sparse addition
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Ignored.
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' Generic for sparse subtraction
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Ignored.
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' Generic for sparse multiplication
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Ignored.
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' Generic for sparse crossproduct
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Ignored.
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' Generic for sparse Euclidean norm
#' @param x A sparse_numeric object.
#' @param ... Ignored.
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' Generic for standardizing sparse vectors
#' @param x A sparse_numeric object.
#' @param ... Ignored.
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

# -------------------------------------------------------------------
# METHODS
# -------------------------------------------------------------------

#' Sparse Addition Method
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Ignored
#' @export
setMethod(
  "sparse_add",
  signature("sparse_numeric","sparse_numeric"),
  function(x, y, ...) {
    if (x@length != y@length) stop("vectors must share the same `length` slot")
    if (!length(x@pos) && !length(y@pos))
      return(new("sparse_numeric", value=numeric(), pos=integer(), length=x@length))
    agg_pos <- c(x@pos, y@pos)
    agg_val <- c(x@value, y@value)
    s <- tapply(agg_val, agg_pos, sum)
    keep <- s != 0
    new("sparse_numeric",
        value = as.numeric(s[keep]),
        pos   = as.integer(names(s)[keep]),
        length = x@length)
  }
)

#' Sparse Subtraction Method
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Ignored
#' @export
setMethod(
  "sparse_sub",
  signature("sparse_numeric","sparse_numeric"),
  function(x, y, ...) {
    if (x@length != y@length) stop("vectors must share the same `length` slot")
    if (!length(x@pos) && !length(y@pos))
      return(new("sparse_numeric", value=numeric(), pos=integer(), length=x@length))
    agg_pos <- c(x@pos, y@pos)
    agg_val <- c(x@value, -y@value)
    s <- tapply(agg_val, agg_pos, sum)
    keep <- s != 0
    new("sparse_numeric",
        value = as.numeric(s[keep]),
        pos   = as.integer(names(s)[keep]),
        length = x@length)
  }
)

#' Sparse Multiplication Method
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Ignored
#' @export
setMethod(
  "sparse_mult",
  signature("sparse_numeric","sparse_numeric"),
  function(x, y, ...) {
    if (x@length != y@length) stop("Vectors must share the same `length`")
    if (!length(x@pos) || !length(y@pos))
      return(new("sparse_numeric", value=numeric(), pos=integer(), length=x@length))
    common <- x@pos[x@pos %in% y@pos]
    if (!length(common))
      return(new("sparse_numeric", value=numeric(), pos=integer(), length=x@length))
    idx_x <- match(common, x@pos)
    idx_y <- match(common, y@pos)
    v <- x@value[idx_x] * y@value[idx_y]
    new("sparse_numeric", value=v, pos=as.integer(common), length=x@length)
  }
)

#' Sparse Crossproduct Method
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Ignored
#' @export
setMethod(
  "sparse_crossprod",
  signature("sparse_numeric","sparse_numeric"),
  function(x, y, ...) {
    if (x@length != y@length) stop("Vectors must share the same `length`")
    if (!length(x@pos) || !length(y@pos)) return(0)
    common <- x@pos[x@pos %in% y@pos]
    if (!length(common)) return(0)
    idx_x <- match(common, x@pos)
    idx_y <- match(common, y@pos)
    sum(x@value[idx_x] * y@value[idx_y])
  }
)

# -------------------------------------------------------------------
# OPERATORS
# -------------------------------------------------------------------

#' Sparse Addition Operator
#' @param e1 A sparse_numeric object
#' @param e2 A sparse_numeric object
#' @export
setMethod("+", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' Sparse Subtraction Operator
#' @param e1 A sparse_numeric object
#' @param e2 A sparse_numeric object
#' @export
setMethod("-", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' Sparse Multiplication Operator
#' @param e1 A sparse_numeric object
#' @param e2 A sparse_numeric object
#' @export
setMethod("*", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

# -------------------------------------------------------------------
# SHOW / PLOT
# -------------------------------------------------------------------

#' Show Method for sparse_numeric
#' @param object A sparse_numeric object
#' @export
setMethod("show", "sparse_numeric", function(object) {
  nnz <- length(object@pos)
  cat("{",
      "\"class\":\"sparse_numeric\", ",
      "\"len\":", object@length, ", ",
      "\"nnz\":", nnz, ", ",
      "\"pos\":[", paste(object@pos, collapse=","), "], ",
      "\"val\":[", paste(format(object@value), collapse=","), "]",
      "}\n", sep="")
})

#' Plot Method for sparse_numeric
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Graphic parameters
#' @export
setMethod(
  "plot",
  signature(x="sparse_numeric", y="sparse_numeric"),
  function(x, y, ...) {
    if (x@length != y@length)
      stop("length mismatch detected")
    match_pts <- intersect(x@pos, y@pos)
    plot(NA, xlim=c(1,x@length), ylim=c(0,1),
         xlab="Index", ylab="",
         main=if (length(match_pts))
           sprintf("Shared non-zero indices (%d total)", length(match_pts))
         else "No shared non-zero entries found",
         ...)
    if (length(match_pts))
      points(match_pts, rep(1, length(match_pts)), pch=19)
  }
)

# -------------------------------------------------------------------
# OTHER METHODS
# -------------------------------------------------------------------

#' Absolute Value Method
#' @param x A sparse_numeric object
#' @export
setMethod(
  "abs",
  signature(x="sparse_numeric"),
  function(x) {
    new("sparse_numeric",
        value = abs(x@value),
        pos   = x@pos,
        length = x@length)
  }
)

#' Mean Method
#' @param x A sparse_numeric object
#' @param ... Ignored
#' @export
setMethod(
  "mean",
  signature(x="sparse_numeric"),
  function(x, ...) {
    if (x@length <= 0L)
      stop("sparse_numeric object has non-positive length")
    sum(x@value) / as.numeric(x@length)
  }
)

#' Euclidean Norm Method
#' @param x A sparse_numeric object
#' @param ... Ignored
#' @export
setMethod(
  "norm",
  signature(x="sparse_numeric"),
  function(x, ...) {
    v <- x@value
    if (!length(v)) return(0)
    sqrt(sum(v * v))
  }
)

#' Standardize Method
#' @param x A sparse_numeric object
#' @param ... Ignored
#' @export
setMethod(
  "standardize",
  signature(x="sparse_numeric"),
  function(x, ...) {
    n <- x@length
    if (n <= 1L)
      stop("cannot standardize a vector of length <= 1")

    v <- x@value
    sum_v  <- sum(v)
    sum_v2 <- sum(v*v)

    mean_x <- sum_v / n
    var_num <- sum_v2 - (sum_v^2)/n
    if (var_num == 0)
      stop("standard deviation is zero; cannot standardize")
    sd_x <- sqrt(var_num / (n - 1L))

    nz_pos  <- x@pos
    nz_vals <- (v - mean_x) / sd_x

    all_pos  <- seq_len(n)
    zero_pos <- all_pos[!(all_pos %in% nz_pos)]

    if (length(zero_pos)) {
      zero_vals <- rep((-mean_x)/sd_x, length(zero_pos))
      pos_all <- c(nz_pos, zero_pos)
      val_all <- c(nz_vals, zero_vals)
    } else {
      pos_all <- nz_pos
      val_all <- nz_vals
    }

    ord <- order(pos_all)
    pos_all <- as.integer(pos_all[ord])
    val_all <- as.numeric(val_all[ord])

    keep <- val_all != 0
    if (!any(keep)) {
      new("sparse_numeric", value=numeric(),
          pos=integer(), length=n)
    } else {
      new("sparse_numeric",
          value=val_all[keep],
          pos=pos_all[keep],
          length=n)
    }
  }
)
