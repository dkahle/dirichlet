#' The Dirichlet Distribution
#' 
#' Density, distribution function, quantile function and random 
#' generation for the dirichlet distribution.
#' 
#' The functions (d/p/q/r)dirichlet simply wrap those of the
#' standard (d/p/q/r)gamma R implementation, so look at, say, 
#' \code{\link{dgamma}} for details.
#' 
#' 
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is 
#'   taken to be the number required.
#' @param alpha dirichlet alpha parameter (numeric vector with
#'   positive entries)
#' @param log,log.p logical; if TRUE, probabilities p are given as 
#'   log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are 
#'   P[X <= x] otherwise, P[X > x].
#' @seealso \code{\link{dgamma}}; these functions just wrap the 
#'   (d/p/q/r)gamma functions.
#' @name dirichlet
#' @importFrom stats dgamma pgamma qgamma rgamma dbeta
#' @examples
#' 
#' ddirichlet(c(.5,.5), c(.5, .5))
#' dbeta(.5, .5, .5)
#' 
#' rdirichlet(10, c(.5, .5))
#' 
NULL




#' @rdname dirichlet
#' @export
ddirichlet <- function(x, alpha, log = FALSE) {
  if (is.matrix(x)) {
    return( apply(x, 1, ddirichlet) )
  }
  if( sum(x) != 1 ) return(0L)
  stopifnot(length(x) == length(alpha))
  log_f <- sum((alpha - 1)*log(x)) + lgamma(sum(alpha)) - sum(lgamma(alpha))
  if(log) return(log_f)
  exp(log_f)
}




# #' @rdname dirichlet
# #' @export
# pdirichlet <- function(q, alpha, lower.tail = TRUE, log.p = FALSE) {
#   pchisq(q^2, df, ncp, lower.tail, log.p)
# }





# #' @rdname dirichlet
# #' @export
# qdirichlet <- function(p, alpha, lower.tail = TRUE, log.p = FALSE) {
#   sqrt( qchisq(p, df, ncp, lower.tail, log.p) )
# }





#' @rdname dirichlet
#' @export
rdirichlet <- function(n, alpha) {
  normalize <- function(.) . / sum(.)
  samps <- sapply(alpha, function(al) rgamma(n, al, 1))
  t(apply(samps, 1, normalize))
}



