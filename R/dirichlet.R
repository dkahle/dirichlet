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
#' @param x vector of quantiles.
#' @param n number of observations. If length(n) > 1, the length is 
#'   taken to be the number required.
#' @param alpha dirichlet alpha parameter (numeric vector with
#'   positive entries)
#' @param log logical; if TRUE, probabilities p are given as 
#'   log(p).
#' @seealso \code{\link{dgamma}}; these functions just wrap the 
#'   (d/p/q/r)gamma functions.
#' @name dirichlet
#' @importFrom stats dgamma pgamma qgamma rgamma dbeta
#' @examples
#' 
#' ddirichlet(c(.5,.5), c(.5, .5))
#' dbeta(.5, .5, .5)
#' 
#' rdirichlet(3, c(1, 1))     # rows sum to 1
#' rdirichlet(3, c(1, 1, 1))  # rows sum to 1
#' rowSums(rdirichlet(3, c(1, 1, 1)))
#' 
#' # this function converts 3d points that sum
#' # to 1 to 2d points in a triangle (for ternary plots)
#' simp2bary <- function (v) {
#'   proj_mat <- matrix(c(
#'     0, 1, 1/2,
#'     0, 0, sqrt(3)/2
#'   ), nrow = 2, ncol = 3, byrow = TRUE)
#'   as.numeric(proj_mat %*% v)
#' }
#' simp2bary(c(1,0,0))
#' simp2bary(c(0,1,0))
#' simp2bary(c(0,0,1))
#' 
#' library(dplyr)
#' library(ggplot2); theme_set(theme_bw())
#' samples <- rdirichlet(2e3, rep(1,3))
#' head(samples)
#' 
#' # project the points into two dimensions
#' bary_samples <- apply(samples, 1, simp2bary) %>% 
#'   t %>% as.data.frame %>% tbl_df
#' names(bary_samples) <- c("dim1", "dim2")
#' qplot(dim1, dim2, data = bary_samples) +
#'   coord_equal()
#' 
#' 
#' 
#' 
#' # visualize a density
#' n <- 201
#' s <- seq(0, 1, length.out = n)[c(-1,-n)]
#' mesh <- expand.grid(s, s, s) %>% 
#'   filter(abs(Var1 + Var2 + Var3 - 1) < 1e-5)
#'   
#' mesh_proj <- apply(mesh, 1, simp2bary) %>% 
#'   t %>% as.data.frame %>% tbl_df
#' 
#' f <- function(v) ddirichlet(v, c(10, 10, 5))
#' mesh_proj$f <- apply(mesh, 1, f)
#' 
#' qplot(V1, V2, data = mesh_proj, color = f, size = I(.2)) +
#'   coord_equal()
#'   
#'   
NULL




#' @rdname dirichlet
#' @export
ddirichlet <- function(x, alpha, log = FALSE) {
  if (is.matrix(x)) return( apply(x, 1, ddirichlet) )
  if( any(x < 0)  ) return(0L)
  tol <- .Machine$double.eps
  if( abs(sum(x) - 1) > tol ) return(0L)
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



