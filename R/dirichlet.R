#' The Dirichlet Distribution
#' 
#' Density, distribution function, quantile function and random 
#' generation for the dirichlet distribution.
#' 
#' The functions (d/r)dirichlet simply wrap those of the
#' standard (d/r)gamma R implementation, so look at, say, 
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
#'   (d/r)gamma functions.
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
#' 
#' library(dplyr)
#' library(ggplot2); theme_set(theme_bw())
#' 
#' samples <- rdirichlet(2e3, rep(1,3))
#' head(samples)
#' 
#' # project the points into two dimensions
#' bary_samples <- simp2bary(samples) %>% as.data.frame %>% tbl_df
#' names(bary_samples) <- c("dim1", "dim2")
#' qplot(dim1, dim2, data = bary_samples) +
#'   coord_equal()
#' 
#' 
#' 
#' 
#' # visualize a density
#' f <- function(v) ddirichlet(v, c(20, 10, 5))
#' mesh <- simplex_mesh(.0025) %>% as.data.frame %>% tbl_df
#' mesh$f <- mesh %>% apply(1, function(v) f(bary2simp(v)))
#'   
#' p <- ggplot(mesh, aes(x, y)) +
#'   geom_raster(aes(fill = f)) +
#'   coord_equal(xlim = c(0,1), ylim = c(0, .85))
#' p
#'   
#' points <- rdirichlet(1e3, c(20, 10, 5)) %>% simp2bary %>% 
#'   as.data.frame %>% tbl_df %>% rename(x = V1, y = V2)

#' p + geom_point(data = points, color = "orange", alpha = .3)
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







#' @rdname dirichlet
#' @export
rdirichlet <- function(n, alpha) {
  normalize <- function(.) . / sum(.)
  samps <- vapply(alpha, function(al) rgamma(n, al, 1), numeric(n))
  if (n == 1) {
    matrix(normalize(samps), nrow = 1, ncol = length(samps))
  } else {
    t(apply(samps, 1, normalize))
  }  
}



