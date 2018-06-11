#' Barycentric coordinates
#' 
#' Convert points on the two-dimensional probability simplex 
#' (embedded in R3) to points on the triangle in R2. These are often
#' used in what is also called ternary plots.
#' 
#' @param v a numeric 3-vector or nx3 matrix; in the latter case, 
#'   the function is applied to the rows
#' @param delta the mesh size
#' @param simplify simplify the result to numeric vector?
#' @name simp2bary
#' @examples
#' 
#' simp2bary(c(1,0,0))
#' simp2bary(c(0,1,0))
#' simp2bary(c(0,0,1))
#' 
#' bary2simp(c(0,0))
#' bary2simp(c(1,0))
#' bary2simp(c(1/2, sqrt(3)/2))
#' 
#' samples <- rdirichlet(5, rep(1,3))
#' simp2bary(samples)
#' 
#' library(dplyr)
#' library(ggplot2); theme_set(theme_bw())
#' 
#' rdirichlet(1e3, rep(1,3)) %>% simp2bary %>% 
#'   as.data.frame %>% tbl_df %>% 
#'   ggplot(aes(V1, V2)) +
#'     geom_point() +
#'     coord_equal()
#' 
#' simplex_mesh(.05) %>% as.data.frame %>% tbl_df %>% 
#'   ggplot(aes(x, y)) + geom_point() +
#'     coord_equal()
#' 
#' simplex_mesh(.01) %>% as.data.frame %>% tbl_df %>% 
#'   ggplot(aes(x, y)) + geom_point(size = .1) +
#'     coord_equal()
#'     
#' simplex_mesh(.005) %>% as.data.frame %>% tbl_df %>% 
#'   ggplot(aes(x, y)) + geom_point(size = .001) +
#'     coord_equal()
#' 
#' simplex_mesh(.005) %>% as.data.frame %>% tbl_df %>% 
#'   ggplot(aes(x, y)) + geom_tile() +
#'     coord_equal()
#' 
#' bary2simp(simplex_mesh(.1))
#' rowSums(bary2simp(simplex_mesh(.1)))
#' 







# this is inefficient since it computes all the zero
# proj_mat <- matrix(c(
#     0, 1, 1/2,
#     0, 0, sqrt(3)/2
#   ), nrow = 2, ncol = 3, byrow = TRUE
# )

proj_mat <- matrix(c(
    1, 1/2,
    0, sqrt(3)/2
  ), nrow = 2, ncol = 2, byrow = TRUE
)


lift_mat <- matrix(c(
    1, -1/sqrt(3), 
    0, 2/sqrt(3)
  ), nrow = 2, ncol = 2, byrow = TRUE
)



#' @rdname simp2bary
#' @export
simp2bary <- function (v, simplify = TRUE) {
  
  if(is.data.frame(v)) v <- as.matrix(v)
  if(is.matrix(v)) v <- t(v)
  
  if (is.vector(v)) v <- v[-1]
  if (is.matrix(v)) v <- v[-1,]
    
  out <- t(proj_mat %*% v)
  
  if (simplify && nrow(out) == 1) out <- as.numeric(out)
  
  out
  
}




#' @rdname simp2bary
#' @export
bary2simp <- function (v, simplify = TRUE) {
  
  if(is.data.frame(v)) v <- as.matrix(v)
  if(is.matrix(v)) v <- t(v)
  
  out <- lift_mat %*% v
  out <- t(rbind(1-colSums(out), out))
  
  if (simplify && nrow(out) == 1) out <- as.numeric(out)
  
  out
  
}



#' @rdname simp2bary
#' @export
simplex_mesh <- function(delta) {
  s <- seq(0, 1, delta)
  mesh <- expand.grid(x = s, y = s)
  keepers <- apply(mesh, 1, function(v){
    x <- v[1]; y <- v[2]
    y <= (sqrt(3)/2)/(1/2)*x  &
      y <=  2*(sqrt(3)/2) - (sqrt(3)/2)/(1/2)*x
  })
  mesh[keepers,]
}

