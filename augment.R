#' Define augmented data for stabilizing logreg and polyreg (function taken from `mice`)
#'
#' @export
#' @param x numeric data.frame (n rows)
#' @param y factor or numeric vector (lengt n)
#' @param data data.frame.
#' @author Stefan van Buuren 
#' @return a list with elements y, ry, x, and w with length n+2*(ncol(x))*length(levels(y))
#'
#' White, I., Daniel, R. and Royston, P (2010). Avoiding bias due to perfect
#' prediction in multiple imputation of incomplete categorical variables.
#' Computational Statistics and Data Analysis, 54:22672275.

#'
augment <- function(y, ry, x, wy, maxcat = 50) {
  # 
  # by the ad hoc procedure of White, Daniel & Royston, CSDA, 2010
  # This function will prevent augmented data beyond the min and
  # the max of the data
  # Input:
  # 
  # y: factor or numeric vector (lengt n)
  # ry: logical vector (length n)
  # Output:
  # return a list with elements y, ry, x, and w with length n+2*(ncol(x))*length(levels(y))
  # SvB May 2009
  icod <- sort(unique(unclass(y)))
  k <- length(icod)
  if (k > maxcat) 
    stop("Maximum number of categories (", maxcat, ") exceeded")
  p <- ncol(x)
  
  # skip augmentation if there are no predictors
  if (p == 0) 
    return(list(y = y, ry = ry, x = x, wy = wy, w = rep(1, length(y))))
  
  ## skip augmentation if there is only 1 missing value 12jul2012 
  ## this need to be fixed 12jul2011
  if (sum(!ry) == 1) 
    return(list(y = y, ry = ry, x = x, wy = wy, w = rep(1, length(y))))
  
  # calculate values to augment
  mean <- apply(x, 2, mean, na.rm = TRUE)
  sd <- sqrt(apply(x, 2, var, na.rm = TRUE))
  minx <- apply(x, 2, min, na.rm = TRUE)
  maxx <- apply(x, 2, max, na.rm = TRUE)
  nr <- 2 * p * k
  a <- matrix(mean, nrow = nr, ncol = p, byrow = TRUE)
  b <- matrix(rep(c(rep.int(c(0.5, -0.5), k), rep.int(0, nr)), length = nr * p), nrow = nr, ncol = p, byrow = FALSE)
  c <- matrix(sd, nrow = nr, ncol = p, byrow = TRUE)
  d <- a + b * c
  d <- pmax(matrix(minx, nrow = nr, ncol = p, byrow = TRUE), d, na.rm = TRUE)
  d <- pmin(matrix(maxx, nrow = nr, ncol = p, byrow = TRUE), d, na.rm = TRUE)
  e <- rep(rep(icod, each = 2), p)
  
  dimnames(d) <- list(paste0("AUG", seq_len(nrow(d))), dimnames(x)[[2]])
  xa <- rbind.data.frame(x, d)
  # beware, concatenation of factors
  ya <- if (is.factor(y)) as.factor(levels(y)[c(y, e)]) else c(y, e)
  rya <- c(ry, rep.int(TRUE, nr))
  wya <- c(wy, rep.int(FALSE, nr))
  wa <- c(rep.int(1, length(y)), rep.int((p + 1)/nr, nr))
  
  return(list(y = ya, ry = rya, x = xa, w = wa, wy = wya))
}