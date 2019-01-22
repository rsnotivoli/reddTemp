#'Distances
#'
#'This function computes and saves distances between stations. Requires "fields" package and  X and Y in meters (UTM)
#'@param sts data.frame with stations information (at least columns X and Y must exist).

.distances <- function(sts){
  x1 <- cbind(sts$X, sts$Y)
  x2 <- x1
  distanc <- rdist(x1, x2)/1000
  colnames(distanc) = sts$ID
  rownames(distanc) = sts$ID
  return(distanc)
}

