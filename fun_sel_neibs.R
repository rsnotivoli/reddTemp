#'Neighbouring stations' selection
#'
#'This function returns the names of the sorted nearest stations (from distances matrix)
#'@param n index
#'@param nams vector of names

sel_neib <- function(n, nams){
  w <- which(n == 0)
  if(length(w) > 0) n[w] <- 10000 #moves candidate far away
  res <- as.character(nams[sort(n, index.return = T)$ix])
  return(res)
}