ch0_to_Na <- function(x, str = ""){
  
  y <- ifelse(length(x) == 0 || is.na(x), str, x)
  
  return(y)
  
}