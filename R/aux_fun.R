#Consecutive sequence?
consecutive.seq <- function(index){
  result <- rle(diff(numbers))
  any(result$lengths>=2 & result$values==1)
}


#functions
has_spans <- function(x){
  unlist(x) %>% max %>% `>`(1)
}  