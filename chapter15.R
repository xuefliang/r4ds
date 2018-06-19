rescale01 <- function(x){
  rng <- range(x,na.rm = T,finite=T)
  (x-rng[1])/(rng[2]-rng[1])
}
rescale01(c(0.5,10))

has_name <- function(x){
  nms <- names(x)
  if(is.null(nms)){
    rep(F,length(x))
  }else{
    !is.na(nms) & nms !=''
  }
}
test <- c(1,2,3)
has_name(test)

x <- sqrt(2)^2
x==2

dplyr::near(2,x)

wt_mean <- function(x,w,na.rm=F){
  stopifnot(is.logical(na.rm),length(na.rm)==1)
  stopifnot(length(x)==length(w))
  
  if(na.rm){
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- x[!miss]
  }
  sum(w*x)/sum(x)
}

commas <- function(...){
  stringr::str_c(...,collapse=',')
}

commas(letters[1:10])

rule <- function(...,pad='-'){
  title <- paste0(...)
  width <- getOption('width')-nchar(title)-5
  cat(title,' ',stringr::str_dup(pad,width),'\n',sep='')
}

rule("Important output")

x <- c(1,2)
sum(x,na.rm = T)

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}






