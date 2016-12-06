#install.packages('openssl')
#library(devtools)
#devtools::install_github('zatonovo/lambda.r')

library(openssl)
library(lambda.r)

#https://cartesianfaith.com/2013/01/05/infinite-generators-in-r/

seq.gen(start, stop=Inf, step=1) %as%
{
  first <- value <- start - step
  function(reset=FALSE) {
    if (reset) { value <<- first; return(invisible()) }
    if (value >= stop) return(NULL)
    
    value <<- value + step
    return(value)
  }
}

iapply(iterator, fn, simplify=TRUE, formatter=function(x) format(x,"%Y-%m-%d")) %as%
{
  out <- list()
  while (! is.null(input <- iterator()))
  {
    df <- data.frame(fn(input))
    if (ncol(df) > 1)
      out[formatter(input)][[1]] <- df
    else
      out[formatter(input)] <- df
  }
  if (simplify) out <- do.call(rbind,out)
  out
}

num.fn <- seq.gen(0,Inf)

num.fn(reset=TRUE)
result_list <- list()

find_pw_letter <- function(seed, n = 0) { 
  print(n)
  hash <- md5(paste0(seed,n))
  
  if(substring(hash,1,5)=='00000'){
    return(list(value=substring(hash,6,6),count=n))
    break
  }
  
  newn <- n+1
  
  find_pw_letter(seed,newn)
  
}

find_pw_letter('abc')

ptm <- proc.time()

num.fn(reset=TRUE)
#result_list <- list()

repeat{
  n <- num.fn()
  hash <- md5(paste0('wtnhxymk',n))
  
  if(substring(hash,1,5)=='00000'){
    result_list <- append(result_list,list(list(value=substring(hash,6,6),count=n)))
    break
  }
}

proc.time() - ptm

while(continue){
  n <- num.fn()
  print(n)
  hash <- md5(paste0('abc',n))
  
  if(substring(hash,1,5)=='00000'){
    return(list(value=substring(hash,6,6),count=n))
    break
  }
}