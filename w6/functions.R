## Cross-validated residual sum of squares function
CVRSS <- function(k, y){
  n <- length(y)
  S <- matrix(0,n,n)
  b <- (k-1)/2
  if(k>1){
    for(i in 1:b){
      S[i,1:(b+i)] <- 1/(k-b+i-1)
      S[n-i+1,(n-b-i+1):n] <- 1/(k-b+i-1)
    }
    for(i in (b+1):(n-b)){
      S[i,(i-b):(i+b)] <- 1/k
    }}
  if(k==1){S <- diag(1,n)}
  s.hat <- S%*%y
  out <- sum(((y-s.hat)/(1-diag(S)))^2)
  return(out)
}

## Compute moving average
movingAverage <- function(k, y){
  n = length(y)
  S = matrix(0,n,n)
  b = (k-1)/2
  if(k>1){
    for(i in 1:b){
      S[i,1:(b+i)] = 1/(k-b+i-1)
      S[n-i+1,(n-b-i+1):n] = 1/(k-b+i-1)
    }
    for(i in (b+1):(n-b)){
      S[i,(i-b):(i+b)] = 1/k
    }}
  if(k==1){S = diag(1,n)}
  out = S%*%y
  return(out)
}

# running-line smoother
RLSmoother <- function(k,y,x){
  n = length(y)
  s.hat = rep(0,n)
  b = (k-1)/2
  if(k>1){
    for(i in 1:(b+1)){
      xi = x[1:(b+i)]
      xi = cbind(rep(1,length(xi)),xi)
      hi = xi%*%solve(t(xi)%*%xi)%*%t(xi)
      s.hat[i] = y[1:(b+i)]%*%hi[i,]
      
      xi = x[(n-b-i+1):n]
      xi = cbind(rep(1,length(xi)),xi)
      hi = xi%*%solve(t(xi)%*%xi)%*%t(xi)
      s.hat[n-i+1] = y[(n-b-i+1):n]%*%hi[nrow(hi)-i+1,]
    }
    for(i in (b+2):(n-b-1)){
      xi = x[(i-b):(i+b)]
      xi = cbind(rep(1,length(xi)),xi)
      hi = xi%*%solve(t(xi)%*%xi)%*%t(xi)
      s.hat[i] = y[(i-b):(i+b)]%*%hi[b+1,]
    }}
  if(k==1){s.hat = y}
  return(s.hat)
}


