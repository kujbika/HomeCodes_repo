#
#brownian bridge from zero to zero
#
#
bridge <- function(){
H0 <- 0 #where to start
H1 <- 0 #where to go
n <- 14
H <- rep( 0,( 2 ^ n + 1))
H[1] <- H0
H[2^n+1] <- H1
for (i in 1:n){
  x <- 2 ^ ( n -i )
  index <- ( 1 : 2 ^ ( i - 1 )) * ( 2 ^ ( n + 1 - i ) ) - x + 1 
  for (j in 1 : (2 ^ ( i - 1 ) ) ){
    dif <- 2 ^ ( n - i )
    var <- 1 / ( 2 ^ ( i + 1 ) )
    H[ index[ j ] ] = ( H[ ( index[ j ] - dif ) ] + H [ (index[ j ] + dif ) ] ) / 2 + rnorm( 1 , 0 , var ^ 0.5)
  }
}
plot( H , type = "l", col = 3,main = "Brownian bridge", xlab = "Time" , ylab ="H(t)" )
abline( h = c( 0, 1 ), col = 2 )
}

bridge()

