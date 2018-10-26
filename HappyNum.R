#A number is happy if:
#       1. It is 1
#       2. Its digits' squared sum is 1
#       3. 
#           If its digits' square sum is bigger than 9, than we replace the number with the sum and start again, until we get a number less than 10.
#           The result is 1.


HappyNum <- function(n){
  m = sum((as.numeric( unlist( strsplit( as.character( n ), "") ) ) ) ^ 2)
  if ( m == 1) return("This is a happy number")
  if(m < 10)  print("This is not a happy number")
  else HappyNum( m )
}
HappyNum( 13 )
HappyNum( 141411201012543634 )
