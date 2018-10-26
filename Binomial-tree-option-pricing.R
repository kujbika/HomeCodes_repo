#binomial tree generator
generate.tree <- function(u ,d ,t, S){
tree <- matrix(0, t + 1, t + 1)
for (i in 1:(t + 1)){
  for (j in 1:i){
   tree[ j , i ] = S* u ^ ( i - j ) * d ^ ( j - 1 )
  }
}
tree
}


#value of option given the tree
value.tree=function(tree,rf = 0,K){
  Kn <- K/tree[1,1]#normalizing
  t <- tree/tree[1,1]
  r <- nrow(t)
  u <- (t[1,r])^(1/(r-1))
  d <- (t[r,r])^(1/(r-1))
  #{1,0} option value
  ov <- (1-(d/(1+rf)))/(u-d)
  lastcol <- t[,r]
  fwd <- lastcol-Kn
  fwd[ fwd < 0 ] = 0
  k <- length(fwd)
  rollback <- matrix(0,k,k)
  rollback[,1] = fwd
  for (col in 2:k){
  for (row in 1:(k+1-col)){
    rollback[row,col] = rollback[(row+1),(col-1)]/(1+rf)+(rollback[row,(col-1)]-rollback[(row+1),(col-1)])*ov
  }
  }
  rollback[ 1, k]
}

#generate, and also value a tree
value.gen.tree=function(S = 100, u = 2, d = .5, t, rf = 0, K = S){
  tree <- generate.tree(u, d, t, S)
  print(tree)
  print( paste( "the call option's price for this tree is", round( value.tree(tree, rf, K) * S, 3)))
}

value.gen.tree(t = 5)
