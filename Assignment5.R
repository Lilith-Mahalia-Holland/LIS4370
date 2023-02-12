a <- matrix(1:100, nrow=10)  
b <- matrix(1:1000, nrow=10)

#det of a
det(a) # = 0

# inverse of a
solve(a) # = 0, singular matrix




#det of b
det(t(b) %*% b) # = 0
# or
# this errors
det(b) # = error, not square matrix

# since this is not a square matrix solve does not inverse the matrix
solve(b) # = error, not square matrix
# by transposing and then matrix multiplying by itself a square matrix is created
solve(t(b) %*% b) # = 0, singular matrix