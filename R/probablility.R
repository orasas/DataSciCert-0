#casino examples

color <- rep(c('Bl', 'Rd', 'Gr'), c(18,18,2))

n <- 100
X<- sample(ifelse( color == 'Rd', -1, 1), n, replace = T)
X[1:10]

X <- sample(c(-1,1), n ,replace = T, prob = c(9/19, 10/19))
sum(X)

n <- 1000
B<- 10000
S <- replicate(B, {
  X <- sample(c(-1,1), n ,replace = T, prob = c(9/19, 10/19))
  sum(X)
})
S[1:10]
mean(S)
mean(S < 0)
#normal
hist(S, breaks = 50)

