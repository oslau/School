##preamble
setwd("/Users/Olivia/Documents/STA 232B/Lab 3")
library(MASS)
library(lme4)

##reading and parsing the data
litters = read.csv("litters.txt")
y = unlist(sapply(1:nrow(litters), function(i){
	rep(c(rep(1, litters[i, 3]), rep(0, (litters[i, 2]-litters[i, 3]))), litters[i, 1])
}))
count = 1
temp = c()
for(i in 1:nrow(litters)){
	temp = c(temp, sort(rep(seq(from = count, length.out = litters[i, 1]), litters[i, 2]))	)
	count = count + litters[i, 1]
}
mydat = data.frame(cbind(y = y, litter = temp))

m = 1328
ni = table(mydat$litter)
sum.y = aggregate(y~litter, data = mydat, sum)$y

##MCEM algorithm
K = 500 ##number of iterations

##Initialize starting values
#lmer(y ~ 1 + (1|litter), data = mydat, family = binomial)
glmmPQL(y ~ 1, random = ~1|litter, family = binomial, data = mydat)
mu = numeric(K)
sigma = numeric(K)
mu[1] = -2.19856
sigma[1] = 0.8680513

##Metropolis-Hastings
expit = function(x){
	exp(x)/(1 + exp(x))
}

MH = function(alpha.curr, alpha.next, mu, U, ni, sum.y){
	gamma = (expit(mu+alpha.next)^sum.y*(1-expit(mu+alpha.next)^(ni - sum.y)))/(expit(mu+alpha.curr)^sum.y*(1-expit(mu+alpha.curr)^(ni - sum.y)))
	alpha.new = alpha.curr
	i = which(U < gamma)
	alpha.new[i] = alpha.next[i]
	return(alpha.new)
}

sim.alpha = function(m, sigma, mu, L = 1000, sum.y, ni, burn){
	U = matrix(runif(L*m), nrow = L, ncol = m)
	alpha = matrix(0, nrow = L, ncol = m)
	alpha[1,] = rep(0, m)
	for(l in 2:L){
		alpha.next = rnorm(m, mu, sqrt(sigma))
		alpha[l, ] = MH(alpha[(l-1),], alpha.next, mu, U[(l-1),], ni, sum.y)
	}
	return(alpha[-(1:burn),])
}

#EM algorithm

E.I = function(alpha, sum.y, mu, ni){
	mean(apply(alpha, 1, function(x){
		sum(sum.y*log(expit(mu+x)) + (ni - sum.y)*log(1-expit(mu+x)))
	}))
}
E.II = function(alpha, m, sigma){
	mean(apply(alpha, 1, function(x){
		(-m/2)*log(2*pi*sigma) - (1/(2*sigma))*sum(x^2)
	}))
}
M.I = function(mu.curr, alpha, sum.y, ni){
	optimize(E.I, c(mu.curr-2, mu.curr+2), alpha = alpha, sum.y = sum.y, ni = ni, maximum = TRUE)$maximum
}
M.II = function(sigma.curr, alpha, m){
	optimize(E.II, c(0, sigma.curr*2), alpha = alpha, m = m, maximum = TRUE)$maximum
}

MCEM = function(mu, sigma, m, ni, sum.y, K, L, burn = 100){
	for(k in 2:K){
		alpha = sim.alpha(m, sigma[k-1], mu[k-1], L, sum.y, ni, burn)
		sigma[k] = M.II(sigma[k-1], alpha, m)
		mu[k] = M.I(mu[k-1], alpha, sum.y, ni)
	}
	list(sigma[-(1:burn)], mu[-(1:burn)])
}