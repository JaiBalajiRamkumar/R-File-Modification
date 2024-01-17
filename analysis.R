# Monte Calro Integration
set.seed(1)
n <- 1000
u <- runif(n, 0, 1)
g_u <- exp(-u)
mean(g_u)

-exp(-1) + 1 # real integration calculation for comparison


# Optimization: Finding a root 
g <- function(x){
	4*x^5 + 12*x^4 + 16*x^3 -12*x^2 - 18*x + 15
}

curve(g, xlim = c(-2, 1.5), col = "blue", lwd = 1.5)
abline(h=0, lty = 2)

g(-1.5) # -8.625 < 0
g(-1) # 13 > 0

g(-1.25) # 4.589844 > 0

g(-1.375) # -1.297241 < 0

g(-1.3125) # 1.80827 > 0

g(-1.34375) # 0.2980543 > 0

g(-1.359375) # -0.4886841 < 0

g(-1.351562) # -0.09259752 < 0

# Naive implementation of bisection method
a <- -1.5
b <- -1
for(i in 1:20){
	m <- (a+b)/2
	if (g(a)*g(m) < 0){
		b <- m
	}
	else {
		a <- m
	}
	print(m, digits = 10)
}
