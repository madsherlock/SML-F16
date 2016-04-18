#Overlapping Coefficient computer 
#Source: http://stats.stackexchange.com/questions/12209/percentage-of-overlapping-regions-of-two-normal-distributions
#Input x = number of obs, (mu1,sd_1) = (µ_1,std_1) -  Model ,(mu2,sd_2) = (µ_2,std_2) - Test
#Weitzman's impl. 

min.f1f2 <- function(x, mu1, mu2, sd1, sd2) {
  f1 <- dnorm(x, mean=mu1, sd=sd1)
  f2 <- dnorm(x, mean=mu2, sd=sd2)
  pmin(f1, f2)
}


generate_plot <- function(mu1,mu2,sd1,sd2)
{
  xs <- seq(from = min(mu1 - 3*sd1, mu2 - 3*sd2), to = max(mu1 + 3*sd1, mu2 + 3*sd2), by = .0001)
  f1 <- dnorm(xs, mean=mu1, sd=sd1)
  f2 <- dnorm(xs, mean=mu2, sd=sd2)

  plot(xs, f1, type="l", ylim=c(0, max(f1,f2)), ylab="density", xlab="Raw values")
  lines(xs, f2, lty="dotted")
  legend('topright', inset=c(0.1,0),c("Model","Test"), lty = c(1,3), cex = 0.55)
  
  ys <- min.f1f2(xs, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)
  xs <- c(xs, xs[1])
  ys <- c(ys, ys[1])
  polygon(xs, ys, col="gray")

### only works for sd1 = sd2
  SMD <- (mu1-mu2)/sd1
  2 * pnorm(-abs(SMD)/2)

### this works in general
  integrate(min.f1f2, -Inf, Inf, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)
}
