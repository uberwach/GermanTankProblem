N = 100
ks = c(2:5,10,20)
sample_num = 1000

old.par <- par(mfrow=c(2, round(length(ks) / 2)))

for (k in ks) {
  estimates = c()
  residuals = c()
  
  for (i in 1:sample_num) {
    samples = sample(1:N, k)
    estimate = max(samples) + max(samples) / k  - 1
    estimates[i] = estimate
    residuals[i] = estimates[i] - N
  }
  
  hist(residuals, xlim=c(-100,50), breaks=c(10*(-10:5)), col="blue", main=sprintf("Residual Histogram (k = %d)", k))
  abline(v = 0, col="orange")
  abline(v = k-100, col="red")
}
par(old.par)
