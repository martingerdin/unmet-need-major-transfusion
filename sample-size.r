## sample size calculation for Ruut's unmet MT need study
## much of the code below is from http://powerandsamplesize.com/Calculators/Compare-Paired-Proportions/McNemar-Z-test-2-Sided-Equality, which in turn cite Connor R. J. 1987. Sample size for testing differences in proportions for the paired-sample design. Biometrics 43(1):207-211.

mcnemar.sample.size <- function(p0 = 0.5,
                                p1 = 0.1,
                                alpha = 0.05,
                                beta = 0.2,
                                twosided = TRUE)
{
    if (twosided) alpha <- alpha * 2
    pdisc <- p1 + p0
    pdiff <- p1 - p0
    n <- ((qnorm(1-alpha/2)*sqrt(pdisc)+qnorm(1-beta)*sqrt(pdisc-pdiff^2))/pdiff)^2
    x1 <- ( pdiff*sqrt(n)-qnorm(1-alpha/2)*sqrt(pdisc))/sqrt(pdisc-pdiff^2)
    x2 <- (-pdiff*sqrt(n)-qnorm(1-alpha/2)*sqrt(pdisc))/sqrt(pdisc-pdiff^2)
    power <- pnorm(x1)+pnorm(x2)
    out <- list(n = ceiling(n),
                p0 = p0,
                p1 = p1,
                alpha = alpha,
                beta = beta,
                power = power)
    return(out)
}
## if we assume that 10% will be predicted to need MT but only 5% received it
mcnemar_stats <- mcnemar.sample.size(p0 = 0.1, p1 = 0.05)
## the sample size needed to detect this difference would be
mcnemar_stats$n # 369 patients
