pwrAB is a simple package to perform power analysis for AB testing. There are a number of R packages that may be used for power calculations, such as the widely used pwr package, but to my knowledge none of them allow the variance and sample size to simulataneously vary across the two groups. Instead, they are based on the pooled t-test, and will provide biased results when the variance and sample size are not equal across groups.

The two functions in the pwrAB package use the Welch unequal variance t-test instead, and will provide valid results even when sample sizes are unequal. Delacre et al. (2017) expands on this topic in more detail for those that are interested (http://www.rips-irsp.com/articles/10.5334/irsp.82/)

