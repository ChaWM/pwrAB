# pwrAB

**pwrAB** is a simple package to perform power analysis for AB testing. There are a number of R packages for power calculations, such as the widely used **pwr** package, but to my knowledge none of the t-test power functions allow the variance and sample size to simultaneously vary across the two groups. Instead, they are based on the pooled t-test, and will provide biased results when the variance and sample size are not equal across groups. It is frequently the case in commercial AB testing that a treatment effect will also shift the variance in the treatment group, making the pooled t-test a poor choice for analysis unless the two groups have equal sample sizes (50/50 split).

The two functions in the **pwrAB** package use the Welch's unequal variance t-test instead, and will provide valid results even when sample sizes are unequal. [Delacre et al. (2017)](http://www.rips-irsp.com/articles/10.5334/irsp.82/) expands on this topic in more detail for those that are interested. The function **AB_t2n_prop** can be used when the response variable is binary, while the **AB_t2n** can be used in all other situations. An implicit assumption underlying the package is that the sample sizes are large enough for the central limit theorem to be relevant (and thus making the t-test an appropriate choice regardless of normality assumptions). In cases where the sample sizes are not sufficiently large relative to the departure to normality, using another package/function for sample sizing is strongly encouraged.

The package is [available on CRAN](https://cran.r-project.org/web/packages/pwrAB/index.html), and can be easily installed by typing:
```
install.packages('pwrAB')
```

Much of the code/syntax of this package is borrowed from the [**pwr** package by Champely et al.](https://cran.r-project.org/web/packages/pwr/index.html). Many thanks to the authors for making their code publicly available.
