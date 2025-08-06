---
title: "Fisher's exact test"
format: pdf
editor: visual
---

```{r}
ROC_results <- function(fit, n_samples) {

  # Single iteration version (i = 1)
  stat <- fdr(fit[[1]])
  truth <- gl(2, n_samples / 2) %in% 2
  th <- seq(0, 1, l = 1000)
  tpr <- sapply(th, function(x) {
    sum((stat <= x)[truth])
  })

  tpr <- tpr / sum(truth)
  fpr <- sapply(th, function(x) {
    sum((stat <= x)[!truth])
  })
  fpr <- fpr / sum(!truth)
  data.frame(TPR = tpr, FPR = fpr, Ntot = names(fit)[1])
}

#We can compare MIMOSA to Fishers exact test
ROC_fisher <- function(fit, n_samples) {
  p <- vector("numeric", n_samples)
  for (i in 1:100){
    # Single iteration version (i = 1)
    mat <- matrix(countsTable(fit[[1]])[i, ], nrow = 2, byrow = FALSE)
    p[i] <- fisher.test(mat, alternative = "less")$p
  }
  p <- p.adjust(p, "fdr")
  truth <- gl(2, n_samples / 2) %in% 2
  th <- seq(0, 1, l = 1000)
  tpr <- sapply(th, function(x) {
    sum((p <= x)[truth])
  }) / sum(truth)
  fpr <- sapply(th, function(x) {
    sum((p <= x)[!truth])
  }) / sum(!truth)
  data.frame(TPR = tpr, FPR = fpr, Ntot = names(fit)[1])
}
```

