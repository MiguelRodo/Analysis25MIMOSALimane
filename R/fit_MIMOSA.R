fit_MIMOSA <- function(data, mtd = "mcmc") {
  sink("output.txt", append = TRUE)
  fit <- MIMOSA(
    NSUB + CYTNUM ~ SUBJECTID | CYTOKINE,
    data = data,
    subset = RefTreat == "Treatment" & CYTOKINE == "IL2",
    ref = RefTreat == "Reference" & CYTOKINE == "IL2",
    method = mtd,
    RT = FALSE
  )
  sink(NULL)
  fit
}