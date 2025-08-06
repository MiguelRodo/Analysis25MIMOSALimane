fit_MIMOSA <- function(data) {
  sink("output.txt", append = TRUE)
  fit <- MIMOSA(
    NSUB + CYTNUM ~ SUBJECTID | CYTOKINE,
    data = E,
    subset = RefTreat == "Treatment" & CYTOKINE == "IL2",
    ref = RefTreat == "Reference" & CYTOKINE == "IL2",
    RT = FALSE
  )
  sink(NULL)
  fit
}