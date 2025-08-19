prepare_for_mimosa <- function(sim_data) {
  library(dplyr)
  sim_data <- dplyr::mutate(sim_data,
                            RefTreat = ifelse(STIMULATION == "Unstimulated",
                                              "Reference", "Treatment"))
  aggregated <- dplyr::group_by(sim_data, SUBJECTID,
                                CYTOKINE, TCELL, STIMULATION,
                                RefTreat) %>%
    dplyr::summarise(
      NSUB = sum(NSUB),
      CYTNUM = sum(CYTNUM),
      .groups = "drop"
    )
  paired <- dplyr::group_by(aggregated, SUBJECTID, CYTOKINE, TCELL) %>%
    dplyr::filter(all(c("Stimulated", "Unstimulated") %in% STIMULATION)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(REFERENCE = STIMULATION == "Unstimulated")
  E <- ConstructMIMOSAExpressionSet(
    paired,
    reference = (STIMULATION == "Unstimulated") ,
    measure.columns = c("NSUB", "CYTNUM"),
    .variables = c("SUBJECTID", "CYTOKINE", "TCELL")
  )
  print("Constructed ExpressionSet")
  return(E)
}
