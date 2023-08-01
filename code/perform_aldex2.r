# ALDEx2 differential abundance analysis

perform_aldex <- function(aldex.in, conds = conds, paired = FALSE, denom = "all"){
  # Get the clr values
  # This is the main ALDEx function for all downstream analyses
  # mc.samples=128 is often sufficient
  x <- aldex.clr(aldex.in, conds = conds, mc.samples = 128, verbose = TRUE, denom = denom)

  #estimate effect size and the within and between condition values
  #include indiv. samples or not
  x.effect <- aldex.effect(x, conds, include.sample.summary = TRUE, verbose = TRUE, useMC = FALSE)

  #perform t-test (both Welches and Wilcoxon, plus a Benjamini-Hochberg multiple test correction)
  x.tt <- aldex.ttest(x, conds, paired.test=paired) # Samples are paired (check for correct order)

  #merge the data
  x.all <- data.frame(x.tt, x.effect)
  return(x.all)
}
