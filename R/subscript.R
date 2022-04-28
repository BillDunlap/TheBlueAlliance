# The [[ can mess up lapply calls.
`[.frc_zebradata` <- function(x, i, ...) {
  getTeamZebraData(x, team = i)
}
`[[.frc_zebradata` <- function(x, i, ...) {
  outputList <- getTeamZebraData(x, team = i)
  stopifnot(length(outputList)==1)
  outputList[[1]]
}
