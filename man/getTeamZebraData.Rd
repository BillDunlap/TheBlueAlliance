\name{getTeamZebraData}
\alias{[.frc_zebradata}
\alias{[[.frc_zebradata}
\alias{getTeamZebraData}
\title{
Robot location time series.
}
\description{
Get time series of the the locations of one robot in a match.
}
\usage{
getTeamZebraData(frc_zebradata, team)
\method{[}{frc_zebradata}(x, i, ...)
\method{[[}{frc_zebradata}(x, i, ...)
}
\arguments{
  \item{x}{
The return value of a call to \code{getZebraData}, which contains
the location timeseries for all six robots in a match.
}
  \item{i}{
A team number, given as a number or a character string,  or a character
string of the form "frc<team number>".  It may also be an integer in the
range 1-6, which will be taken as equivalent to names(x)[i].  (For teams
1-6 you must a supply a string).
}
}
\value{
A data.frame giving the location of the robot every 100 milliseconds.
Its columns are
\item{x }{The x component of the location}
\item{y }{The y component of the location}
\item{time }{The times at which the locations were measured}
The data.frame also has attributes
\item{alliance}{"red" or "blue"}
\item{team}{the team number}
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
z_match <- getZebraData("2022pncmp_qm10")
names(z_match)
str(z_match[2])
str(z_match["7641"])
str(z_match[[2]])
str(z_match[["frc7461"]])
str(z_match[[7461]])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
