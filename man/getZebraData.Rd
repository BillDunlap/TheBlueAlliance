\name{getZebraData}
\alias{getZebraData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robot location time series for a match
}
\description{
Get the robot location time series for the six robots in a match.
}
\usage{
getZebraData(match, authKey = defaultAuthKey())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{match}{
A character string identifying the match, as in the return value of
\code{getEventMatches(...)$key}.  It encodes the year, event location,
type of match, and match number.
}
  \item{authKey}{
Your TheBlueAlliance.com authentication key.
}
}
\value{
A list of six data.frames, named with the team numbers.  Each
data.frame contains the columns
\item{x }{The x component of the locations, in feet}
\item{y }{The y component of the locations, in feet}
\item{time }{The times at which the locations were recorded, in seconds from the start of the match.}
As of this writing, the locations are recorded every 100 milliseconds.

Each data.frame also has the attributes
\item{alliance}{"red" or "blue"}
\item{team}{the team number}.
}
\examples{
m <- getEventMatches("2022wasam")
z6 <- getZebraData(m$key[[77]])
str(z6[[2]])
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
