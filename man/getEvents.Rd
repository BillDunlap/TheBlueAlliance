\name{getEvents}
\alias{getEvents}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List FIRST events.
}
\description{
Get information on all FIRST events (competitions) in a given year.
}
\usage{
getEvents(year = defaultYear(), simple = TRUE, authKey = defaultAuthKey())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
The year of interest.
}
  \item{simple}{
Do you want all the gory details.
}
  \item{authKey}{
Your TheBlueAlliance.com authentication key.
}
}
\value{
A data.frame.
}

\seealso{
\link{\code{getEventMatches}}.
}
\examples{
e <- getEvents(2022)
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
