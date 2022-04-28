\name{getFromTheBlueAlliance}
\alias{getFromTheBlueAlliance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get information from TheBlueAlliance.com
}
\description{
A low-level interface to the version-3 API from TheBlueAlliance.com
}
\usage{
getFromTheBlueAlliance(apiPath, authKey = defaultAuthKey())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{apiPath}{
An API command as described in \url{https://www.thebluealliance.com/apidocs/v3}.
}
  \item{authKey}{
Your TheBlueAlliance.com authentication key.
}
}
\value{
A JSON object.
}
\references{
\url{https://www.thebluealliance.com/apidocs/v3}
}
\examples{
j <- getFromTheBlueAlliance(paste(sep="/", "events", 2022, "simple"))
str(httr::content(j))
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
