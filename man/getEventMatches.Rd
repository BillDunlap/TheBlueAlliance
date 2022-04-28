\name{getEventMatches}
\alias{getEventMatches}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List matches in an event.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getEventMatches(event, simple = TRUE, authKey = defaultAuthKey())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{event}{
A character string giving the event key.  See \code{getDistrictEvents} or \code{getEvents}.
}
  \item{simple}{
A logical value.  If \code{TRUE} return the commonly desired information on an event;
if \code{FALSE}, give more information.
}
  \item{authKey}{
Your TheBlueAlliance.com authentication key.
}
}
\value{
A data.frame.
}

\seealso{
}
\examples{
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
