\name{getTeamEvents}
\alias{getTeamEvents}
\title{
Events that a given team competed at.
}
\description{
List all the events a given team competed at since the team's creation.
}
\usage{
getTeamEvents(team, authKey = defaultAuthKey())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{team}{
A team number or a string of the form "frc<team number>".
}
  \item{authKey}{
Your TheBlueAlliance.com authentication key.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data.frame.
}

\seealso{
\link{code{getDistrictEvents}}.
}
\examples{
getTeamEvents(3238)
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
