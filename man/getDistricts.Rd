\name{getDistricts}
\alias{getDistricts}
\title{
List FIRST districts.
}
\description{
Give information on all the FIRST districts.
}
\usage{
getDistricts(year = defaultYear(), authKey = defaultAuthKey())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
The year of interest.
}
  \item{authKey}{
Your TheBlueAlliance.com authentication key.
}
}
\value{
A data.frame
}
\note{
Some regions use the "district" model for events, where teams compete in two
events and the top ranked teams in those events compete at the district championship.
Others use the "regional" model and teams compete only in a regional championship.
The highest ranked teams in the district or regional championships advance to
the (world) championship event.
See \url{https://www.firstinspires.org/resource-library/frc/district-regional-teams} for more information on these models.

The larger championship events may run several "divisional" events concurrently,
with the division winners advancing to the championship finals.
}


\seealso{
\code{getDistrictEvents}.
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
