\name{getDistrictEvents}
\alias{getDistrictEvents}
\title{
FRC events in a given district and year.
}
\description{
Get information on FRC events in a given district and year.
}
\usage{
getDistrictEvents(district, year = defaultYear(), districtKey = paste0(year, district), simple = TRUE, authKey = defaultAuthKey())
}
\arguments{
  \item{district}{
A character string giving the abbreviation for the district.  Use \code{getDistricts()} to get a list of them.
}
  \item{year}{
The year of interest.
}
  \item{districtKey}{
A character string of the form "<year><district>".
}
  \item{simple}{
A logical value.  If \code{TRUE}, then give only the commonly desired information on a district;
if \code{FALSE}, then give more details.
}
  \item{authKey}{
Your authentication key for TheBlueAlliance.com.
}
}
\value{
A data.frame with one entry for each event with components
\item{city }{A character string giving the event's city.}
\item{country }{A character string giving the event's country.}
\item{district }{A data.frame describing the district.}
\item{end_date }{ A 'Date' giving the last day of the event. }
\item{event_code }{A character string giving a short identifier for the event.}
\item{event_type }{An integer giving the event type code:
0=Regional,
1=District,
2=District Championship,
3=Championship Division,
4=Championship Finals,
5=District Championship Division,
99=Offseason,
100=Preseason}.
Etc.
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
\code{getDistricts(year)} will list the district names and codes for a given year.
}
\examples{
e <- getDistrictEvents("pnw", year=2022)$event_code
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
