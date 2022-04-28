\name{defaultYear}
\alias{defaultYear}
\title{
Get and set the default year
}
\description{
Get and set the default year when retrieving event data
}
\usage{
defaultYear(year = NULL)
}
\arguments{
  \item{year}{
An integer or a 4-digit character string specifying the year of the FRC events you are looking for.
If NULL or not supplied, the current calendar year will be made the default year.
}
}
\details{
}
\value{
A 4-digit character string giving the year.
}
\note{
The \code{get...Event...} functions use this, as the event key is a combination of the year and event location code.
}

\examples{
\dontrun{
defaultYear(2003)
getEvents()$city
defaultYear() # reset to current year
}
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
