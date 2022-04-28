\name{defaultAuthKey}
\alias{defaultAuthKey}
\title{
Get or set default authentication key for thebluealliance.com.
}
\description{
Store and retrieve the authentication key required for accessing data from thebluealliance.com.
}
\usage{
defaultAuthKey(authKey = NULL)
}
\arguments{
  \item{authKey}{
A character string.  If supplied and not \code{NULL}, it will be stored;
otherwise if there is a stored value the stored value will be returned.
If \code{authKey} is not supplied and none has ever been supplied, an error
is given.
}
}
\details{
A personal authentication key can be gotten by registering at url{https:://thebluealliance.com/account}.

\code{defaultAuthKey()} is used as the default value of the \code{authKey} argument to all the \code{get<Something>}
functions in this package.
}
\value{
A string containing the value of the stored authentication key.
}
\references{
url{https://thebluealliance.com/apidocs}.
}
\note{
In the future \code{defaultAuthKey(NULL)} may look in an environment variable or file for the key.
}

\examples{
\dontrun{
try(defaultAuthKey()) 
try(getTeam("frc360")[c("nickname","school_name","city")])
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
