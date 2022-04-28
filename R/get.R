# this is Bill's key.  Get a new one by logging into thebluealliance.com
billsAuthKey <- "fvHE0hyNFYzTlgkk6OJFfVaB2mWa2MV9mwzNfh9igq7lMsNDEdjKpoyM4CaKdaPz"
defaultAuthKey <- local(function(authKey = NULL) {
  if (!is.null(authKey)) {
    .authKey <- authKey
  }
  .authKey
}, envir = list2env(list(.authKey = billsAuthKey)))

defaultYear <- local(function(year = NULL) {
  if (!is.null(year)) {
    .year <- year
  } else {
    .year =  as.POSIXlt(Sys.time())$year + 1900
  }
  .year
}, envir = list2env(list(.year =  as.POSIXlt(Sys.time())$year + 1900)))

TBA <- "https://www.thebluealliance.com/api/v3/"

getFromTheBlueAlliance <- function(apiPath, authKey=defaultAuthKey()) {
  stopifnot(length(apiPath) == 1)
  httr::GET(paste0(TBA, apiPath),
                   httr::add_headers(accept = "application/json" ,
                                    `X-TBA-Auth-Key`= authKey))
}

check_response <- function(response) {
  if (response$status_code %/% 100 >= 4) {
    stop("HTML error ", response$status_code, " from url ", response$url)
  } else if (response$status_code != 200) {
    message("status_code=", response$status_code, " from url ", response$url)
  }
}
.teamToTeamCode <- function(team) {
  if (is.numeric(team) || (is.character(team) && grepl("^[0-9]+$", team))) {
    paste0("frc", team)
  } else {
    team
  }
}
.teamCodeToTeam <- function(team_code) {
  sub("^frc", "", team_code)
}
.fixupTimes <- function(data) {
  stopifnot(is.data.frame(data))
  origin <- as.POSIXct("1970-01-01")
  for(nm in grep(value=TRUE, "time", names(data))) {
    data[[nm]] <- as.POSIXct(data[[nm]], origin=origin)
  }
  data
}

# A typical match key is "2022wasno_qm26", for qualifying match 26 at
# the 2022 Snohomish, WA event (at Glacier Peak High School).
# Use getEventMatches(event_key) to list them.
.getZebraData <- function(match, authKey=defaultAuthKey()) {
  getFromTheBlueAlliance(paste(sep="/", "match", match, "zebra_motionworks"))
}
getZebraData <- function(match, authKey=defaultAuthKey()) {
  response <- .getZebraData(match=match, authKey=authKey)
  check_response(response)
  content <- httr::content(response, type="text", encoding="UTF-8")
  if (identical(content, "null\n")) {
    stop("No zebra_motionworks data for ", match)
  }
  rawData <- jsonlite::fromJSON(content, simplifyDataFrame=FALSE)
  stopifnot(is.numeric(rawData$times), is.list(rawData$alliances), is.list(rawData$alliances$red), is.list(rawData$alliances$blue))
  times <- rawData$times
  data <- lapply(c("blue","red"),
                 function(allianceName) {
                   tmp <- lapply(rawData$alliances[[allianceName]], function(team) {
                                                       structure(data.frame(x=team$xs, y=team$ys, time=times),
                                                                 alliance=allianceName,
                                                                 team=.teamCodeToTeam(team$team_key))
                                                     })
                   names(tmp) <-  vapply(tmp, attr, "team", FUN.VALUE=NA_character_)
                   tmp
                 })
  data <- unlist(unname(data), recursive=FALSE)
  structure(data, class="frc_zebradata")
}

#alliances <- function(frc_zebradata) {
#  lapply(unclass(frc_zebradata), names)
#}

getTeamZebraData <- function(frc_zebradata, team) {
  stopifnot(is.numeric(team) || is.character(team), length(team)==1)
  if (is.character(team)) {
    team <- sub("^frc", "", team)
    if (anyNA(as.integer(team))) {
      stop("'team' should be team number or \"frc<teamNumber>\"")
    }
    index <- match(team, names(frc_zebradata))
  }
  if (is.numeric(team)) {
    index <- ifelse(team > 6,
      match(team, as.numeric(names(frc_zebradata))),
      team)
  }
  if (anyNA(index)) {
     stop("team ", paste(collapse=", ", team[is.na(index)]), " is not in frc_zebradata")
  }
  unclass(frc_zebradata)[index]
}

.getEvents <- function(year=defaultYear(), simple=TRUE, authKey=defaultAuthKey()) {
  apiPath <- paste(sep="/", "events", year)
  if (simple) {
    apiPath <- paste(sep="/", apiPath, "simple")
  }
  getFromTheBlueAlliance(apiPath)
}
getEvents <- function(year=defaultYear(), simple=TRUE, authKey=defaultAuthKey()) {
  response <- .getEvents(year=year, simple=simple, authKey=authKey)
  check_response(response)
  text <- httr::content(response, type="text", encoding="UTF-8")
  data <- jsonlite::fromJSON(text)
  .fixupEvents(data)
}

.getTeam <- function(team, authKey=defaultAuthKey()) {
  getFromTheBlueAlliance(paste(sep="/", "team", .teamToTeamCode(team)))
}
getTeam <- function(team, authKey=defaultAuthKey()) {
  response <- .getTeam(team=team, authKey=authKey)
  check_response(response)
  jsonlite::fromJSON(httr::content(response, type="text", encoding="UTF-8"))
}
.fixupEvents <- function(data) {
  data$start_date <- as.Date(data$start_date) # or as.POSIXct()?
  data$end_date <- as.Date(data$end_date)     # ditto
  data <- data[order(data$start_date),]
  structure(data, class=c("events", class(data)))
}
.getTeamEvents <- function(team, authKey=defaultAuthKey()) {
  getFromTheBlueAlliance(paste(sep="/", "team", .teamToTeamCode(team), "events"))
}
getTeamEvents <- function(team, authKey=defaultAuthKey()) {
  response <- .getTeamEvents(team=team, authKey=authKey)
  check_response(response)
  data <- jsonlite::fromJSON(httr::content(response, type="text", encoding="UTF-8"))
  .fixupEvents(data)
}

.fixupMatches <- function(data) {
  stopifnot(is.data.frame(data))
  data <- .fixupTimes(data)
  data$blue <- data$alliances$blue[c("score","team_keys")]
  data$red <- data$alliances$red[c("score","team_keys")]
  data$alliances <- NULL
  data <- data[order(data$actual_time),]
  rownames(data) <- data$key
  structure(data, class=c("matches", class(data)))
}
# event here is TBA's event_key: 4-digit year followed by location code, e.g.,
# 2022orore for 2022 Oregon City, OR event or 2022wasno for Glacier Peak HS (in Snohomish, WA) event.
.getEventMatches <- function(event, simple=TRUE, authKey=defaultAuthKey()) {
  request <- paste(sep="/", "event", event, "/matches")
  if (simple) {
    request <- paste(sep="/", request, "simple")
  }
  getFromTheBlueAlliance(request)
}
getEventMatches <- function(event, simple=TRUE, authKey=defaultAuthKey()) {
  response <- .getEventMatches(event=event, simple=simple, authKey=authKey)
  check_response(response)
  data <- jsonlite::fromJSON(content(response, type="text", encoding="UTF-8"))
  .fixupMatches(data)
}
.getTeamMatches <- function(team, event=NULL, year=NULL, simple=TRUE, authKey=defaultAuthKey()) {
  # /team/{team_key}/event/{event_key}/matches
  # /team/{team_key}/matches/{year}
  if (is.null(event) + is.null(year) != 1) {
    stop("must supply either 'event' or 'year'")
  }
  if (!is.null(event)) {
    request <- paste(sep="/", "team", .teamToTeamCode(team), "event", event, "matches")
  } else if (!is.null(year)) {
    request <- paste(sep="/", "team", .teamToTeamCode(team), "matches", year)
  }
  if (simple) {
    request <- paste(sep="/", request, "simple")
  }
  response <- getFromTheBlueAlliance(request)
}
getTeamMatches <- function(team, event=NULL, year=NULL, simple=TRUE, authKey=defaultAuthKey()) {
  response <- .getTeamMatches(team=team, event=event, year=year, simple=simple, authKey=authLey)
  check_response(response)
  data <- jsonlite::fromJSON(content(response, type="text", encoding="UTF-8"))
  .fixupMatches(data)
}

.getDistricts <- function(year, authKey=defaultAuthKey()) {
  reponse <- getFromTheBlueAlliance(paste(sep="/", "districts", year))
}
getDistricts <- function(year=defaultYear(), authKey=defaultAuthKey()) {
  response <- .getDistricts(year, authKey=authKey)
  check_response(response)
  txt <- httr::content(response, type="text", encoding="UTF-8")
  jsonlite::fromJSON(txt)
}

.getDistrictEvents <- function(districtKey, simple = TRUE, authKey=defaultAuthKey()) {
  request <- paste(sep="/", "district", districtKey, "events")
  if (simple) {
    request <- paste(sep="/", request, "simple")
  }
  getFromTheBlueAlliance(request)
}
getDistrictEvents <- function(district, year=defaultYear(), districtKey = paste0(year, district), simple = TRUE, authKey=defaultAuthKey()) {
  response <- .getDistrictEvents(districtKey, simple, authKey=defaultAuthKey())
  check_response(response)
  txt <- httr::content(response, type="text", encoding="UTF-8")
  data <- jsonlite::fromJSON(txt)
  .fixupEvents(data)
}

.getDistrictRankings <- function(districtKey, authKey=defaultAuthKey()) {
  request <- paste(sep="/", "district", districtKey, "rankings")
  getFromTheBlueAlliance(request)
}
getDistrictRankings <- function(district, year=defaultYear(), districtKey=paste0(year, district), authKey=defaultAuthKey()) {
  response <- .getDistrictRankings(districtKey = districtKey, authKey=authKey)
  check_response(response)
  data <- jsonlite::fromJSON(httr::content(response,type="text",encoding="UTF-8"))
  data <- data[order(data$rank),]
  i_event_points <- match("event_points", names(data))
  no_data <- data.frame(alliance_points = NA_integer_,
                        award_points = NA_integer_,
                        district_cmp = NA,
                        elim_points = NA_integer_,
                        event_key = NA_character_,
                        qual_points = NA_integer_,
                        total = NA_integer_)
  data <- cbind(data[-i_event_points],
                num_events = vapply(data$event_points, nrow, NA_integer_),
                event1 = do.call(rbind, lapply(data$event_points, function(x)if (nrow(x)>0) x[1,] else no_data)),
                event2 = do.call(rbind, lapply(data$event_points, function(x)if (nrow(x)>0) x[2,] else no_data)))
  data
}
