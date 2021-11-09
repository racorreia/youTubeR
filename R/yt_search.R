#' @title YouTube Search API node
#' @description Access the YouTube Search API node
#' @param query The query parameter specifies the query term to search for.
#' @param type The type parameter restricts a search query to only retrieve a particular type of resource. The value is a comma-separated list of resource types. The default value is "video", but also recognizes "channel" and "playlist".
#' @param location The location parameter, in conjunction with the locationRadius and radiusUnit parameters, defines a circular geographic area and also restricts a search to videos that specify, in their metadata, a geographic location that falls within that area. The parameter value is a string that specifies latitude/longitude coordinates e.g. (37.42307,-122.08427).
#' @param locationRadius The locationRadius parameter, in conjunction with the location parameter, defines a circular geographic area. The API does not support locationRadius parameter values larger than 1000 kilometers.
#' @param radiusUnit Measurement unit for the locationRadius parameter. Valid measurement units are m, km, ft, and mi.
#' @param regionCode The regionCode parameter instructs the API to return search results for videos that can be viewed in the specified country. The parameter value is an ISO 3166-1 alpha-2 country code in string format.
#' @param relevanceLanguage The relevanceLanguage parameter instructs the API to return search results that are most relevant to the specified language. The parameter value is typically an ISO 639-1 two-letter language code. However, you should use the values zh-Hans for simplified Chinese and zh-Hant for traditional Chinese. Please note that results in other languages will still be returned if they are highly relevant to the search query term.
#' @param maxResults The maxResults parameter specifies the maximum number of items that should be returned in the result set. Acceptable values are 0 to 50, inclusive.
#' @param pageToken The pageToken parameter identifies a specific page in the result set that should be returned. In an API response, the nextPageToken and prevPageToken properties identify other pages that could be retrieved.
#' @param publishedAfter The publishedAfter parameter indicates that the API response should only contain resources created at or after the specified time. Values should be provided in POSIXct or Date format.
#' @param publishedBefore The publishedBefore parameter indicates that the API response should only contain resources created before or at the specified time. Values should be provided in POSIXct or Date format.
#' @param api.key The api.key parameter species the key used to access the API.
#' @return Returns an object with the API call results.
#' @examples
#' \dontrun{
#' video_search <- yt_search(query = INSERT_QUERY,
#'                         maxResults = 50,
#'                         publishedAfter = as.Date("2021/10/01"),
#'                         api.key = INSERT_API_KEY)
#' }
#' @seealso
#'  \code{\link[utils]{URLencode}}
#'  \code{\link[lubridate]{is.POSIXt}},\code{\link[lubridate]{is.Date}}
#'  \code{\link[anytime]{iso8601}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[ISOcodes]{ISO_3166_1}},\code{\link[ISOcodes]{ISO_639_2}}
#' @rdname yt_search
#' @export
#' @importFrom utils URLencode
#' @importFrom lubridate is.POSIXct is.Date
#' @importFrom anytime rfc3339
#' @importFrom jsonlite fromJSON
#' @importFrom ISOcodes ISO_3166_1 ISO_639_2
yt_search <- function(query, type = "video", location = NULL, locationRadius = NULL, radiusUnit = NULL, regionCode = NULL, relevanceLanguage = NULL, maxResults = NULL, pageToken = NULL, publishedAfter = NULL, publishedBefore = NULL, api.key){

  # Set base API call
  link <- "https://youtube.googleapis.com/youtube/v3/search?part=snippet"

  # Test if provided query parameter is a string
  if(is.character(query)){
    query <- utils::URLencode(query,reserved=T)
    call <- paste0(link, "&q=", query)
  } else {
    stop("Parameter \'query\' must be a character string")
  }

  # Test if provided query parameter is a string
  if(is.character(type) && type %in% c("video", "channel", "playlist")){
    call <- paste0(call, "&type=", type)
  } else {
    stop("Parameter \'query\' must be either \"video\", \"channel\" or \"playlist\"")
  }

  # Test if location related parameters are provided adequately
  if(!is.null(location) | !is.null(locationRadius) | !is.null(radiusUnit)){
    if(!is.null(location) && !is.null(locationRadius) && !is.null(radiusUnit)){
      if(is.vector(location) && is.numeric(locationRadius) && radiusUnit %in% c("m", "km", "ft", "mi")){
        call <- paste0(call, "&location=", paste0(location, collapse = "%2C"), "&locationRadius=", locationRadius, radiusUnit)
      } else {
        stop("Parameters \'location\', \'locationRadius\' and \'radiusUnit\' were not defined appropriately, see ?yt_search_list for details")
      }
    } else {
      stop("Parameters \'location\', \'locationRadius\' and \'radiusUnit\' must all be defined")
    }
  }

  # Test if publishedAfter value is provided
  if(!is.null(publishedAfter)){
    if(lubridate::is.POSIXct(publishedAfter)){
      start_date<-as.character(anytime::rfc3339(publishedAfter))
      start_date<-gsub("(\\+.*)","Z",start_date)
      start_date <- utils::URLencode(start_date, reserved = T)
      call <- paste0(call, "&publishedAfter=", start_date)
    } else if(lubridate::is.Date(publishedAfter)){
      publishedAfter <- as.POSIXct(publishedAfter, tz="GMT", format="%Y-%m-%d %H:%M:%S")
      start_date<-as.character(anytime::rfc3339(publishedAfter))
      start_date<-gsub("(\\+.*)","Z",start_date)
      start_date <- utils::URLencode(start_date, reserved = T)
      call <- paste0(call, "&publishedAfter=", start_date)
    } else {
      stop("Parameter \'publishedAfter\'  must be type POSIXct or Date")
    }
  }

  # Test if publishedBefore value is provided
  if(!is.null(publishedBefore)){
    if(lubridate::is.POSIXct(publishedBefore)){
      end_date <- as.character(anytime::rfc3339(publishedBefore))
      end_date <- gsub("(\\+.*)","Z",end_date)
      end_date <- utils::URLencode(end_date, reserved = T)
      call <- paste0(call, "&publishedBefore=", end_date)
    } else if(lubridate::is.Date(publishedBefore)){
      publishedBefore <- as.POSIXct(publishedBefore, tz="GMT", format="%Y-%m-%d %H:%M:%S")
      end_date <- as.character(anytime::rfc3339(publishedBefore))
      end_date <- gsub("(\\+.*)","Z",end_date)
      end_date <- utils::URLencode(end_date, reserved = T)
      call <- paste0(call, "&publishedBefore=", end_date)
    } else {
      stop("Parameter \'publishedBefore\'  must be type POSIXct or Date")
    }
  }

  # Test if regionCode value is provided
  if(!is.null(regionCode)){
    if(regionCode %in% ISOcodes::ISO_3166_1$Alpha_2){
      call <- paste0(call, "&regionCode=", regionCode)
    } else {
      stop("Parameter \'regionCode\'  must be an ISO 3166-1 alpha-2 country code")
    }
  }

  # Test if relevanceLanguage value is provided
  if(!is.null(relevanceLanguage)){
    if(relevanceLanguage == "zh"){
      relevanceLanguage <- "zh-Hans"
      warning("relevanceLanguage parameter converted to simplified Chinese (\"zh-Hans\"), for traditional Chinese use (\"zh-Hant\")")
    }

    if(relevanceLanguage %in% c(ISOcodes::ISO_639_2$Alpha_2,"zh-Hans","zh-Hant")){
      call <- paste0(call, "&relevanceLanguage=", relevanceLanguage)
    } else {
      stop("Parameter \'regionCode\'  must be an ISO 639-1 alpha-2 language code. See ?yt_search for more details.")
    }
  }

  # Test if maxResults value is provided
  if(!is.null(maxResults)){
    if(is.numeric(maxResults) && maxResults <= 50 && maxResults >= 1){
      call <- paste0(call, "&maxResults=", maxResults)
    } else {
      stop("Parameter \'maxResults\'  must be a number between 1 and 50")
    }
  }

  # Test if pageToken value is provided
  if(!is.null(pageToken)){
    if(is.character(pageToken)){
      call <- paste0(call, "&pageToken=", pageToken)
    } else {
      stop("Parameter \'pageToken\' must be a character string")
    }
  }

  # Add API key parameter
  if(is.character(api.key)){
    call <- paste0(call, "&key=", api.key)
  } else {
    stop("Parameter \'api.key\' must be a string value")
  }

  get_call <- jsonlite::fromJSON(txt = call)

  return(get_call)
}
