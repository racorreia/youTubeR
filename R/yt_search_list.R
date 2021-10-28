yt_search_list <- function(query, type, location = NULL, locationRadius = NULL, radiusUnit = NULL, maxResults = NULL, pageToken = NULL){

  # Set base API call
  link <- "https://youtube.googleapis.com/youtube/v3/search?part=snippet"

  # Test if provided query parameter is a string
  if(is.character(query)){
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

  # Test if provided channelID and id parameters are provided
  if(is.null(id)){
    stop("Parameter \'id\' must be provided")
  } else {
    # Test if the id parameter contains a valid value type
    if(is.character(id)){
      call <- paste0(call, "&id=", id)
    } else {
      stop("Parameter \'id\' must be a string")
    }
  }

  # Test if maxResults valus is provided
  if(!is.null(maxResults)){
    if(is.numeric(maxResults) && maxResults <= 50 && maxResults >= 1){
      call <- paste0(call, "&maxResults=", maxResults)
    } else {
      stop("Parameter \'maxResults\'  must be a number between 1 and 50")
    }
  }

  # Test if pageToken value is provided
  if(!is.null(pageToken)){
    if(is.character(maxResults)){
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

  return(get_call$items)
}
