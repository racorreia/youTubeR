#' @title YouTube Channels API node
#' @description Access the YouTube Channels API node
#' @param part The part parameter specifies a comma-separated list of one or more channel resource properties that the API response will include. Valid parameters can be found here: https://developers.google.com/youtube/v3/docs/channels/list.
#' @param id The id property specifies the channel's YouTube channel ID.
#' @param maxResults The maxResults parameter specifies the maximum number of items that should be returned in the result set. Acceptable values are 0 to 50, inclusive. The default value is 5.
#' @param pageToken The pageToken parameter identifies a specific page in the result set that should be returned. In an API response, the nextPageToken and prevPageToken properties identify other pages that could be retrieved.
#' @param api.key The api.key parameter species the key used to access the API.
#' @return Returns an object with the API call results.
#' @examples
#' \dontrun{
#'  library(youTubeR)
#'
#'  test <- yt_channels(part = c("id", "snippet", "localizations", "statistics", "brandingSettings", "topicDetails"),
#'                           id = "UCJy_cQ9QwYihJ4kIQpvdEAA",
#'                           maxResults = 50,
#'                           api.key = INSERT_API_KEY_HERE)
#'
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname yt_channels
#' @export
#' @importFrom jsonlite fromJSON

yt_channels <- function(part, id, maxResults = NULL, pageToken = NULL, api.key){

  # Set base API call
  link <- "https://youtube.googleapis.com/youtube/v3/channels?"

  # Add part parameter
  part_par <- c("auditDetails", "brandingSettings", "contentDetails", "contentOwnerDetails",
                "statistics", "localizations", "id", "snippet", "status", "topicDetails")

    # Test if provided part values are valid
    if(all(part %in% part_par)){
      call <- paste0(link, "part=", paste(part, collapse = "%2C"))
    } else {
      stop("Parameter \'part\' includes non valid values")
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

    # Test if maxResults value is provided
    if(!is.null(maxResults)){
      if(is.numeric(maxResults) && maxResults <= 50 && maxResults >= 1){
        call <- paste0(call, "&maxResults=", maxResults)
      } else {
        stop("Parameter \'maxResults\' must be a number between 1 and 50")
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
