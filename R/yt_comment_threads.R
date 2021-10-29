#' @title YouTube Comment Threads API node
#' @description Access the YouTube Comment Threads API node
#' @param videoId The videoId parameter instructs the API to return comment threads associated with the specified video ID
#' @param order The order parameter specifies the order in which the API response should list comment threads. Valid values are "time" and "relevance".
#' @param searchTerms The searchTerms parameter instructs the API to limit the API response to only contain comments that contain the specified search terms.
#' @param maxResults The maxResults parameter specifies the maximum number of items that should be returned in the result set. Acceptable values are 1 to 100 inclusive.
#' @param pageToken The pageToken parameter identifies a specific page in the result set that should be returned. In an API response, the nextPageToken property identifies the next page of the result that can be retrieved.
#' @param textFormat Set this parameter's value to html or plainText to instruct the API to return the comments left by users in html formatted or in plain text.
#' @param api.key The api.key parameter species the key used to access the API.
#' @return Returns an object with the API call results.
#' @examples
#' \dontrun{
#' comments <- yt_comment_threads(videoId = INSETR_VIDEO_ID,
#'                                maxResults = 100,
#'                                order = "time",
#'                                api.key = INSERT_API_KEY)
#' }
#' @seealso
#'  \code{\link[utils]{URLencode}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname yt_comment_threads
#' @export
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
yt_comment_threads <- function(videoId, order = NULL, searchTerms = NULL, maxResults = NULL, pageToken = NULL, textFormat = NULL, api.key){

  # Set base API call
  link <- "https://youtube.googleapis.com/youtube/v3/commentThreads?"

  # Add part values
  link <- paste0(link, "part=", paste0(c("snippet", "replies"), collapse = "%2C"))

  # Test if provided query parameter is a string
  if(is.character(videoId)){
    call <- paste0(link, "&videoId=", videoId)
  } else {
    stop("Parameter \'videoId\' must be a character string identifying the video to gather comments from")
  }

  # Test if maxResults value is provided
  if(!is.null(maxResults)){
    if(is.numeric(maxResults) && maxResults <= 100 && maxResults >= 1){
      call <- paste0(call, "&maxResults=", maxResults)
    } else {
      stop("Parameter \'maxResults\'  must be a number between 1 and 100")
    }
  }

  # Test if order value is provided
  if(!is.null(order)){
    if(order %in% c("time", "relevance")){
      call <- paste0(call, "&order=", order)
    } else {
      stop("Parameter \'order\'  must be either \"time\" or \"relevance\"")
    }
  }

  # Test if order value is provided
  if(!is.null(searchTerms)){
    if(is.character(searchTerms)){
      call <- paste0(call, "&searchTerms=", utils::URLencode(paste0(searchTerms, collapse = "%2C"), reserved = T))
    } else {
      stop("Parameter \'searchTerms\'  must be a string")
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

  # Test if pageToken value is provided
  if(!is.null(textFormat)){
    if(textFormat %in% c("html", "plainText")){
      call <- paste0(call, "&textFormat=", textFormat)
    } else {
      stop("Parameter \'textFormat\' must be either \"html\" or \"plainText\"")
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
