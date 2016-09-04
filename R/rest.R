#' @importFrom httr GET stop_for_status
.get <-
    function(path, params=character(), content=FALSE,
             base_url=.cache$get("base_url"))
{
    url <- sprintf("%s/%s", base_url, path)
    if (length(params)) {
        params <- paste0(names(params), "=", as.character(params), collapse="&")
        url <- sprintf("%s?%s", url, params)
    }
    response <- GET(url)
    stop_for_status(response)
    response
}        
