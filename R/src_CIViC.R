#' @importFrom utils tail

src_CIViC <-
    function(...)
{
    objects <- list(...)
    objects <- lapply(objects, function(object) {
        attr(object, "JSON") <- NULL
        class(object) <- c("tbl_CIViC", "tbl_df", tail(class(object), -1))
        object
    })
    class(objects) <- c("src_CIViC", "src")
    objects
}

#' @importFrom dplyr src_tbls src_desc
#' @export
format.src_CIViC <- function(x, ...)
{
    tbls <- paste0(sort(src_tbls(x)), collapse=", ")
    paste0("src: ", src_desc(x), "\n",
           dplyr:::wrap("tbls: ", tbls))
}

#' @export
src_desc.src_CIViC <- function(x)
    paste0("CIViC query")

#' @export
src_tbls.src_CIViC <- function(x) names(x)

#' @importFrom dplyr tbl
#' @export
tbl.src_CIViC <- function(src, from, ...)
    src[[from]]
