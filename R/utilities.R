## check

is_single_number <- function(x)
    is.numeric(x) && (length(x) == 1L) && !is.na(x)

is_single_character <- function(x)
    is.character(x) && (length(x) == 1L) && !is.na(x)
