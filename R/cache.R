.cache <- local({
    env <- new.env(parent=emptyenv())
    list(get = function(key) {
        env[[key]]
    }, set = function(key, value) {
        env[[key]] <- value
        invisible(env[[key]])
    })
})
