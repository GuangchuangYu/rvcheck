##' load function from package
##'
##'
##' @title get_fun_from_pkg
##' @param pkg package
##' @param fun function
##' @return function
##' @export
##' @author guangchuang yu
get_fun_from_pkg <- function(pkg, fun) {
    ## requireNamespace(pkg)
    ## eval(parse(text=paste0(pkg, "::", fun)))
    require(pkg, character.only = TRUE)
    eval(parse(text = fun))
}

is.rserver <- function(){
    RStudio.Version = tryCatch(get("RStudio.Version"), error = function(e) NULL)
    if(is.null(RStudio.Version)) return(FALSE)
    if(!is.function(RStudio.Version)) return(FALSE)
    RStudio.Version()$mode == 'server'
}

`%||%` <- function(a, b) ifelse(is.null(a), b, a)

##' open working directory
##'
##'
##' @title o
##' @param file to be open; open workding directory by default
##' @return NULL
##' @author Guangchuang Yu
##' @export
o <- function(file=".") {
    os <- Sys.info()[1]
    if (is.rserver()) {
        if (dir.exists(file)) {
            stop("open directory in RStudio Server is not supported.")
        }
        rserver_ip <- getOption("rserver_ip")
        if (!is.null(rserver_ip)) {
            rserver_port <- getOption("rserver_port") %||% '8787' 
            if (!startsWith(rserver_ip, "http")) {
                rserver_ip = paste0("http://", rserver_ip)
            }
            utils::browseURL(
                       paste0(
                           paste(rserver_ip, rserver_port, sep=":"),
                           "/file_show?path=",
                           file
                       ))
        } else {
            file.edit <- get("file.edit")
            file.edit(file)   
        }
    } else if (os == "Darwin") {
        cmd <- paste("open", file)
        system(cmd)
    } else if (os == "Linux") {
        cmd <- paste("xdg-open", file, "&")
        system(cmd)
    } else if (os == "Windows") {
        ## wd <- sub("/", "\\", getwd())
        ## cmd <- paste("explorer", wd)
        ## suppressWarnings(shell(cmd))
        cmd <- paste("start", file)
        shell(cmd)
    }
}

##' read clipboard
##'
##'
##' @title read.cb
##' @param ... parameters for read.table
##' @return data.frame
##' @author Guangchuang Yu
##' @importFrom utils read.table
##' @export
read.cb <- function(...) {
    os <- Sys.info()[1]
    if (os == "Darwin") {
        read.table(pipe("pbpaste"), ...)
    } else {
        read.table("clipboard", ...)
    }
}




##' extract aes mapping, compatible with ggplot2 < 2.3.0 & > 2.3.0
##'
##'
##' @title get_aes_var
##' @param mapping aes mapping
##' @param var variable
##' @return mapped var
##' @importFrom utils tail
##' @importFrom rlang quo_text
##' @export
##' @author guangchuang yu
get_aes_var <- function(mapping, var) {
    res <- rlang::quo_text(mapping[[var]])
    ## to compatible with ggplot2 v=2.2.2
    tail(res, 1)
}

