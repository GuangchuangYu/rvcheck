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

##' open working directory
##'
##'
##' @title o
##' @return NULL
##' @author Guangchuang Yu
##' @export
o <- function() {
    os <- Sys.info()[1]
    if (os == "Darwin") {
	system("open .")
    } else if (os == "Linux") {
        system("xdg-open . &")
    } else if (os == "Windows") {
	wd <- sub("/", "\\", getwd())
	cmd <- paste("explorer", wd)
	suppressWarnings(shell(cmd))
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

