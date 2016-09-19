##' check latest github version of R package
##'
##'
##' @title check_github
##' @param pkg package name
##' @return list
##' @export
##' @examples
##' \dontrun{
##' library(rvcheck)
##' check_github('guangchuangyu/ggtree')
##' }
##' @author Guangchuang Yu
check_github <- function(pkg) {
    installed_version <- tryCatch(packageVersion(gsub(".*/", "", pkg)), error=function(e) NA)

    url <- paste0("https://raw.githubusercontent.com/", pkg, "/master/DESCRIPTION")
    x <- readLines(url)
    remote_version <- gsub("Version:\\s*", "", x[grep('Version:', x)])

    res <- list(package = pkg,
                installed_version = installed_version,
                latest_version = remote_version,
                up_to_date = NA)

    if (is.na(installed_version)) {
        message(paste("##", pkg, "is not installed..."))
        message(msg)
    } else {
        if (remote_version > installed_version) {
            msg <- paste("##", pkg, "is out of date...")
            message(msg)
            res$up_to_date <- FALSE
        } else if (remote_version == installed_version) {
            message("package is up-to-date devel version")
            res$up_to_date <- TRUE
        }
    }

    return(res)
}

##' check latest release version of bioconductor package
##'
##'
##' @title check_bioc
##' @param pkg package name
##' @return list
##' @export
##' @examples
##' \dontrun{
##' library(rvcheck)
##' check_bioc('ggtree')
##' }
##' @author Guangchuang Yu
check_bioc <- function(pkg="BiocInstaller") {
    msg <- paste("## try http:// if https:// URLs are not supported",
                 'source("https://www.bioconductor.org/biocLite.R")',
                 paste0('biocLite("', pkg, '")'), sep="\n")

    check_release("https://bioconductor.org/packages/", pkg, msg)
}

##' check latest release version of cran package
##'
##'
##' @title check_cran
##' @param pkg package name
##' @return list
##' @export
##' @examples
##' \dontrun{
##' library(rvcheck)
##' check_cran('emojifont')
##' }
##' @author Guangchuang Yu
check_cran <- function(pkg) {
    msg <- paste("## try",
                 paste0('install.packages("', pkg, '")'),
                 sep="\n")
    check_release("https://cran.r-project.org/web/packages/", pkg, msg)
}

##' @importFrom utils packageVersion
check_release <- function(base_url, pkg, msg) {
    installed_version <- tryCatch(packageVersion(pkg), error=function(e) NA)

    url <- paste0(base_url, pkg)
    x <- readLines(url)
    remote_version <- gsub("\\D+([\\.0-9]+)\\D+", '\\1', x[grep("Version", x)+1])

    res <- list(package = pkg,
                installed_version = as.character(installed_version),
                latest_version = remote_version,
                up_to_date = NA)

    if (is.na(installed_version)) {
        message(paste("##", pkg, "is not installed..."))
        message(msg)
    } else {
        if (remote_version > installed_version) {
            message(paste("##", pkg, "is out of date..."))
            message(msg)
            res$up_to_date <- FALSE
        } else if (remote_version == installed_version) {
            message("package is up-to-date release version")
            res$up_to_date <- TRUE
        } else {
            message("devel branch is used, this function only works for release version...")
        }
    }

    return(res)
}

