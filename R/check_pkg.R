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
    check_github_gitlab(pkg, "github")
}

##' check latest gitlab version of R package
##'
##'
##' @title check_gitlab
##' @param pkg package name
##' @return list
##' @export
##' @author Guangchuang Yu
check_gitlab <- function(pkg) {
    check_github_gitlab(pkg, "gitlab")
}


check_github_gitlab <- function(pkg, repo="github") {
    installed_version <- tryCatch(packageVersion(gsub(".*/", "", pkg)), error=function(e) NA)

    if(repo == "github") {
        url <- paste0("https://raw.githubusercontent.com/", pkg, "/master/DESCRIPTION")
    } else if (repo == "gitlab") {
        url <- paste0("https://gitlab.com/", pkg, "/raw/master/DESCRIPTION")
    } else {
        stop("only work with github and gitlab")
    }
    
    x <- readLines(url)
    remote_version <- gsub("Version:\\s*", "", x[grep('Version:', x)])

    res <- list(package = pkg,
                installed_version = installed_version,
                latest_version = remote_version,
                up_to_date = NA)

    if (is.na(installed_version)) {
        message(paste("##", pkg, "is not installed..."))
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
check_bioc <- function(pkg="BiocManager") {
    msg <- paste('install.packages("BiocManager")',
                 paste0('BiocManager::install("', pkg, '")'), sep="\n")
    if (pkg == "BiocManager"){
        check_cran(pkg)
    } else{
        check_release("BioCsoft", pkg, msg)
    }
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
    check_release("CRAN", pkg, msg)
}

##' @importFrom utils packageVersion
##' @importFrom BiocManager repositories
check_release <- function(repo, pkg, msg) {
    installed_version <- tryCatch(packageVersion(pkg), error=function(e) NA)
    repos <- BiocManager::repositories()
    base_url <- repos[repo]
    n <- nchar(base_url)
    suffix <- substring(base_url, n, n)

    if (suffix != "/") {
        base_url <- paste0(base_url, '/')
    }
    if (repo == "CRAN") {
        url <- paste0(base_url, 'web/packages/', pkg)
    } else if (repo == "BioCsoft") {
        url <- paste0(base_url, 'html/', pkg, '.html')
    }

    x <- readLines(url)
    remote_version <- gsub("\\D+([\\.0-9-]+)\\D+", '\\1', x[grep("<td>Version:{0,1}</td>", x)+1])

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

