##' check latest R version
##'
##' 
##' @title check_r
##' @return list
##' @export
##' @examples
##' \dontrun{
##' library(rvcheck)
##' check_r()
##' }
##' @author Guangchuang Yu
check_r <- function() {
    base_url <- "https://cran.r-project.org/src/base/"
    x <- readLines(base_url)
    r <- gsub(".*(R-\\d).*", '\\1', x[grep("R-\\d", x)])

    ## r_major <- max(r)
    ## in case they bump major version and has empty folder in it.
    ## e.g. as of 2019-10-01.
    ## they create R-4, https://cran.r-project.org/src/base/R-4/
    ## and there is nothing in it.
    ## current release is R-3.6.1
    r <- sort(r, decreasing = TRUE)
    for(r_major in r) {
        r_major_url <- paste0(base_url, r_major)
        y <- readLines(r_major_url)
        
        pattern <- "R-\\d\\.\\d\\.\\d\\.tar\\.gz"
        y <- y[grep(pattern, y)]
        r_version <- gsub(paste0('.*(', pattern, ').*'), '\\1', y)
        if (length(r_version))
            break
    }

    r_latest <- max(r_version)
    r_latest_version <- gsub("\\.tar\\.gz", "", r_latest)
    r_latest_url <- paste(r_major_url, r_latest, sep='/')


    r_installed_version <- paste0("R-", gsub('\\D+(\\d\\.\\d\\.\\d).*', '\\1', R.version.string))

    res <- list(installed_version = r_installed_version,
                latest_version = r_latest_version,
                latest_url = r_latest_url,
                up_to_date = (r_installed_version == r_latest_version)
                )
    return(res)
}
