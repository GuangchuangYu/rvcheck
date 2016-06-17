##' update all packages
##'
##' 
##' @title update_all
##' @return NULL
##' @importFrom utils update.packages
##' @importFrom utils remove.packages
##' @export
##' @examples
##' \dontrun{
##'	library(rvcheck)
##' update_all()
##' }
##' @author Guangchuang Yu
update_all <- function() {
    pkg <- "BiocInstaller"
    bioc_version <- tryCatch(packageVersion(pkg), error=function(e) NA)

    if (!is.na(bioc_version)) {
        bioc <- is_bioc_up_to_date()
        if (is.na(bioc)) {
            message("You are using devel branch of Bioconductor...")
        } else if (!bioc) {
            if (!check_r()$up_to_date) {
                stop("you need to upgrade your R first...")
            }
            
            message("BiocInstaller is out of date...")
            message("Upgrading BiocInstaller...")
            if (pkg %in% loadedNamespaces())
                detach("package:BiocInstaller", character.only=TRUE)
            remove.packages("BiocInstaller")
            source("https://www.bioconductor.org/biocLite.R")
        }
        require(pkg, character.only = TRUE)
        biocLite <- eval(parse(text="biocLite"))
        biocLite(ask=FALSE, checkBuilt=TRUE)
    } else {
        update.packages(ask=FALSE, checkBuild=TRUE)
    }
}

is_bioc_up_to_date <- function() {
    suppressMessages(check_bioc()$up_to_date)
}
