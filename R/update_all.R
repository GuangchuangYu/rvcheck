##' update all packages
##'
##'
##' @title update_all
##' @param check_R whether check R version
##' @param which repo (CRAN, BioC, github) to update
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
update_all <- function(check_R=TRUE, which=c("CRAN", "BioC", "github")) {
    if (check_R && !check_r()$up_to_date) {
        stop("you need to upgrade your R first...")
    }

    if ('CRAN' %in% which) {
        update_cran()
    }

    if ('BioC' %in% which) {
        update_bioc()
    }

    if ('github' %in% which) {
        update_github()
    }
    message("done....")
}

is_bioc_up_to_date <- function() {
    suppressMessages(check_bioc()$up_to_date)
}

update_cran <- function() {
    message("upgrading CRAN packages...")
    tryCatch(update.packages(ask=FALSE, checkBuilt=TRUE),
             error=function(e) NULL)
}

update_bioc <- function() {
    pkg <- "BiocInstaller"
    bioc_version <- tryCatch(packageVersion(pkg), error=function(e) NULL)
    if (is.null(bioc_version)) {
        message("no Bioconductor packages found...")
    } else {
        bioc <- is_bioc_up_to_date()
        if (is.na(bioc)) {
            message("You are using devel branch of Bioconductor...")
        } else if (!bioc) {
            message("BiocInstaller is out of date...")
            message("Upgrading BiocInstaller...")
            if (pkg %in% loadedNamespaces())
                detach("package:BiocInstaller", character.only=TRUE)
            remove.packages("BiocInstaller")
            source("https://www.bioconductor.org/biocLite.R")
        }
        message("upgrading BioC packages...")
        suppressPackageStartupMessages(require(pkg, character.only = TRUE))
        biocLite <- eval(parse(text="biocLite"))
        biocLite(ask=FALSE, checkBuilt=TRUE)
    }
}


##' @importFrom utils installed.packages
##' @importFrom utils packageDescription
update_github <- function() {
    message("upgrading github packages...")
    pkgs <- installed.packages()[, 'Package']
    install_github <- get_fun_from_pkg("devtools", "install_github")
    tmp <- sapply(pkgs, function(pkg) {
        desc <- packageDescription(pkg)
        if (length(desc) <= 1 || is.null(desc$GithubSHA1))
            return(NULL)
        tryCatch(install_github(repo=paste0(desc$GithubUsername, '/', desc$GithubRepo), checkBuilt=TRUE),
                 error=function(e) NULL)
    })
}

