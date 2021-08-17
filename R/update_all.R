##' update all packages
##'
##'
##' @title update_all
##' @param check_R whether check R version
##' @param which repo (CRAN, BioC, github) to update
##' @param lib.loc location of library, default is NULL and will set to .libPaths()
##' @param ... additional parameters to install packages
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
update_all <- function(check_R=TRUE, which=c("CRAN", "BioC", "github"), lib.loc = NULL, ...) {
    if (check_R && !check_r()$up_to_date) {
        stop("you need to upgrade your R first...")
    }

    if ('CRAN' %in% which) {
        update_cran(lib.loc = lib.loc, ...)
    }

    if ('BioC' %in% which) {
        update_bioc(lib.loc = lib.loc, ...)
    }

    if ('github' %in% which) {
        update_github(lib.loc = lib.loc, ...)
    }
    message("done....")
}

is_bioc_up_to_date <- function() {
    suppressMessages(check_bioc()$up_to_date)
}

update_cran <- function(lib.loc = NULL, ...) {
    message("upgrading CRAN packages...")
    tryCatch(update.packages(lib.loc = lib.loc,
                             ask=FALSE,
                             checkBuilt=TRUE, ...),
             error=function(e) NULL)
}

##' @importFrom utils install.packages
update_bioc <- function(lib.loc = NULL, ...) {
    pkg <- "BiocManager"
    bioc_version <- tryCatch(packageVersion(pkg, lib.loc = lib.loc), error=function(e) NULL)
    flag <- "BiocManager"
    if (is.null(bioc_version)) {
        biocLite <- tryCatch(packageVersion("BiocInstaller", lib.loc = lib.loc), error=function(e) NULL)
        if (is.null(biocLite)){
            flag <- "No_BioC"
        } else if (check_r()$installed_version < "R-3.5.0") {
            flag <- "BiocInstaller"
        } else {
            message('Bioconductor has switched to a new package manager: "BiocManager".')
            message("Removing BiocInstaller and install BiocManager")
            remove.packages("BiocInstaller", lib = lib.loc)
            install.packages("BiocManager", lib = lib.loc)
            flag <- "BiocManager"
        }
    }

    if (flag == "No_BioC"){
        message("no Bioconductor packages found...")
    } else if (flag == "BiocInstaller") {
        message("Your R is out-dated.")
        message('Bioconductor 3.8 has switched to a new package manager: "BiocManager".')
        invisible(readline(prompt="Press [enter] to continue to update Bioconductor (outdated release that fit your R version)"))
        if ("BiocInstaller" %in% loadedNamespaces()) {
            detach("package:BiocInstaller", character.only=TRUE)
            remove.packages("BiocInstaller", lib = lib.loc)
            source("https://www.bioconductor.org/biocLite.R")
        }
        suppressPackageStartupMessages(require(pkg, character.only = TRUE))
        biocLite <- eval(parse(text="biocLite"))
        biocLite(ask=FALSE, checkBuilt=TRUE)
    } else {
        bioc <- is_bioc_up_to_date()
        if (is.na(bioc)) {
            message("You are using devel branch of Bioconductor...")
        } else if (!bioc) {
            message("BiocManager is out of date...")
            message("Upgrading BiocManager...")
            if (pkg %in% loadedNamespaces())
                detach("package:BiocManager", character.only=TRUE)
            remove.packages("BiocManager")
            install.packages("BiocManager")
        }

        x <- readLines("https://bioconductor.org/about/release-announcements/")
        i <- grep("/news/bioc_\\d_\\d+_release", x)[1]
        remote_version <- sub(".*(\\d\\.\\d{1,2}).*", "\\1", x[i])
        version <- eval(parse(text = "BiocManager::version"))
        local_version <- as.character(version())
        major <- function(version) as.numeric(sub("\\.\\d+$",  "", version))
        minor <- function(version) as.numeric(sub("^\\d\\.",  "", version))

        install <- eval(parse(text="BiocManager::install"))
        install_version <- local_version

        if (local_version < remote_version ||
            major(local_version) < major(remote_version) ||
            (major(local_version) == major(remote_version) &&
             minor(local_version) < minor(remote_version))
            ) {
            versions <- c(remote_version, local_version)
            i <- utils::menu(versions, title="Which Bioconductor version to install?")
            ## invisible(readline(prompt=paste0("Press [enter] to continue to upgrade to Bioconductor (",
            ##                                 remote_version, ")")))
            install_version <- versions[i]
        }

        message("upgrading BioC packages...")
        suppressPackageStartupMessages(require(pkg, character.only = TRUE))
        install(ask=FALSE, checkBuilt=TRUE, version = install_version, lib.loc = lib.loc, ...)
        
    }
}


##' @importFrom utils installed.packages
##' @importFrom utils packageDescription
##' @importFrom yulab.utils get_fun_from_pkg
update_github <- function(lib.loc = NULL, ...) {
    message("upgrading github packages...")
    pkgs <- installed.packages()[, 'Package']
    install_github <- get_fun_from_pkg("remotes", "install_github")
    tmp <- sapply(pkgs, function(pkg) {
        desc <- packageDescription(pkg, lib.loc = lib.loc)
        if (length(desc) <= 1 || is.null(desc$GithubSHA1))
            return(NULL)
        tryCatch(install_github(repo=paste0(desc$GithubUsername, '/', desc$GithubRepo),
                                ref = desc$GithubRef,
                                checkBuilt=TRUE,
                                lib.loc = lib.loc, ...),
                 error=function(e) NULL)
    })
}

