.onAttach <- function(libname, pkgname) {
    BoostMLR.version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                      fields="Version")
    packageStartupMessage(paste("\n",
                                pkgname,
                                BoostMLR.version,
                                "\n",
                                "\n",
                                "Type BoostMLR.news() to see new features, changes, and bug fixes.",
                                "\n",
                                "Email at hansie.biostat@gmail.com to report any bugs or ask for clarifications.",
                                "\n",
                                "\n"))
}
