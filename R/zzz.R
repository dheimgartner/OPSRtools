.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0("\nPlease cite the 'OPSRtools' package as:\n",
           "Heimgartner, D. (2025) ",
           "OPSRtools: Estimation and Post-Estimation Routines for the OPSR Package, tbc, ",
           "doi:xxx.\n\n",
           "If you have questions, suggestions, or comments regarding the 'OPSRtools' ",
           "package, please open an issue on https://github.com/dheimgartner/OPSRtools\n\n",
           "To see these entries in BibTeX format, use 'citation('OPSRtools')'"),
    domain = NULL,  appendLF = TRUE)
}
