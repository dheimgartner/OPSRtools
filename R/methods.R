#' @importFrom methods setMethod
#' @importFrom texreg extract
methods::setMethod("extract", signature = className("opsr.select", "OPSRtools"),
                   definition = OPSR:::extract.opsr)
