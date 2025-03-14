#' Refit an OPSR Model on New Data
#'
#' @param object an object of class `"opsr"`.
#' @param data a data frame containing the variables in the model.
#' @param ... additional arguments passed to [`opsr`].
#'
#' @return An object of class `"opsr"`
#' @export
opsr_from_opsr <- function(object, data, ...) {
  fit <- OPSR::opsr(object$formula, data = data, fixed = object$fixed, start = object$start,
                    ...)
  fit
}

# opsr_from_opsr <- function(object, data, ...) {
#   if (OPSR:::is_opsr_null(object)) {
#     stop("Null model not supported")
#   }
#   dots <- list(...)
#   oc <- object$call
#   for (i in names(dots)) {
#     di <- dots[[i]]
#     oc[[i]] <- substitute(di)
#   }
#   oc$data <- substitute(data)
#   fit <- eval(oc, parent.frame())
#   fit
# }
