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

opsr_from_opsr <- function(object, data, ...) {
  fit <- opsr(object$formula, data = data, fixed = object$fixed, start = object$start,
              ...)
  fit
}
