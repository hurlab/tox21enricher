#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
jsme <- function(structure='', width = '650px', height = '650px') {

  # forward options using x
  x = list(
    structure = structure
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'jsme',
    x = x,
    width = width,
    height = height,
    package = 'jsme'
  )
}

smiles <- function(session, element, inputEl) {
  session$sendCustomMessage(type = 'smiles',
                            message = list(el=element, inputEl = inputEl))
}

molFile <- function(session, element, inputEl) {
  session$sendCustomMessage(type = 'molFile',
                            message = list(el=element, inputEl = inputEl))
}

jmeFile <- function(session, element, inputEl) {
  session$sendCustomMessage(type = 'jmeFile',
                            message = list(el=element, inputEl = inputEl))
}

useMOL <- function(session, element, molFile) {
  session$sendCustomMessage(type = 'useMOL',
                            message = list(el=element, molFile = molFile))
}

useJME <- function(session, element, jmeFile) {
  session$sendCustomMessage(type = 'useJME',
                            message = list(el=element, jmeFile = jmeFile))
}

resetEditor <- function(session, element) {
  session$sendCustomMessage(type = 'resetEditor',
                            message = list(el=element))
}

#' Shiny bindings for jsme
#'
#' Output and render functions for using jsme within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a jsme
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name jsme-shiny
#'
#' @export
jsmeOutput <- function(outputId, width = '100px', height = '100px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'jsme', width, height, package = 'jsme')
}

#' @rdname jsme-shiny
#' @export
renderJsme <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, jsmeOutput, env, quoted = TRUE)
}
