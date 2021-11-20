#' @title Check variable inputs
#'
#' @description Internal function to check input variables
#'
#' @param input An argument.
#' @param input_type The expected type of the argument. (Currently supported: \code{"character"}, \code{"numeric"}, \code{"logical"})
#' @param input_validation Optional.
#' @param min_value Optional.
#' @param max_value Optional.
#' @param max_length Optional.
#' @param variable_name Optional.
#' @param required Optional. If \code{"TRUE"}, checks whether the argument is supplied at all.
#'
#' @return nothing

validate <- function(
  input, input_type = NULL, input_validation = NULL,
  min_value = -Inf, max_value = Inf, max_length = Inf,
  variable_name = NULL, required = NULL
) {

  if (missing(variable_name)) {
    variable_name <- deparse(match.call()$input)
  }

  # Really questioning the purpose here
  # How to best deal with required vs non-missing (optional arguments fuck up a lot here)
  if (!is.null(required)) {
    if (required & missing(input)) {
      stop("Please supply ", variable_name, ". (is missing)")
    }
  }

  if (!missing(input_validation)) {
    if (!(sum(input %in% input_validation) == length(input))) {
      stop("Please supply correct ", variable_name, " value. (allowed values: ",
           paste0(input_validation, collapse = ", "), ")")
    }
  }

  if (is.null(input)) {
    stop("Please supply ", variable_name, ". (is.null)")
  } else if (is.na(input)) {
    stop("Please supply ", variable_name, ". (is.na)")
  } else if (input == "") {
    stop("Please supply ", variable_name, ". (is empty string)")
  } else if (length(input) > max_length) {
    stop("Too many values supplied for ", variable_name, ". (max_length is ",
         max_length, ")")
  }

  if (!is.null(input_type)) {

    if (input_type == "character") {

      tryCatch(
        expr = {
          input <- as.character(input)
        },
        error = function(e) {
          stop("Could not coerce ", variable_name,
               " to the correct format. (is not ", input_type, " type)")
        },
        warning = function(w) {
          stop("Could not coerce ", variable_name,
               " to the correct format. (is not ", input_type, " type)")
        }
      )

    } else if (input_type == "numeric") {

      tryCatch(
        expr = {
          input <- as.numeric(input)
        },
        error = function(e) {
          stop("Could not coerce ", variable_name,
               " to the correct format. (is not ", input_type, " type)")
        },
        warning = function(w) {
          stop("Could not coerce ", variable_name,
               " to the correct format. (is not ", input_type, " type)")
        }
      )

      if (input < min_value) {
        stop("Please enter a ", variable_name,
             " higher than ", min_value, ". (is lower than min_value)")
      } else if (input > max_value) {
        stop("Please enter a ", variable_name,
             " lower than ", max_value, ". (is higher than min_value)")
      }

    } else if (input_type == "logical") {

      tryCatch(
        expr = {
          as.logical(input)
        },
        error = function(e) {
          stop("Could not coerce ", variable_name,
               " to the correct format. (is not ", input_type, " type)")
        },
        warning = function(w) {
          stop("Could not coerce ", variable_name,
               " to the correct format. (is not ", input_type, " type)")
        }
      )

    }

  }


}
