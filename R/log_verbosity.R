#' @title Verbosity logging function
#' @description Outputs to the terminal messages about progress, warnings and errors. A timestamp is shown at the
#' beginning of each message, which includes the current date and time.
#' @param msg message to be shown in the terminal.
#' @param level specifies the verbosity setting of the current message, where 0 is reserved for errors,
#' 1 for warnings and 2 for other any other information.
#' used in conjuction with the verbosity parameter to control whether this function operates or not.
#' @param type there are three possible message types: info, warn and error. In case none of these types is passed
#' then the debug type is assumed.
#' @param verbosity is to be used in conjuction with the level parameter, and it is used to control which levels are to
#' be shown, where 0 only shows errors, 1 in addition shows warnings, and 2 in addition shows any other information.
#' @param return_msg if it is set to True then this function will return the complete message, otherwise it will
#' only output to the terminal
#' @return the message to be printed to the terminal in case return_msg is set to TRUE
#' @export
log_verbosity <- function(msg, level = 1, type = "info", verbosity = NULL, return_msg = FALSE) {
  # Get verbosity from parameter or options
  if (is.null(verbosity)) {
    verbosity <- getOption("microsim_verbosity", 1)
  }

  # Only output message if current verbosity >= required level
  if (verbosity >= level) {
    prefix <- switch(type,
                     "info" = "[INFO]",
                     "warn" = "[WARN]",
                     "error" = "[ERROR]",
                     "[DEBUG]"
    )
    fmsg <- paste0(prefix, " ", Sys.time(), ": ", msg, "\n")
    cat(fmsg)
    if (return_msg) return(fmsg)
  }
  invisible(NULL)
}