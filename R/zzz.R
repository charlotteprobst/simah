# Package-level global variables for R CMD check
# See: https://r-pkgs.org/dependencies-in-metadata.html
#
# These special pronouns from dplyr need to be declared because
# R CMD check doesn't recognize them as valid symbols

utils::globalVariables(c(
  ".",
  ".data"
))
