% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-code.R
\name{export_code}
\alias{export_code}
\title{Utilities for code export}
\usage{
export_code(expressions, board)
}
\arguments{
\item{expressions}{Block expressions}

\item{board}{Board object}
}
\value{
String containing properly arranged block expressions.
}
\description{
To facilitate other means of code export than implemented by the default
\code{\link[=generate_code]{generate_code()}} plugin, this utility performs much of the heavy lifting
to properly arrange and scope block-level expressions.
}
