% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spread_values.R
\name{json_functions}
\alias{json_functions}
\alias{jstring}
\alias{jlogical}
\alias{jinteger}
\alias{jdouble}
\alias{jnumber}
\title{Navigates nested objects to get at names of a specific type, to be used as
arguments to \code{\link{spread_values}}}
\usage{
jstring(..., recursive = FALSE)

jlogical(..., recursive = FALSE)

jinteger(..., recursive = FALSE)

jdouble(..., recursive = FALSE)

jnumber(..., recursive = FALSE)
}
\arguments{
\item{...}{a quoted or unquoted sequence of strings designating the object
name sequence you wish to follow to find a value}

\item{recursive}{logical indicating whether second level and beyond objects
should be extracted.  Only works when there exists a single value in
the nested json object}
}
\value{
a function that can operate on parsed JSON data
}
\description{
Note that these functions fail if they encounter the incorrect type. Note
that \code{jnumber()} is an alias for \code{jdouble()}.
}
\seealso{
\code{\link{spread_values}} for using these functions to spread
         the values of a JSON object into new columns
}
