\name{encode_cstring}
\alias{encode_cstring}
\title{Serialize cstring elements...}
\usage{encode_cstring(char)}
\description{Serialize cstring elements}
\details{Converts between R chars and BSON cstrings.
cstrings are typically used as e_names.}
\value{a raw vector}
\arguments{\item{name}{a char from the R names, to be used as the BSON e_name}
\item{val}{should be NULL}}
