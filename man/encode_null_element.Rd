\name{encode_null_element}
\alias{encode_null_element}
\title{Serialize null elements...}
\usage{encode_null_element(name, val)}
\description{Serialize null elements}
\details{The natural R type to the BSON Null value is NULL.

BSON format:
0A e_name}
\value{a raw vector}
\arguments{\item{name}{a char from the R names, to be used as the BSON e_name}
\item{val}{should be NULL}}
