\name{decode_null_element}
\alias{decode_null_element}
\title{Deserialize null elements...}
\usage{decode_null_element(raw)}
\description{Deserialize null elements}
\details{The natural R type to the BSON Null value is NULL.
The raw vector should begin with 0A, marking a BSON null.

BSON format:
0A e_name}
\value{a named list whose single element is NULL}
\arguments{\item{raw}{a raw vector}}
