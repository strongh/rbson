\name{encode_datetime}
\alias{encode_datetime}
\title{Functions for BSON datetime type...}
\usage{encode_datetime(datetime)}
\description{Functions for BSON datetime type}
\details{The BSON datetime is UTC millisecond since the unix epoch. 
This is conveniently the internal representation of dates in R.}
\arguments{\item{num}{a R date to convert}
\item{raw}{a raw vector to convert}
\item{name}{the name of a datetime BSON element}}
