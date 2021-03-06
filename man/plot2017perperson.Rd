% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot2017perperson.R
\name{plot2017perperson}
\alias{plot2017perperson}
\title{Plot a leaflet map of cost per person, per CCG, per month of a BNF section, chemical or presentations.}
\usage{
plot2017perperson(argument)
}
\arguments{
\item{argument}{An ID unique to BNF sections, chemicals or presentations.}
}
\value{
Returns a leaflet plot (html widget) of cost per person, per CCG, per month of the input BNF section, drug or chemcial.
}
\description{
Plot a leaflet map of cost per person, per CCG, per month of a BNF section, chemical or presentations.
}
\examples{
plot2017perperson("7.4.5")
}
