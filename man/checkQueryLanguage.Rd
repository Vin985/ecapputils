% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/languageUtils.R
\name{checkQueryLanguage}
\alias{checkQueryLanguage}
\title{CheckQueryLanguage}
\usage{
checkQueryLanguage(session, userInfo)
}
\arguments{
\item{session}{the user shiny session}

\item{userInfo}{a reactive value object where the language will
be stored. The language will be stored in a "lang" attribute}
}
\value{
creates an observer that checks if the language is provided in
the query string and if so changes the current language
}
\description{
CheckQueryLanguage
}

