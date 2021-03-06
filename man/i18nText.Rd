% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/languageUtils.R
\name{i18nText}
\alias{i18nText}
\title{i18nText
Shiny wrapper to get internationalized text based on given key
Text is wrapped in HTML to make sure special characters are correctly displayed.
Only the text is returned}
\usage{
i18nText(key, lang = NULL, data = NULL)
}
\arguments{
\item{lang}{the desired language}

\item{data}{a data.frame with keys as the first column and a column for each
language}
}
\description{
i18nText
Shiny wrapper to get internationalized text based on given key
Text is wrapped in HTML to make sure special characters are correctly displayed.
Only the text is returned
}

