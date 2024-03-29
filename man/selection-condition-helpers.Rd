% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_selection-condition-helpers.R
\name{selection-condition-helpers}
\alias{selection-condition-helpers}
\alias{selection_is_empty}
\alias{selection_is_comment_line}
\alias{selection_is_comment_block}
\alias{selection_is_n_lines}
\alias{selection_is_single_line}
\alias{selection_is_parsable}
\alias{selection_is_evaluable}
\alias{selection_is_litteral}
\alias{selection_is_reserved_word}
\alias{selection_is_symbol}
\alias{selection_is_call}
\alias{selection_matches}
\alias{selection_inherits}
\alias{selection_is_function}
\alias{selection_is_data_frame}
\alias{selection_is_syntactic_package_name}
\alias{selection_is_installed_package}
\alias{selection_is_cran_package}
\alias{selection_is_in_rmd_chunk}
\alias{selection_is_in_rmd_text}
\title{Selection Focused Condition Helpers}
\usage{
selection_is_empty(target = c("default", "lines", "script"))

selection_is_comment_line(target = c("default", "lines", "script"))

selection_is_comment_block(target = c("default", "lines", "script"))

selection_is_n_lines(n, target = c("default", "lines", "script"))

selection_is_single_line(target = c("default", "lines", "script"))

selection_is_parsable(
  multi_ok = TRUE,
  single_ok = TRUE,
  symbol_ok = TRUE,
  empty_ok = FALSE,
  target = c("default", "lines", "script")
)

selection_is_evaluable(
  simple_only = FALSE,
  target = c("default", "lines", "script")
)

selection_is_litteral(type = NA)

selection_is_reserved_word()

selection_is_symbol(litteral_ok = FALSE, reserved_ok = FALSE)

selection_is_call(
  symbol_ok = FALSE,
  litteral_ok = FALSE,
  reserved_ok = FALSE,
  target = c("default", "lines", "script")
)

selection_matches(
  pattern,
  n_min = 1L,
  n_max = Inf,
  target = c("default", "lines", "script"),
  ...
)

selection_inherits(class)

selection_is_function()

selection_is_data_frame()

selection_is_syntactic_package_name()

selection_is_installed_package()

selection_is_cran_package()

selection_is_in_rmd_chunk()

selection_is_in_rmd_text()
}
\arguments{
\item{target}{If \code{target} is \code{"lines"} the selection is extended to lines,
if it is \code{"script"} it is extended to the full script}

\item{n}{number of lines}

\item{multi_ok}{Are multiple calls eligible ?}

\item{single_ok}{Are single calls eligible ?}

\item{symbol_ok}{Are symbols eligible ?}

\item{empty_ok}{whether an empty section qualifies}

\item{simple_only}{if \code{TRUE}, only symbols and calls to \code{::}, \code{:::}, \code{[},
\code{[[} or \code{$} are considered. It should be used on condition calls to make sure
we never evaluate a call that would have side effects.}

\item{type}{atomic type}

\item{litteral_ok}{Are literal strings or numbers eligible ?}

\item{reserved_ok}{Are reserved words eligible ?}

\item{pattern}{regular expression}

\item{n_min}{minimal number of occurences to validate match}

\item{n_max}{maximal number of occurences to validate match}

\item{...}{additional arguments passed to \code{regexpr()}}

\item{class}{class used to check inheritance}
}
\description{
These helpers are meant to be used on the left hand side of the trick specification,
to filter which tricks the addin should display.
}
