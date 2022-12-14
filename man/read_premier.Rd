% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_premier.R
\name{read_premier}
\alias{read_premier}
\title{Read data from the premier healthcare database (PHD)}
\usage{
read_premier(
  no_apr_file,
  ICD_proc_file = NULL,
  ICD9_proc_codes = NULL,
  ICD10_proc_codes = NULL,
  CPT_file = NULL,
  CPT_codes = NULL,
  ICD_diag_file = NULL,
  ICD9_diag_codes = NULL,
  ICD10_diag_codes = NULL,
  charge_file = NULL,
  charge_codes = NULL,
  filter_codes = NULL,
  medrec_ids = NULL
)
}
\arguments{
\item{no_apr_file}{name of file containing demographic information}

\item{ICD_proc_file}{name of file containing ICD procedure codes}

\item{ICD9_proc_codes}{named list containing codes of interest}

\item{ICD10_proc_codes}{named list containing codes of interest}

\item{CPT_file}{name of file containing CPT codes}

\item{CPT_codes}{named list containing codes of interest}

\item{ICD_diag_file}{name of file containing ICD procedure codes}

\item{ICD9_diag_codes}{named list containing codes of interest}

\item{ICD10_diag_codes}{named list containing codes of interest}

\item{charge_file}{name of file containing premier charge codes}

\item{charge_codes}{named list containing codes of interest}

\item{filter_codes}{column names that should be used to filter data.
Should match the naming convention: e.g., \code{ICD9_[name used in ICD9_diag_codes]},
\code{ICDP10_[name used in ICD10_proc_codes]}, \code{CPT_[name used in CPT_codes]}}

\item{medrec_ids}{specific patient IDs to be retained}
}
\value{
data.table containing encounters matching codes (or patients) of interest
with demograpic information
}
\description{
This is a wrapper function for \code{read_premier_codes()}
}
