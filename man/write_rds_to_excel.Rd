% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_rds_to_excel.R
\name{write_rds_to_excel}
\alias{write_rds_to_excel}
\title{Write RDS files to excel}
\usage{
write_rds_to_excel(filePath, filenames)
}
\arguments{
\item{filePath}{Character string. The path to the directory containing the files to process}

\item{filenames}{Character string. A vecor of filenames to convert to excel}
}
\value{
Excel files are created in folder \code{filePath}
}
\description{
Converts output in RDS files to XLSX files
}
