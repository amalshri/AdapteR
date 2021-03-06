% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLTable.R
\name{FLTable}
\alias{FLTable}
\title{Constructor function for FLTable.}
\usage{
FLTable(table, obs_id_colname, var_id_colnames = character(0),
  cell_val_colname = character(0), whereconditions = character(0),
  connection = NULL, type = "double")
}
\arguments{
\item{table}{name of the table}

\item{obs_id_colname}{column name set as primary key}

\item{cell_val_colname}{column name where cell values are stored if \code{FLTable} is deep}

\item{whereconditions}{whereconditions if any to reference the table}

\item{connection}{ODBC/JDBC connection object}

\item{var_id_colname}{column name where variable id's are stored if \code{FLTable} is deep}
}
\value{
\code{FLTable} returns an object of class FLTable mapped to a table
in Teradata.
}
\description{
\code{FLTable} constructs an object of class \code{FLTable}.
}
\details{
\code{FLTable} refers to an in-database table. This is equivalent to data.frame object. 
This object is commonly used as input for data mining functions.
}
\examples{
widetable  <- FLTable("tblAbaloneWide", "ObsID")
deeptable <- FLTable("tblUSArrests","ObsID","VarID","Num_Val")
names(widetable)
}

