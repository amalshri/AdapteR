% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLTranspose.R
\name{t}
\alias{t}
\title{Matrix Transpose.}
\usage{
t(object, ...)
}
\arguments{
\item{object}{is of class FLMatrix}

\item{...}{any additional arguments}
}
\value{
\code{t} returns a FLMatrix object which is the transpose of input FLMatrix object
and replicates the equivalent R output.
}
\description{
\code{t} returns the transpose of FLMatrix objects.
}
\section{Constraints}{

Input can be a matrix of dimensions (m x n) where m > n, m < n or m = n.
}
\examples{
flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
resultFLMatrix <- t(flmatrix)
}

