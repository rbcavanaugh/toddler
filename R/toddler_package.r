#' toddler: Base R Functions to Mess Up Clean Data for Teaching
#'
#' A collection of functions that introduce common data quality issues into
#' clean datasets, designed for teaching data cleaning and validation. Like a
#' toddler with a crayon, these functions will gleefully mess up your perfectly
#' organized data in educationally valuable ways.
#'
#' @details
#' The toddler package provides functions to systematically introduce realistic
#' data quality problems:
#'
#' \strong{Structural Problems:}
#' \itemize{
#'   \item \code{\link{toddler_wide}} - Convert tidy data to wide format
#'   \item \code{\link{toddler_unjoin}} - Split data into multiple pieces
#'   \item \code{\link{toddler_extra}} - Add unwanted header and summary rows
#' }
#'
#' \strong{Formatting Issues:}
#' \itemize{
#'   \item \code{\link{toddler_names}} - Mess up column name consistency
#'   \item \code{\link{toddler_whitespace}} - Add random leading/trailing spaces
#'   \item \code{\link{toddler_inconsistent}} - Make categorical values inconsistent
#' }
#'
#' \strong{Data Type Problems:}
#' \itemize{
#'   \item \code{\link{toddler_types}} - Mix text values into numeric columns
#'   \item \code{\link{toddler_units}} - Add unit suffixes to numbers
#' }
#'
#' \strong{Completeness Issues:}
#' \itemize{
#'   \item \code{\link{toddler_missing}} - Introduce missing values (NAs)
#'   \item \code{\link{toddler_duplicate}} - Add duplicate rows
#' }
#'
#' All functions are pipe-friendly and work with base R, requiring no additional
#' dependencies beyond base R.
#'
#' @section Package philosophy:
#' Real-world data is messy. Students learn data cleaning best when they work
#' with realistic problems rather than artificially clean datasets. This package
#' helps educators create datasets that mirror the chaos students will encounter
#' in practice.
#'
#' @docType _PACKAGE
#' @name toddler-package
#' @aliases toddler
NULL
