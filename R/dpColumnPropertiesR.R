#' Data Profile Column Properties via R
#'
#' \code{dpColumnPropertiesR} calculates the column properties of an R data frame.
#'
#' The data frame is typically loaded from a text file, Excel sheet, or other tabular
#' structure.
#'
#' For DBMS resident data you should use dpColumnPropertiesSQL to avoid downloading
#' all rows to R.
#'
#' @param in_project_id character The UUID ID for the project in
#'   which this table is to be included.
#' @param in_table_id integer The table sequence number for table to analize.
#' @param input_tbl data.frame The data frame to analize.
#' @param collapse_with = "|" character The characer string used with \code{collapse}
#' @return data.frame with a row for each column in input_tbl and columns:
#' \itemize{
#'   \item project_id - UUID of project with this table
#'   \item table_id - of table with these columns
#'   \item column_id - integer sequence of column
#'   \item column_name - from table meta-data
#'   \item column_type - from table meta-data. NULL if no meta-data in which case
#'     all columns loaded as text
#'   \item column_content_type - most predominate type discovered in text date. NULL
#'     not text.
#'   \item column_dommain - as discovered by scan. PLACE HOLDER FOR VER 0.2 (URL,
#'     ZIP, Phone, Date, Age, ...)
#'   \item num_rows - number of rows in table (nrow(input_tbl))
#'   \item num_nulls - number NA's in column
#'   \item num_distinct - number distinct values in column
#'   \item num_empty - if character, number null-strings (as distinct from NAs), else NA
#'   \item num_numeric - if character, number represinting numbers, else NA
#'   \item num_date - if character, number representing dates or timestamps, else NA
#'   \item mean - NA if not numeric
#'   \item minimum - smallest value as string
#'   \item first_quartile - value at 25% along sorted vector as string
#'   \item second_quartile - value at 50% along sorted vector as string
#'   \item third_quartile - value at 75% along sorted vector as string
#'   \item maximum - largest value as string
#'   \item min_length - minimum number of characters or significant digits
#'   \item max_lenght - maximum number of characters or significant digits
#'   \item head - collapsed \code{head()} limited to 250 characters
#'   \item tail - collapsed \code{tail()} limited to 250 characters
#'   \item bottom - collapsed \code{head(sort())} limited to 250 characters
#'   \item top - collapsed \code{tail(sort())} limited to 250 characters
#' }
#' @examples
#' \dontrun{
#' dpProjID <- dpTables$project_id[1]
#' dpTblID <- 1
#' Tbl <- read_csv(dpTables$table_source[dpTblID])
#' dpColProp <- dpColumnPropertiesR(dpProjID, dpTblID, Tbl)
#' }
dpColumnPropertiesR <- function(in_project_id, in_table_id, input_tbl,
                               collapse_with = "|"){
  colNames <- colnames(input_tbl)
  df <- NULL
  for(ithCol in seq_along(colNames)){
    colClass <- class(input_tbl[[ithCol]])
    # colSummary <- summary(input_tbl[ithCol])
    colFiveValue <- fiveval(input_tbl[[ithCol]])
    if (colClass %in% c("integer", "numeric"))
      colCharLength <- nchar(prettyNum(input_tbl[[ithCol]], drop0trailing = TRUE,
                                       trim = TRUE, digits = 20))
    else
      colCharLength <- nchar(input_tbl[[ithCol]])

    dfi <- data.frame(
      project_id = in_project_id,
      table_id = in_table_id,
      column_id = ithCol,
      column_name = colNames[ithCol],
      column_type = colClass,
      column_content_type = NA,
      column_domain = NA,
      num_rows = nrow(input_tbl),
      num_nulls = sum(is.na(input_tbl[[ithCol]])),
      num_distinct = length(unique(input_tbl[[ithCol]])),
      num_empty = ifelse(colClass != "character", NA,
                         sum(!nzchar(input_tbl[[ithCol]]))),
      num_numeric = ifelse(colClass != "character", NA,
                           sum(is.numeric(input_tbl[ithCol]))),
      num_date = ifelse(colClass[1] != "character", NA,
                        sum(dProf::try.Date(input_tbl[[ithCol]]))),
      mean = ifelse(colClass %in% c("numeric", "integer"),
                    mean(input_tbl[[ithCol]], na.rm = TRUE), NA),
      minimum = colFiveValue[1],
      first_quartile = colFiveValue[2],
      second_quartile = colFiveValue[3],
      third_quartile = colFiveValue[4],
      maximum = colFiveValue[5],
      min_length = min(colCharLength),
      max_length = max(colCharLength),
      head = stringi::stri_sub(paste(head(input_tbl[[ithCol]],100)
                            , collapse = collapse_with), length = 250),
      tail = stringi::stri_sub(paste(tail(input_tbl[[ithCol]],100),
                            collapse = collapse_with), length = 250),
      bottom = stringi::stri_sub(paste(head(sort(input_tbl[[ithCol]]),100),
                              collapse = collapse_with), length = 250),
      top = stringi::stri_sub(paste(head(sort(input_tbl[[ithCol]], decreasing = TRUE),100),
                           collapse = collapse_with), length = 250),
      stringsAsFactors = FALSE
    )
    df <- rbind(df, dfi)
  }
  df
}