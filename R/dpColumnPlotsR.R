#' Data Profile Column Plot Data via R
#'
#' \code{dpColumnPlotsR} calculates the values for column level plots.
#'
#' This design separates the plot data generation for plotting. There are two
#' advantages of this design. First, for large data we want to offload the calculation
#' to the data platform so we only get back reduced data. Secondly, different report
#' designs will require unique plot aesthetics.
#'
#' Generates the data for a bar or histogram plot. Note \code{x_var} is a character and
#' should be converted to numeric when ploting a histogram.
#'
#' For DBMS resident data you should use \code{dpColumnPlotsR} to avoid downloading all
#' rows to R.
#'
#' @param in_project_id character The UUID ID for the project in
#'   which this table is to be included.
#' @param in_table_id integer The table sequence number for table to analize.
#' @param input_tbl data.frame The data frame to analize.
#' @param dp_col_prop data.frame The data frame with the column properties of
#' \code{input_tbl} that has been previously generated.
#' @param max_levels = 12 The maximum number of levals to include in a catigorical plot
#' @return data.frame with a row for each column in input_tbl and columns:
#' \itemize{
#'   \item project_id - UUID of project with this table
#'   \item table_id - of table with these columns
#'   \item column_id - integer sequence of column
#'   \item plot_id - intially = 1; may eventually have more than one plot per column
#'   \item plot_type - one of ["catigorical", "first character", "histogram"]
#'   \item x_var character - label of the bar
#'   \item count integer - number of rows in bar's value or range
#' }
#' @examples
#' \dontrun{
#' dpProjID <- dpTables$project_id[1]
#' dpTblID <- 1
#' Tbl <- read_csv(dpTables$table_source[dpTblID])
#' dpColProp <- dpColumnPropertiesR(dpProjID, dpTblID, Tbl)
#' dpColPlts <- dpColumnPlotsR(dpProjID, dpTblID, Tbl, dpColProp)
#' }
dpColumnPlotsR <- function(in_project_id, in_table_id, input_tbl, dp_col_prop,
                           max_levels = 12){
  df <- NULL
  for(ColRow in seq_along(input_tbl)){
    col_sum_i <- subset(dp_col_prop, project_id == in_project_id &
                        table_id == in_table_id & column_id == ColRow)
    ColName <- col_sum_i$column_name
    while(TRUE) {
      if(col_sum_i$num_distinct < max_levels) {     ## a few discrete levels > bar plot
        plot_type <- "catigorical"
        bars <- as.data.frame(table(input_tbl[ColRow]),
                              stringsAsFactors = FALSE)
        break
      }
      if(col_sum_i$column_type == "character"){    ## bin by 1st character of string
        plot_type <- "first character"
        bars <- as.data.frame(table(stringr::str_sub(input_tbl[[ColRow]],end = 1)),
                              stringsAsFactors = FALSE)
        break
      }
      if(col_sum_i$column_type %in% c("integer", "numeric")){
        plot_type <- "histogram"
        bars <- as.data.frame(table(cut(input_tbl[[ColRow]],
                                        breaks = pretty(input_tbl[[ColRow]], 50))),
                              stringsAsFactors = FALSE)
        bars$Var1 <- as.numeric(stringr::str_replace(
          stringr::str_split_fixed(bars$Var1, stringr::fixed(","), n = 2)[, 2], stringr::fixed("]"), ""))
        break
      }
      if(col_sum_i$column_type %in% c("Date", "POSIXct")){
        plot_type <- "histogram"
        bars <- as.data.frame(table(cut(input_tbl[[ColRow]],
                                        breaks = pretty(input_tbl[[ColRow]], 50))),
                              stringsAsFactors = FALSE)
        break
      }
    }
    names(bars) <- c("x_var", "count")
    dfi <- cbind(data.frame(project_id = in_project_id,
                            table_id = in_table_id,
                            column_id = ColRow,
                            plot_id = 1,
                            plot_type = plot_type,
                            stringsAsFactors = FALSE),
                 bars)
    df <- rbind(df, dfi)
  }
  df
}
