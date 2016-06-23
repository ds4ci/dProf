#' Make a table data frame.
#'
#' \code{dpMakeTable} creates an initial one-row data frame for
#' a table in the data profiling project.
#'
#' @param in_project_id character The UUID ID for the project in
#'   which this table is to be included.
#' @param name character R data.frame or DBMS table name
#' @param source character The literal string to connect to the table. Will
#'   be a function of environment; eg text file, DBMS, R, etc.
#' @param description character A longer description of table.
#' @param created_by character Person or system creating this table entry.
#' @param notes character Notes about this table.
#' @return data.frame Initialized to one row with columns:
#' \itemize{
#'   \item project_id - UUID of project with this table
#'   \item table_id - integer sequence of tables within project
#'   \item table_name - from argument list
#'   \item table_source - from argument list
#'   \item table_description
#'   \item table_notes- from argument list
#'   \item table_rows - # rows in table
#'   \item table_columns - # columns in table
#'   \item created_by - from argument list
#'   \item created_at - Sys.time()
#'   \item mod_by - initially NA
#'   \item mod_at - initially NA
#' }
#' @examples
#' \dontrun{
#' TblName <- "NewsCurrentEvents"
#' TblSource <- system.file("extdata", "NewsCurrentEvents.csv",
#'                           package = "dProf")
#' dpTables <- dpMakeTable(dpTest$project_id[1], TblName, TblSource, "Jim P",
#'                         notes = "Using the example csv in project")
#' str(dpTables)
#' }
dpMakeTable <- function(in_project_id, name, source, description,
created_by, notes = NA){
  if(is.null(created_by)) stop("created_by not in argument list")
  df <- data.frame(
    project_id = in_project_id,
    table_id = 1,
    table_name = name,
    table_source = source,
    table_description = description,
    table_notes = notes,
    table_rows = 0,
    table_columns = 0,
    created_by = created_by,
    created_at = Sys.time(),
    mod_by = NA,
    mod_at = NA,
    stringsAsFactors = FALSE
  )
  df
}

