#' Make a project data frame.
#'
#' \code{dpMakeProject} creates an initial one-row data frame for
#' the data profiling project.
#'
#' @param name character Short name of the project.
#' @param description character Longer description of project.
#' @param created_by character Person or system creating project.
#' @param notes=NA character Whatever notes needed for project.
#' @return data.frame Initialized of one row with columns:
#' \itemize{
#'   \item project_id - UUID generated for this row
#'   \item project_name - from argument list
#'   \item project_description - from argument list
#'   \item project_notes - optional, from argument list
#'   \item created_by - from argument list
#'   \item created_at - Sys.time()
#'   \item mod_by - initially NA
#'   \item mod_at - initially NA
#' }
#' @examples
#' \dontrun{
#' dpTest <- dpMakeProject("Test1", "A test project", "JimP")
#' summary(dpTest)
#' }
dpMakeProject <- function(name, description, created_by, notes =  NA) {
  # if(is.null(created_by)) stop("created_by not in argument list")
  df <- data.frame(
    project_id = uuid::UUIDgenerate(),
    project_name = name,
    project_description = description,
    project_notes = notes,
    created_by = created_by,
    created_at = Sys.time(),
    mod_by = NA,
    mod_at = NA,
    stringsAsFactors = FALSE
  )
  df
}