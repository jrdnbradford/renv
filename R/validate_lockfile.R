
#' Validate the renv lockfile against a schema
#'
#' `renv::validate_lockfile()` can be used to validate your `renv.lock`
#' against a default or custom schema.
#'
#' Uses ROpenSci's [`jsonvalidate`](https://docs.ropensci.org/jsonvalidate/) package.
#'
#' @param lockfile Contents of the lockfile, or a filename containing one.
#'   If not provided, it defaults to the project's lockfile.
#'
#' @param lockfile_schema Contents of the renv schema, or a filename containing a schema.
#'   If not provided, renv's default schema is used.
#'
#' @param greedy Boolean. Continue after first error?
#'
#' @param error Boolean. Throw an error on parse failure?
#'   If `TRUE`, then the function returns `NULL` on success
#'   (i.e., call only for the side-effect of an error on failure, like `stopifnot`).
#'
#' @return `TRUE` if validation passes.
#'   `FALSE` if validation fails.
#'   If `error = TRUE`, returns `NULL` on success.
#'
#' @examples
#' \dontrun{
#'
#' # validate the project's lockfile
#' renv::validate_lockfile()
#'
#' # validate the project's lockfile using a non-default schema
#' renv::validate_lockfile(lockfile_schema = "/path/to/your/custom/schema")
#'
#' # validate a lockfile using its path
#' renv::validate_lockfile(lockfile = "/path/to/your/lockfile")
#' }
#' @export
validate_lockfile <- function(project = NULL,
                              lockfile = NULL,
                              lockfile_schema = NULL,
                              greedy = FALSE,
                              error = FALSE,
                              verbose = FALSE)
{
  project <- renv_project_resolve(project)
  lockfile <- lockfile %||% renv_lockfile_path(project = project)
  print(paste("Lockfile path:", lockfile))
  # renv_lockfile_load(project = project)

  if (!file.exists(lockfile))
    stop(paste("No project lockfile exists at", lockfile))

  if (is.null(lockfile_schema)) {
    print("Using {renv} lockfile schema")
    lockfile_schema <- system.file("schema",
                                   "draft-07.renv.lock.schema.json",
                                   package = "renv",
                                   mustWork = TRUE)
  }

  print("Validating...")
  # "ajv" engine required for schema specifications later than draft-04"
  jsonvalidate::json_validate(lockfile,
                              lockfile_schema,
                              engine = "ajv",
                              greedy = greedy,
                              error = error,
                              verbose = verbose)
}
