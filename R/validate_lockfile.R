
#' Validate the renv lockfile against a schema
#'
#' `renv::validate_lockfile()` can be used to validate your renv.lock
#' against a default or custom schema.
#'
#' Uses ROpenSci's [`jsonvalidate`](https://docs.ropensci.org/jsonvalidate/) package.
#'
#' @param lockfile Contents of the lockfile, or a filename containing one.
#'
#' @param lockfile_schema Contents of the renv schema, or a filename containing a schema.
#'  If not provided, renv's default schema is used.
#'
#' @param engine Specify the validation engine to use.
#'   Options are "imjv" (the default; which uses "is-my-json-valid")
#'   and "ajv" (Another JSON Schema Validator).
#'   The latter supports more recent json schema features.
#'
#' @param greedy Continue after first error?
#'
#' @param error Throw an error on parse failure?
#'   If `TRUE`, then the function returns `NULL` on success
#'   (i.e., call only for the side-effect of an error on failure, like `stopifnot`).
#'
#' @examples
#' \dontrun{
#'
#' # validate the project's lockfile
#' renv::validate_lockfile()
#' }
#' @export
validate_lockfile <- function(project = NULL, lockfile = NULL, lockfile_schema = NULL,
                              engine = "ajv",
                              greedy = FALSE,
                              error = FALSE,
                              verbose = FALSE)
{
  project <- renv_project_resolve(project)
  lockfile <- lockfile %||% renv_lockfile_path(project = project)
  print(lockfile)
  # renv_lockfile_load(project = project)


  if (!file.exists(lockfile))
    stop(paste("No project lockfile exists at", lockfile))

  if (is.null(lockfile_schema)) {
    print("Using {renv} lockfile schema")
    lockfile_schema <- "inst/schema/draft-07.renv.lock.schema.json"
  }

  print("Validating...")
  jsonvalidate::json_validate(
    lockfile,
    lockfile_schema,
    engine  = engine,
    greedy  = greedy,
    error   = error,
    verbose = verbose
  )
}
