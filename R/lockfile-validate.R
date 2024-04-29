
#' Validate the renv lockfile against a schema
#'
#' @description
#' `renv::lockfile_validate()` can be used to validate your `renv.lock`
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
#'
#' @param verbose Boolean. If `TRUE`, then an attribute `errors` will list validation failures as a `data.frame`.
#' 
#' @param strict Boolean. Set whether the schema should be parsed strictly or not.
#'   If in strict mode schemas will error to "prevent any unexpected behaviours or silently ignored mistakes in user schema".
#'   For example it will error if encounters unknown formats or unknown keywords.
#'   See https://ajv.js.org/strict-mode.html for details.
#'
#' @return Boolean. `TRUE` if validation passes. `FALSE` if validation fails.
#'
#' @examples
#' \dontrun{
#'
#' # validate the project's lockfile
#' renv::lockfile_validate()
#'
#' # validate the project's lockfile using a non-default schema
#' renv::lockfile_validate(lockfile_schema = "/path/to/your/custom/schema.json")
#'
#' # validate a lockfile using its path
#' renv::lockfile_validate(lockfile = "/path/to/your/renv.lock")
#' }
#' @export
lockfile_validate <- function(project = NULL,
                              lockfile = NULL,
                              lockfile_schema = NULL,
                              greedy = FALSE,
                              error = FALSE,
                              verbose = FALSE,
                              strict = FALSE)
{
  project <- renv_project_resolve(project)
  lockfile <- lockfile %||% renv_lockfile_path(project = project)

  # if (!is.null(lockfile))
  #   stop(paste("No project lockfile exists at", lockfile))
  # if (!file.exists(lockfile))
  #   stop(paste("No project lockfile exists at", lockfile))

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
                              verbose = verbose,
                              strict = strict)
}
