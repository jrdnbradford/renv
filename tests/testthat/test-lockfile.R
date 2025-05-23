
test_that("lockfiles can be diffed", {

  lhs <- list(A = 1, B = 2, C = "a", D = list(E = 1, F = 2))
  rhs <- list(A = 1, B = 3, C = "b", D = list(E = 1, F = 3))

  diff <- renv_lockfile_diff(lhs, rhs)
  expected <- list(
    B = list(before = 2, after = 3),
    C = list(before = "a", after = "b"),
    D = list(
      F = list(before = 2, after = 3)
    )
  )

  expect_identical(diff, expected)

})

test_that("we can serialize lockfiles using unnamed repositories", {

  # no repositories set
  local({
    renv_scope_options(repos = list())
    actual <- renv_lockfile_init(project = NULL)
    json <- renv_lockfile_write(actual, file = NULL)
    expected <- renv_lockfile_read(text = json)
    expect_equal(actual, expected)
  })

  # unnamed repositories set
  local({
    renv_scope_options(repos = c("alpha", "beta"))
    actual <- renv_lockfile_init(project = NULL)
    json <- renv_lockfile_write(actual, file = NULL)
    expected <- renv_lockfile_read(text = json)
    expect_equal(actual, expected)
  })

})

test_that("we can create lockfiles from manifests", {

  skip_on_cran()
  lock <- renv_lockfile_from_manifest("resources/manifest.json")

  expect_equal(lock$R$Version, "4.2.1")
  expect_equal(lock$R$Repositories, list(CRAN = "https://cloud.r-project.org"))

})

test_that("we create lockfile from a manifest automatically when no lockfile found", {

  skip_on_cran()

  project <- tempfile()
  dir.create(project)

  path <- renv_tests_path("resources/manifest.json")
  expected <- renv_lockfile_from_manifest(path)
  file.copy(path, file.path(project, "manifest.json"))

  # when called with `strict = TRUE` does not create manifest
  expect_error(renv_lockfile_load(project, strict = TRUE))

  # creates and reads lockfile
  actual <- renv_lockfile_load(project)
  expect_identical(expected, actual)
  expect_true(file.exists(file.path(project, "renv.lock")))

  unlink(project, recursive = TRUE)

})

test_that("the Requirements field is read as character", {

  lockfile <- renv_lockfile_read(text = '
{
  "R": {
    "Version": "2.15.2",
    "Repositories": []
  },
  "Packages": {
    "morning": {
      "Package": "morning",
      "Version": "0.1.0",
      "Requirements": [
        "coffee",
        "toast"
      ]
    }
  }
}
')

  actual <- lockfile$Packages$morning$Requirements
  expected <- c("coffee", "toast")
  expect_identical(actual, expected)

})

test_that("lockfile APIs can be used", {
  renv_tests_scope("breakfast")
  init()

  lockfile <- lockfile_read()
  expect_s3_class(lockfile, "renv_lockfile")

  # try writing some repositories
  repos <- list(CRAN = "https://cloud.r-project.org")
  lockfile <- lockfile_modify(lockfile, repos = repos)
  expect_equal(repos, lockfile$R$Repositories)

  # try updating a record
  lockfile <- lockfile_modify(lockfile, remotes = list(bread = "bread@0.1.0"))
  bread <- lockfile$Packages$bread
  expect_equal(bread$Version, "0.1.0")

  # try writing to file
  lockfile_write(lockfile)

  # check that it's the same
  expect_equal(lockfile, lockfile_read())

})

test_that("lockfiles with UTF-8 contents can be written, read", {

  author <- enc2utf8("cr\u00e8me br\u00fbl\u00e9e")
  Encoding(author) <- "UTF-8"

  lockfile <- list(
    Packages = list(
      example = list(
        Package = "example",
        Version = "1.0.0",
        Author  = author
      )
    )
  )

  path <- renv_scope_tempfile("renv-lockfile-")
  renv_lockfile_write(lockfile, file = path)
  actual <- renv_lockfile_read(path)
  expect_identical(unclass(actual), lockfile)

})
