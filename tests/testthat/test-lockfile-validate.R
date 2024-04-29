
test_that("a known-good lockfile passes validation", {

  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      },
      {
        "Name": "BioCsoft",
        "URL": "https://bioconductor.org/packages/3.8/bioc"
      }
    ]
  },
  "Python": {
    "Version": "3.10.12",
    "Type": "virtualenv",
    "Name": "./renv/python/virtualenvs/renv-python-3.10"
  },
  "Bioconductor": {
    "Version": "3.8"
  },
  "Packages": {
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    }
  }
}
'
  expect_no_error(lockfile_validate(lockfile = lockfile))
  expect_true(lockfile_validate(lockfile = lockfile))
})


test_that("a custom schema file can be used for successful validation", {
  # Custom schema adds a required R$Nickname field present in the lockfile
  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Nickname": "Shortstop Beagle",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": {
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    }
  }
}
'

  schema <- '
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "R": {
      "type": "object",
      "properties": {
        "Version": {
          "type": "string"
        },
        "Nickname": {
          "type": "string"
        }
      },
      "required": ["Version", "Nickname"]
    }
  }
}
'
  expect_no_error(lockfile_validate(lockfile = lockfile, lockfile_schema = schema))
  expect_true(lockfile_validate(lockfile = lockfile, lockfile_schema = schema))
})

test_that("a custom schema file can be used for failed validation", {
  # Custom schema adds a required R$Nickname field not present in the lockfile
  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": {
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    }
  }
}
'

  schema <- '
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "R": {
      "type": "object",
      "properties": {
        "Version": {
          "type": "string"
        },
        "Nickname": {
          "type": "string"
        }
      },
      "required": ["Version", "Nickname"]
    }
  }
}
'

  expect_false(lockfile_validate(lockfile = lockfile, lockfile_schema = schema))
})

test_that("an incorrect Packages$Hash field fails validation", {

  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": {
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd"
    }
  }
}
'

  expect_false(lockfile_validate(lockfile = lockfile))
})

test_that("invalid JSON fails validation", {
  # Packages uses [] which is not valid JSON
  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": [
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    }
  ]
}
'
  expect_error(lockfile_validate(lockfile = lockfile, error = TRUE))
})

test_that("strict mode catches unknown keyword in provided schema", {
  # Custom schema provides "Version" with "type": "UNKNOWN"
  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": [
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    }
  ]
}
'

  schema <- '
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "R": {
      "type": "object",
      "properties": {
        "Version": {
          "type": "UNKNOWN"
        }
      }
    }
  }
}
'
  expect_error(lockfile_validate(lockfile = lockfile, strict = TRUE))
})
