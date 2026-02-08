library(testthat)
library(placematchr)

test_that("DE normalization works", {
  expect_equal(normalize_city("M\u00FCnchen", country = "DE"), "muenchen")
  expect_equal(normalize_city("muenchen", country = "DE"), "muenchen")
  expect_equal(normalize_city("Frankfurt am Main", country = "DE"), "frankfurt am main")
  expect_equal(normalize_city("Frankfurt/Main", country = "DE"), "frankfurt am main")
  expect_equal(normalize_city("Berlin (West)", country = "DE"), "berlin")
  expect_equal(normalize_city("Paris", country = "DE"), "paris")
})

test_that("CH normalization works", {
  expect_equal(normalize_city("Z\u00FCrich", country = "CH"), "zuerich")
  expect_equal(normalize_city("Geneve", country = "CH"), "geneve")
  expect_equal(normalize_city("St. Gallen", country = "CH"), "sankt gallen")
  expect_equal(normalize_city("Berlin", country = "CH"), "berlin")
})

test_that("Matching returns results", {
  # This relies on data being present
  res <- match_city("Stuttgart", country = "DE")
  expect_equal(res$city_clean[1], "stuttgart")
  expect_true(!is.na(res$nuts_3_id[1]))

  res_ch <- match_city("Zürich", country = "CH")
  expect_equal(res_ch$city_clean[1], "zuerich")
})
