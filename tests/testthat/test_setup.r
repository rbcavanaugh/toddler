# Test setup and helper functions for toddler package tests

# Suppress warnings during testing for cleaner output
options(warn = -1)

# Helper function to create consistent test data
create_test_data <- function(n = 10) {
  data.frame(
    id = 1:n,
    name = sample(letters, n, replace = TRUE),
    value = runif(n, 0, 100),
    category = sample(c("A", "B", "C"), n, replace = TRUE),
    flag = sample(c("yes", "no"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Helper function to check if data frame has expected structure
check_df_structure <- function(df, expected_cols = NULL, expected_rows = NULL) {
  expect_s3_class(df, "data.frame")
  
  if (!is.null(expected_cols)) {
    expect_equal(ncol(df), expected_cols)
  }
  
  if (!is.null(expected_rows)) {
    expect_equal(nrow(df), expected_rows)
  }
  
  # Check that data frame is not completely empty (unless expected)
  if (!is.null(expected_rows) && expected_rows > 0) {
    expect_gt(ncol(df), 0)
  }
}

# Helper to check reproducibility
check_reproducibility <- function(func, data, ...) {
  set.seed(12345)
  result1 <- func(data, seed = 12345, ...)
  set.seed(12345)
  result2 <- func(data, seed = 12345, ...)
  
  expect_equal(result1, result2, 
               info = paste("Function", deparse(substitute(func)), "is not reproducible"))
}

# Helper to test that functions don't completely destroy data
check_data_integrity <- function(original, modified, allow_extra_rows = FALSE) {
  # Should still be a data frame
  expect_s3_class(modified, "data.frame")
  
  # Should have same or more rows (if duplicates allowed)
  if (allow_extra_rows) {
    expect_gte(nrow(modified), nrow(original))
  } else {
    expect_equal(nrow(modified), nrow(original))
  }
  
  # Should have same number of columns (column names may change)
  expect_equal(ncol(modified), ncol(original))
  
  # Should not be completely empty (unless original was empty)
  if (nrow(original) > 0 && ncol(original) > 0) {
    expect_gt(sum(!is.na(modified)), 0)
  }
}