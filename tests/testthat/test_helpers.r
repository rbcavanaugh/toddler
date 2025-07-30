# Tests for helper functions and internal utilities

test_that(".to_title_case works correctly", {
  # Test basic functionality
  expect_equal(toddler:::.to_title_case("hello world"), "Hello World")
  expect_equal(toddler:::.to_title_case("data science"), "Data Science")
  expect_equal(toddler:::.to_title_case("machine learning algorithms"), "Machine Learning Algorithms")
  
  # Test edge cases
  expect_equal(toddler:::.to_title_case("a"), "A")
  expect_equal(toddler:::.to_title_case(""), "")
  expect_equal(toddler:::.to_title_case("HELLO"), "HELLO")  # Only first letter capitalized
  expect_equal(toddler:::.to_title_case("hello"), "Hello")
  
  # Test with multiple spaces
  expect_equal(toddler:::.to_title_case("hello  world"), "Hello  World")
  
  # Test with special characters (should not affect them)
  expect_equal(toddler:::.to_title_case("hello-world"), "Hello-world")
  expect_equal(toddler:::.to_title_case("test_case"), "Test_case")
})

test_that("package functions handle NULL and missing inputs gracefully", {
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  
  # Test NULL seed (should not error)
  expect_no_error(toddler_missing(test_data, seed = NULL))
  expect_no_error(toddler_duplicate(test_data, seed = NULL))
  expect_no_error(toddler_names(test_data, seed = NULL))
  expect_no_error(toddler_whitespace(test_data, seed = NULL))
  expect_no_error(toddler_inconsistent(test_data, seed = NULL))
  expect_no_error(toddler_types(test_data, seed = NULL))
  expect_no_error(toddler_units(test_data, seed = NULL))
  expect_no_error(toddler_extra(test_data, seed = NULL))
  
  # Test NULL cols parameter where applicable
  expect_no_error(toddler_missing(test_data, cols = NULL))
  expect_no_error(toddler_whitespace(test_data, cols = NULL))
  expect_no_error(toddler_inconsistent(test_data, cols = NULL))
  expect_no_error(toddler_types(test_data, cols = NULL))
  expect_no_error(toddler_units(test_data, cols = NULL))
})

test_that("data frame structure is preserved", {
  # Create test data with row names and attributes
  test_data <- data.frame(
    x = 1:5,
    y = letters[1:5],
    row.names = paste0("row_", 1:5)
  )
  attr(test_data, "custom_attr") <- "test_value"
  
  # Test that basic structure is preserved (though some attributes may be lost)
  result <- toddler_missing(test_data, prop = 0.2)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), ncol(test_data))
  expect_equal(names(result), names(test_data))
  
  # Test with factors
  factor_data <- data.frame(
    x = 1:5,
    f = factor(c("A", "B", "A", "B", "A")),
    stringsAsFactors = FALSE
  )
  
  result_factor <- toddler_missing(factor_data, cols = "x", prop = 0.2)
  expect_s3_class(result_factor, "data.frame")
  expect_true(is.factor(result_factor$f))  # Factor should be preserved
})

test_that("column type detection works correctly", {
  mixed_data <- data.frame(
    int_col = 1:5,
    num_col = c(1.1, 2.2, 3.3, 4.4, 5.5),
    char_col = letters[1:5],
    factor_col = factor(c("A", "B", "A", "B", "A")),
    logical_col = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test functions that should only affect character columns
  whitespace_result <- toddler_whitespace(mixed_data, cols = NULL, prop = 1)
  expect_true(is.numeric(whitespace_result$int_col))      # Should be unchanged
  expect_true(is.numeric(whitespace_result$num_col))      # Should be unchanged
  expect_true(is.character(whitespace_result$char_col))   # May have whitespace
  expect_true(is.factor(whitespace_result$factor_col))    # Should be unchanged
  expect_true(is.logical(whitespace_result$logical_col))  # Should be unchanged
  
  # Test functions that should only affect numeric columns
  types_result <- toddler_types(mixed_data, cols = NULL, prop = 1)
  expect_true(is.character(types_result$int_col))         # Should be converted
  expect_true(is.character(types_result$num_col))         # Should be converted
  expect_true(is.character(types_result$char_col))        # Should be unchanged
  expect_true(is.factor(types_result$factor_col))         # Should be unchanged
  expect_true(is.logical(types_result$logical_col))       # Should be unchanged
  
  units_result <- toddler_units(mixed_data, cols = NULL, prop = 1)
  expect_true(is.character(units_result$int_col))         # Should be converted
  expect_true(is.character(units_result$num_col))         # Should be converted
  expect_true(is.character(units_result$char_col))        # Should be unchanged
  expect_true(is.factor(units_result$factor_col))         # Should be unchanged
  expect_true(is.logical(units_result$logical_col))       # Should be unchanged
})

test_that("random sampling behaves reasonably", {
  test_data <- data.frame(x = 1:100, y = sample(letters, 100, replace = TRUE))
  
  # Test that proportions are approximately correct with large data
  set.seed(12345)
  missing_result <- toddler_missing(test_data, cols = "x", prop = 0.3, seed = 12345)
  missing_count <- sum(is.na(missing_result$x))
  expected_count <- round(100 * 0.3)
  expect_equal(missing_count, expected_count)
  
  set.seed(12345)
  dup_result <- toddler_duplicate(test_data, prop = 0.2, seed = 12345)
  dup_count <- nrow(dup_result) - nrow(test_data)
  expected_dups <- round(100 * 0.2)
  expect_equal(dup_count, expected_dups)
  
  # Test that randomness is actually working (different seeds give different results)
  set.seed(111)
  result1 <- toddler_missing(test_data, prop = 0.5, seed = 111)
  set.seed(222)
  result2 <- toddler_missing(test_data, prop = 0.5, seed = 222)
  
  # With 50% missing on 100 rows, extremely unlikely to be identical
  expect_false(identical(result1, result2))
})