# Comprehensive tests for toddler_missing() function's new capabilities

test_that("toddler_missing handles all parameter combinations correctly", {

    # Clean test data
    clean_data <- data.frame(
        numeric_col = 1:10,
        character_col = letters[1:10],
        factor_col = factor(rep(c("A", "B"), 5)),
        logical_col = rep(c(TRUE, FALSE), 5)
    )

    # Data with existing NAs
    data_with_nas <- clean_data
    data_with_nas$numeric_col[c(2, 5, 8)] <- NA
    data_with_nas$character_col[c(1, 7)] <- NA
    data_with_nas$factor_col[c(3, 9)] <- NA

    # Test 1: Default behavior (add_missing = TRUE, modify_missing = FALSE)
    default_result <- toddler_missing(clean_data, prop = 0.2)
    expect_true(sum(is.na(default_result)) > 0)
    expect_equal(ncol(default_result), ncol(clean_data))

    # Test 2: Only modify existing NAs
    modify_only <- toddler_missing(data_with_nas, add_missing = FALSE, modify_missing = TRUE)
    expect_equal(sum(is.na(modify_only)), 0)  # No NAs should remain

    # Test 3: Both operations
    both_ops <- toddler_missing(clean_data, prop = 0.3, modify_missing = TRUE)
    expect_equal(sum(is.na(both_ops)), 0)  # NAs added then modified away

    # Test 4: Neither operation (should return unchanged data)
    no_ops <- toddler_missing(clean_data, add_missing = FALSE, modify_missing = FALSE)
    expect_equal(no_ops, clean_data)
})

test_that("replacement parameter works correctly", {

    data_with_nas <- data.frame(
        x = c(1, NA, 3, NA, 5),
        y = c("a", "b", NA, "d", NA)
    )

    # Test single replacement value
    single_replacement <- toddler_missing(data_with_nas, add_missing = FALSE,
                                          modify_missing = TRUE, replacement = "UNKNOWN",
                                          random = FALSE)
    expect_true("UNKNOWN" %in% single_replacement$x)
    expect_true("UNKNOWN" %in% single_replacement$y)
    expect_false(any(is.na(single_replacement)))

    # Test multiple replacement values with random = TRUE
    set.seed(123)
    multi_replacement <- toddler_missing(data_with_nas, add_missing = FALSE,
                                         modify_missing = TRUE,
                                         replacement = c("missing", "null", "empty"),
                                         random = TRUE, seed = 123)

    all_values <- c(multi_replacement$x, multi_replacement$y)
    replacement_values <- c("missing", "null", "empty")
    has_replacement <- any(all_values %in% replacement_values)
    expect_true(has_replacement)

    # Test reproducibility with random replacements
    set.seed(456)
    random1 <- toddler_missing(data_with_nas, add_missing = FALSE,
                               modify_missing = TRUE, random = TRUE, seed = 456)
    set.seed(456)
    random2 <- toddler_missing(data_with_nas, add_missing = FALSE,
                               modify_missing = TRUE, random = TRUE, seed = 456)
    expect_equal(random1, random2)
})

test_that("extra_tricky parameter preserves numeric columns", {

    mixed_data <- data.frame(
        integers = c(1L, 2L, NA, 4L),
        doubles = c(1.1, NA, 3.3, 4.4),
        characters = c("a", NA, "c", "d"),
        factors = factor(c("X", "Y", NA, "Z"))
    )

    # Without extra_tricky, all columns become character
    regular_modify <- toddler_missing(mixed_data, add_missing = FALSE, modify_missing = TRUE)
    expect_true(all(sapply(regular_modify, is.character)))

    # With extra_tricky, numeric columns stay numeric
    tricky_modify <- toddler_missing(mixed_data, add_missing = FALSE,
                                     modify_missing = TRUE, extra_tricky = TRUE)

    expect_true(is.numeric(tricky_modify$integers))
    expect_true(is.numeric(tricky_modify$doubles))
    expect_true(is.character(tricky_modify$characters))
    expect_true(is.character(tricky_modify$factors))  # Factors become character

    # Numeric columns should have -999 for former NAs
    expect_true(-999 %in% tricky_modify$integers)
    expect_true(-999 %in% tricky_modify$doubles)

    # Character columns should have messy strings
    messy_values <- c("n/a", "na", "NA", "N/A", "-999", "999", "missing")
    char_values <- tricky_modify$characters
    has_messy_char <- any(char_values %in% messy_values)
    expect_true(has_messy_char)
})

test_that("column selection works with modify_missing", {

    data_with_nas <- data.frame(
        col1 = c(1, NA, 3),
        col2 = c("a", NA, "c"),
        col3 = c(10, NA, 30),
        col4 = c("x", NA, "z")
    )

    # Test specific column selection - expect warning for fewer than 3 NAs
    expect_warning(
        cols_1_3 <- toddler_missing(data_with_nas, cols = c("col1", "col3"),
                                    add_missing = FALSE, modify_missing = TRUE),
        "Less than 3 missing values"
    )

    # col1 and col3 should be modified (no NAs)
    expect_false(any(is.na(cols_1_3$col1)))
    expect_false(any(is.na(cols_1_3$col3)))

    # col2 and col4 should still have NAs
    expect_true(any(is.na(cols_1_3$col2)))
    expect_true(any(is.na(cols_1_3$col4)))

    # Test column indices - expect warning for fewer than 3 NAs
    expect_warning(
        cols_by_index <- toddler_missing(data_with_nas, cols = c(1, 4),
                                         add_missing = FALSE, modify_missing = TRUE),
        "Less than 3 missing values"
    )

    expect_false(any(is.na(cols_by_index$col1)))
    expect_false(any(is.na(cols_by_index$col4)))
    expect_true(any(is.na(cols_by_index$col2)))
    expect_true(any(is.na(cols_by_index$col3)))
})

test_that("error handling works correctly", {

    test_data <- data.frame(x = 1:5, y = letters[1:5])

    # This should still error (from original function)
    expect_error(toddler_missing(test_data, cols = c("x", "y"), prop = c(0.1, 0.2, 0.3)))

    # These should not error
    expect_no_error(toddler_missing(test_data, add_missing = FALSE, modify_missing = FALSE))

    # Data with no NAs should warn but not error
    expect_warning(
        result_no_nas <- toddler_missing(test_data, add_missing = FALSE, modify_missing = TRUE),
        "No missing values found"
    )
    expect_equal(result_no_nas, test_data)  # Should return unchanged

    # Empty data frame should work without error
    empty_df <- data.frame()
    expect_no_error(result1 <- toddler_missing(empty_df, modify_missing = TRUE))
    expect_no_error(result2 <- toddler_missing(empty_df, add_missing = FALSE, modify_missing = TRUE))
    expect_equal(result1, empty_df)
    expect_equal(result2, empty_df)

    # Data frame with no columns
    no_col_df <- data.frame(row.names = 1:3)
    expect_no_error(result3 <- toddler_missing(no_col_df, modify_missing = TRUE))
    expect_equal(result3, no_col_df)

    # Invalid column selection should warn
    expect_warning(
        result4 <- toddler_missing(test_data, cols = c("nonexistent"), modify_missing = TRUE),
        "No valid columns selected"
    )
    expect_equal(result4, test_data)
})

test_that("default messy missing values are realistic", {

    data_with_nas <- data.frame(
        x = c(1, NA, 3, NA, 5, NA, 7),
        y = c("a", NA, "c", NA, "e", NA, "g")
    )

    modified <- toddler_missing(data_with_nas, add_missing = FALSE, modify_missing = TRUE)

    # Check that default replacement values are used
    default_replacements <- c("n/a", "na", "NA", "N/A", "-999", "999", "missing")
    all_values <- c(modified$x, modified$y)

    # Should have some of the default messy values
    has_default_messy <- any(all_values %in% default_replacements)
    expect_true(has_default_messy)

    # Should have no actual NAs
    expect_equal(sum(is.na(modified)), 0)

    # All columns should be character (since extra_tricky = FALSE by default)
    expect_true(is.character(modified$x))
    expect_true(is.character(modified$y))
})

test_that("pipeline operations work correctly", {

    # Test that both operations can be used in a pipeline
    clean_data <- data.frame(
        score = 1:20,
        category = rep(letters[1:4], 5)
    )

    # Pipeline: add NAs, then modify them, then add more NAs
    set.seed(999)
    pipeline_result <- clean_data |>
        toddler_missing(prop = 0.2, seed = 999) |>  # Add some NAs
        toddler_missing(add_missing = FALSE, modify_missing = TRUE, seed = 999) |>  # Make them messy
        toddler_missing(prop = 0.1, seed = 999)  # Add more NAs

    expect_s3_class(pipeline_result, "data.frame")
    expect_equal(nrow(pipeline_result), nrow(clean_data))

    # Should have some NAs (from the final step) and some messy values (from middle step)
    has_nas <- sum(is.na(pipeline_result)) > 0
    expect_true(has_nas)

    # Should have messy missing representations in the data
    all_values <- unlist(pipeline_result)
    messy_values <- c("n/a", "na", "NA", "N/A", "-999", "999", "missing")
    has_messy <- any(all_values %in% messy_values, na.rm = TRUE)
    expect_true(has_messy)
})

test_that("warning messages work correctly", {

    # Test data with few NAs (less than 3) - this will actually generate a warning
    few_nas <- data.frame(x = c(1, NA, 3, 4, 5), y = c("a", "b", "c", "d", "e"))
    expect_warning(
        toddler_missing(few_nas, add_missing = FALSE, modify_missing = TRUE),
        "Less than 3 missing values"
    )

    # Test with prop validation
    expect_error(
        toddler_missing(data.frame(x = 1:5), prop = 1.5),
        "All prop values must be between 0 and 1"
    )

    expect_error(
        toddler_missing(data.frame(x = 1:5), prop = -0.1),
        "All prop values must be between 0 and 1"
    )
})

test_that("edge cases with replacement vectors", {

    data_with_nas <- data.frame(x = c(1, NA, 3, NA, 5), y = c("a", NA, "c", NA, "e"))

    # Empty replacement vector should still work (4 NAs total, so no warning expected)
    expect_no_error(
        result_empty <- toddler_missing(data_with_nas, add_missing = FALSE,
                                        modify_missing = TRUE, replacement = character(0))
    )
    expect_s3_class(result_empty, "data.frame")
    expect_equal(sum(is.na(result_empty)), 0)  # NAs should be replaced with empty strings

    # Single replacement value
    result_single <- toddler_missing(data_with_nas, add_missing = FALSE,
                                     modify_missing = TRUE, replacement = "MISSING")
    expect_true("MISSING" %in% result_single$x)
    expect_true("MISSING" %in% result_single$y)
    expect_equal(sum(is.na(result_single)), 0)

    # Test case that DOES generate the warning (only 2 NAs total)
    data_few_nas <- data.frame(x = c(1, NA, 3, 4, 5), y = c("a", "b", NA, "d", "e"))
    expect_warning(
        result_few <- toddler_missing(data_few_nas, add_missing = FALSE,
                                      modify_missing = TRUE, replacement = "UNKNOWN"),
        "Less than 3 missing values"
    )
    expect_s3_class(result_few, "data.frame")
    expect_equal(sum(is.na(result_few)), 0)
})
