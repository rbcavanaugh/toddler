# Tests for data quality functions: toddler_missing, toddler_duplicate, toddler_types, toddler_units

test_that("toddler_missing adds NAs correctly", {
    test_data <- data.frame(
        x = 1:10,
        y = letters[1:10],
        z = runif(10)
    )

    # Test basic functionality (adding NAs)
    set.seed(123)
    missing_data <- toddler_missing(test_data, prop = 0.3, seed = 123)

    expect_s3_class(missing_data, "data.frame")
    expect_equal(nrow(missing_data), nrow(test_data))
    expect_equal(ncol(missing_data), ncol(test_data))

    # Should have some missing values
    total_nas <- sum(is.na(missing_data))
    expect_gt(total_nas, 0)

    # Test specific columns
    x_only <- toddler_missing(test_data, cols = "x", prop = 0.5, seed = 123)
    expect_true(sum(is.na(x_only$x)) > 0)
    expect_false(any(is.na(x_only$y)))  # Other columns unchanged

    # Test column indices
    cols_by_index <- toddler_missing(test_data, cols = 1:2, prop = 0.3, seed = 123)
    expect_s3_class(cols_by_index, "data.frame")

    # Test different proportions for different columns
    diff_props <- toddler_missing(test_data, cols = c("x", "y"),
                                  prop = c(0.1, 0.5), seed = 123)
    expect_s3_class(diff_props, "data.frame")

    # Test error condition
    expect_error(toddler_missing(test_data, cols = c("x", "y"), prop = c(0.1, 0.2, 0.3)))

    # Test reproducibility
    set.seed(123)
    miss1 <- toddler_missing(test_data, seed = 123)
    set.seed(123)
    miss2 <- toddler_missing(test_data, seed = 123)
    expect_equal(miss1, miss2)

    # Test with proportion = 0
    no_missing <- toddler_missing(test_data, prop = 0)
    expect_equal(sum(is.na(no_missing)), 0)
})

test_that("toddler_missing modifies existing NAs correctly", {
    # Test data with existing NAs
    test_data_with_na <- data.frame(
        x = c(1, 2, NA, 4, NA),
        y = c("a", NA, "c", "d", NA),
        z = c(1.1, 2.2, NA, 4.4, 5.5)
    )

    # Test modify_missing only (no new NAs)
    set.seed(123)
    modified_data <- toddler_missing(test_data_with_na, add_missing = FALSE,
                                     modify_missing = TRUE, seed = 123)

    expect_s3_class(modified_data, "data.frame")
    expect_equal(nrow(modified_data), nrow(test_data_with_na))
    expect_equal(ncol(modified_data), ncol(test_data_with_na))

    # Should have no actual NAs anymore
    expect_equal(sum(is.na(modified_data)), 0)

    # Columns should be converted to character (except with extra_tricky)
    expect_true(is.character(modified_data$x))
    expect_true(is.character(modified_data$y))
    expect_true(is.character(modified_data$z))

    # Test extra_tricky option (keep numeric columns numeric)
    set.seed(123)
    tricky_data <- toddler_missing(test_data_with_na, add_missing = FALSE,
                                   modify_missing = TRUE, extra_tricky = TRUE, seed = 123)

    expect_true(is.numeric(tricky_data$x))  # Should stay numeric
    expect_true(is.numeric(tricky_data$z))  # Should stay numeric
    expect_true(is.character(tricky_data$y))  # Was character, gets messy values

    # Numeric columns should have -999 instead of messy strings
    expect_true(-999 %in% tricky_data$x)
    expect_true(-999 %in% tricky_data$z)

    # Test specific replacement value
    specific_data <- toddler_missing(test_data_with_na, add_missing = FALSE,
                                     modify_missing = TRUE, replacement = "MISSING",
                                     random = FALSE, seed = 123)

    # All former NAs should be "MISSING"
    expect_true(all(specific_data$x %in% c("1", "2", "MISSING", "4")))
    expect_true("MISSING" %in% specific_data$y)

    # Test reproducibility
    set.seed(456)
    mod1 <- toddler_missing(test_data_with_na, add_missing = FALSE,
                            modify_missing = TRUE, seed = 456)
    set.seed(456)
    mod2 <- toddler_missing(test_data_with_na, add_missing = FALSE,
                            modify_missing = TRUE, seed = 456)
    expect_equal(mod1, mod2)
})

test_that("toddler_missing can do both operations", {
    test_data <- data.frame(
        x = 1:5,
        y = letters[1:5]
    )

    # Test both add_missing and modify_missing
    set.seed(789)
    both_operations <- toddler_missing(test_data, prop = 0.4, modify_missing = TRUE, seed = 789)

    expect_s3_class(both_operations, "data.frame")
    expect_equal(nrow(both_operations), nrow(test_data))

    # Should have no NAs (they were added then modified)
    expect_equal(sum(is.na(both_operations)), 0)

    # Columns should be character due to messy missing values
    expect_true(is.character(both_operations$x))
    expect_true(is.character(both_operations$y))

    # Should contain some messy missing value representations
    all_values <- c(both_operations$x, both_operations$y)
    messy_values <- c("n/a", "na", "NA", "N/A", "-999", "999", "missing")
    has_messy <- any(all_values %in% messy_values)
    expect_true(has_messy)
})

test_that("toddler_duplicate adds duplicate rows", {
    test_data <- data.frame(
        id = 1:5,
        value = letters[1:5]
    )

    # Test basic functionality
    set.seed(123)
    dup_data <- toddler_duplicate(test_data, prop = 0.4, seed = 123)

    expect_s3_class(dup_data, "data.frame")
    expect_gt(nrow(dup_data), nrow(test_data))
    expect_equal(ncol(dup_data), ncol(test_data))

    # Check that duplicates actually exist
    original_rows <- nrow(test_data)
    new_rows <- nrow(dup_data)
    expected_dups <- round(original_rows * 0.4)
    expect_equal(new_rows, original_rows + expected_dups)

    # Test with proportion = 0
    no_dups <- toddler_duplicate(test_data, prop = 0)
    expect_equal(no_dups, test_data)

    # Test reproducibility
    set.seed(123)
    dup1 <- toddler_duplicate(test_data, seed = 123)
    set.seed(123)
    dup2 <- toddler_duplicate(test_data, seed = 123)
    expect_equal(dup1, dup2)

    # Test with single row
    single_row <- data.frame(x = 1)
    dup_single <- toddler_duplicate(single_row, prop = 1)
    expect_equal(nrow(dup_single), 2)
})

test_that("toddler_types mixes data types", {
    test_data <- data.frame(
        age = c(25, 30, 35, 40),
        score = c(85.5, 92.1, 78.9, 88.2),
        name = c("Alice", "Bob", "Charlie", "Diana")
    )

    # Test basic functionality
    set.seed(123)
    mixed_types <- toddler_types(test_data, prop = 0.5, seed = 123)

    expect_s3_class(mixed_types, "data.frame")
    expect_equal(nrow(mixed_types), nrow(test_data))
    expect_equal(ncol(mixed_types), ncol(test_data))

    # Numeric columns should now be character
    expect_true(is.character(mixed_types$age))
    expect_true(is.character(mixed_types$score))

    # Should contain some of the mess values
    all_numeric_values <- c(mixed_types$age, mixed_types$score)
    mess_values <- c("N/A", "missing", ".", "-", "NULL")
    has_mess <- any(all_numeric_values %in% mess_values)
    expect_true(has_mess)

    # Test specific columns
    age_only <- toddler_types(test_data, cols = "age", prop = 0.5, seed = 123)
    expect_true(is.character(age_only$age))
    expect_true(is.numeric(age_only$score))  # Should be unchanged

    # Test with no numeric columns
    char_only <- data.frame(x = letters[1:5], y = LETTERS[1:5])
    mixed_char <- toddler_types(char_only)
    expect_equal(mixed_char, char_only)  # Should be unchanged

    # Test reproducibility
    set.seed(123)
    types1 <- toddler_types(test_data, seed = 123)
    set.seed(123)
    types2 <- toddler_types(test_data, seed = 123)
    expect_equal(types1, types2)
})

test_that("toddler_units adds units to numbers", {
    test_data <- data.frame(
        height = c(170, 165, 180, 175),
        weight = c(70, 65, 80, 75),
        name = c("Alice", "Bob", "Charlie", "Diana")
    )

    # Test basic functionality
    set.seed(123)
    units_data <- toddler_units(test_data, prop = 0.5, seed = 123)

    expect_s3_class(units_data, "data.frame")
    expect_equal(nrow(units_data), nrow(test_data))
    expect_equal(ncol(units_data), ncol(test_data))

    # Numeric columns should now be character
    expect_true(is.character(units_data$height))
    expect_true(is.character(units_data$weight))
    expect_true(is.character(units_data$name))  # Was already character

    # Should contain some units
    all_numeric_values <- c(units_data$height, units_data$weight)
    default_units <- c("kg", "lbs", "cm", "in", "sec", "min")
    has_units <- any(sapply(default_units, function(unit) {
        any(grepl(unit, all_numeric_values, fixed = TRUE))
    }))
    expect_true(has_units)

    # Test custom units
    custom_units <- toddler_units(test_data, units = c("meters", "pounds"),
                                  prop = 1, seed = 123)
    all_values <- c(custom_units$height, custom_units$weight)
    has_custom <- any(grepl("meters|pounds", all_values))
    expect_true(has_custom)

    # Test specific columns
    height_only <- toddler_units(test_data, cols = "height", prop = 1, seed = 123)
    expect_true(is.character(height_only$height))
    expect_true(is.numeric(height_only$weight))  # Should be unchanged

    # Test with no numeric columns
    char_only <- data.frame(x = letters[1:5], y = LETTERS[1:5])
    units_char <- toddler_units(char_only)
    expect_equal(units_char, char_only)  # Should be unchanged

    # Test reproducibility
    set.seed(123)
    units1 <- toddler_units(test_data, seed = 123)
    set.seed(123)
    units2 <- toddler_units(test_data, seed = 123)
    expect_equal(units1, units2)

    # Test with proportion = 0 (should be unchanged)
    no_units <- toddler_units(test_data, prop = 0)
    expect_equal(no_units$height, test_data$height)  # Should be exactly equal
    expect_equal(no_units$weight, test_data$weight)  # Should be exactly equal
    expect_equal(no_units$name, test_data$name)      # Should be exactly equal
})

test_that("data quality functions handle edge cases", {
    # Test with empty data frame
    empty_df <- data.frame()
    expect_s3_class(toddler_missing(empty_df), "data.frame")
    expect_s3_class(toddler_duplicate(empty_df), "data.frame")

    # Test with single row
    single_row <- data.frame(x = 1, y = "a")
    expect_s3_class(toddler_missing(single_row), "data.frame")
    expect_s3_class(toddler_duplicate(single_row), "data.frame")
    expect_s3_class(toddler_types(single_row), "data.frame")
    expect_s3_class(toddler_units(single_row), "data.frame")

    # Test toddler_missing with data that has no NAs when modify_missing = TRUE
    no_na_data <- data.frame(x = 1:5, y = letters[1:5])
    expect_warning(
        no_change <- toddler_missing(no_na_data, add_missing = FALSE, modify_missing = TRUE),
        "No missing values found in selected columns"
    )
    expect_equal(no_change, no_na_data)  # Should be unchanged

    # Test with all NA data
    na_data <- data.frame(x = rep(NA, 5), y = rep(NA_character_, 5))
    expect_s3_class(toddler_missing(na_data), "data.frame")
    expect_s3_class(toddler_duplicate(na_data), "data.frame")

    # Test toddler_missing modify on all-NA data
    all_na_modified <- toddler_missing(na_data, add_missing = FALSE, modify_missing = TRUE)
    expect_equal(sum(is.na(all_na_modified)), 0)  # Should have no NAs after modification

    # Test missing with non-existent columns
    test_data <- data.frame(x = 1:5)
    missing_nonexistent <- toddler_missing(test_data, cols = c("x", "nonexistent"))
    expect_s3_class(missing_nonexistent, "data.frame")

    # Test with extreme proportions
    expect_s3_class(toddler_missing(test_data, prop = 0), "data.frame")
    expect_s3_class(toddler_missing(test_data, prop = 1), "data.frame")
    expect_s3_class(toddler_duplicate(test_data, prop = 0), "data.frame")
    expect_s3_class(toddler_duplicate(test_data, prop = 2), "data.frame")

    # Test toddler_missing with minimal NAs (should warn about < 3 missing values)
    data_with_na <- data.frame(x = c(1, NA, 3))
    expect_warning(
        result_few_nas <- toddler_missing(data_with_na, add_missing = FALSE, modify_missing = TRUE),
        "Less than 3 missing values found"
    )
    expect_s3_class(result_few_nas, "data.frame")
    expect_equal(sum(is.na(result_few_nas)), 0)  # Should have no NAs after modification
})
