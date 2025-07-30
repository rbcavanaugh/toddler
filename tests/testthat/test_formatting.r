# Tests for formatting functions: toddler_names, toddler_whitespace, toddler_inconsistent

test_that("toddler_names messes up column names appropriately", {
    test_data <- data.frame(
        first_name = "Alice",
        last_name = "Smith",
        age_years = 25,
        test_score = 95
    )

    original_names <- names(test_data)

    # Test mixed style (default)
    set.seed(123)
    mixed_names <- toddler_names(test_data, style = "mixed", seed = 123)
    expect_s3_class(mixed_names, "data.frame")
    expect_equal(ncol(mixed_names), ncol(test_data))

    # Names should be different (with high probability)
    set.seed(123)
    mixed_names2 <- toddler_names(test_data, style = "mixed", seed = 123)
    expect_equal(names(mixed_names), names(mixed_names2))  # Reproducible

    # Test title case
    title_names <- toddler_names(test_data, style = "title")
    expect_true(any(grepl(" ", names(title_names))))  # Should have spaces
    expect_true(any(grepl("^[A-Z]", names(title_names))))  # Should start with capital

    # Test upper case
    upper_names <- toddler_names(test_data, style = "upper")
    expect_true(all(names(upper_names) == toupper(original_names)))

    # Test lower case
    lower_names <- toddler_names(test_data, style = "lower")
    expect_true(all(names(lower_names) == tolower(original_names)))

    # Test data integrity
    expect_equal(mixed_names[, 1], test_data[, 1])  # Data should be unchanged
})

test_that("toddler_whitespace adds spaces correctly", {
    test_data <- data.frame(
        name = c("Alice", "Bob", "Charlie"),
        city = c("New York", "Boston", "Chicago"),
        stringsAsFactors = FALSE
    )

    # Test with default settings
    set.seed(123)
    spaced_data <- toddler_whitespace(test_data, prop = 0.5, seed = 123)

    expect_s3_class(spaced_data, "data.frame")
    expect_equal(nrow(spaced_data), nrow(test_data))
    expect_equal(ncol(spaced_data), ncol(test_data))

    # Some values should have extra spaces
    all_values <- unlist(spaced_data[sapply(spaced_data, is.character)])
    has_leading <- any(grepl("^\\s+", all_values))
    has_trailing <- any(grepl("\\s+$", all_values))
    expect_true(has_leading || has_trailing)

    # Test specific columns
    name_only <- toddler_whitespace(test_data, cols = "name", prop = 1, seed = 123)
    expect_true(all(name_only$city == test_data$city))  # City should be unchanged

    # Test with numeric data (should be ignored)
    numeric_data <- data.frame(x = 1:5, y = letters[1:5])
    spaced_numeric <- toddler_whitespace(numeric_data, prop = 1)
    expect_equal(spaced_numeric$x, numeric_data$x)  # Numeric unchanged

    # Test reproducibility
    set.seed(123)
    spaced1 <- toddler_whitespace(test_data, seed = 123)
    set.seed(123)
    spaced2 <- toddler_whitespace(test_data, seed = 123)
    expect_equal(spaced1, spaced2)
})

test_that("toddler_inconsistent creates inconsistencies", {
    test_data <- data.frame(
        answer = rep(c("yes", "no"), 5),
        gender = rep(c("male", "female"), 5),
        status = rep(c("true", "false"), 5),
        other = rep("test", 10),
        stringsAsFactors = FALSE
    )

    # Test basic functionality
    set.seed(123)
    inconsistent_data <- toddler_inconsistent(test_data, seed = 123)

    expect_s3_class(inconsistent_data, "data.frame")
    expect_equal(nrow(inconsistent_data), nrow(test_data))
    expect_equal(ncol(inconsistent_data), ncol(test_data))

    # Test specific columns
    answer_only <- toddler_inconsistent(test_data, cols = "answer", seed = 123)

    # Test known transformations
    yes_variations <- c("YES", "y", "True", "yes")
    no_variations <- c("NO", "n", "False", "no")
    male_variations <- c("Male", "MALE", "M", "male")
    female_variations <- c("Female", "FEMALE", "F", "female")

    # With high probability and enough data, we should see some variations
    set.seed(123)
    large_data <- data.frame(
        answer = rep("yes", 100),
        stringsAsFactors = FALSE
    )
    inconsistent_large <- toddler_inconsistent(large_data, seed = 123)
    unique_answers <- unique(inconsistent_large$answer)

    # Should have some variety (not all exactly "yes")
    expect_true(length(unique_answers) >= 1)

    # Test with non-character data (should be ignored)
    mixed_data <- data.frame(x = 1:5, y = c("yes", "no", "yes", "no", "yes"))
    inconsistent_mixed <- toddler_inconsistent(mixed_data)
    expect_equal(inconsistent_mixed$x, mixed_data$x)  # Numeric unchanged

    # Test reproducibility
    set.seed(123)
    inc1 <- toddler_inconsistent(test_data, seed = 123)
    set.seed(123)
    inc2 <- toddler_inconsistent(test_data, seed = 123)
    expect_equal(inc1, inc2)
})

test_that("formatting functions handle edge cases", {
    # Test with empty data frame
    empty_df <- data.frame()
    expect_s3_class(toddler_names(empty_df), "data.frame")

    # Test with single column
    single_col <- data.frame(x = c("a", "b", "c"))
    expect_s3_class(toddler_whitespace(single_col), "data.frame")
    expect_s3_class(toddler_inconsistent(single_col), "data.frame")

    # Test with no character columns
    numeric_only <- data.frame(x = 1:5, y = 6:10)
    spaced_numeric <- toddler_whitespace(numeric_only)
    expect_equal(spaced_numeric, numeric_only)  # Should be unchanged

    inconsistent_numeric <- toddler_inconsistent(numeric_only)
    expect_equal(inconsistent_numeric, numeric_only)  # Should be unchanged

    # Test with missing values
    with_na <- data.frame(x = c("yes", NA, "no"), y = c(1, 2, 3))
    expect_s3_class(toddler_inconsistent(with_na), "data.frame")
    expect_s3_class(toddler_whitespace(with_na), "data.frame")

    # Test with zero proportion
    no_change <- toddler_whitespace(single_col, prop = 0)
    expect_equal(no_change, single_col)
})
