# Integration tests and pipeline tests

test_that("functions work together in pipeline", {
    # Create comprehensive test data
    clean_data <- data.frame(
        participant_id = 1:20,
        age_group = rep(c("young", "old"), 10),
        test_score = runif(20, 50, 100),
        completed = rep(c("yes", "no"), 10),
        gender = rep(c("male", "female"), 10),
        stringsAsFactors = FALSE
    )

    # Test complete pipeline
    set.seed(42)
    messy_data <- clean_data |>
        toddler_missing(prop = 0.1, seed = 42) |>
        toddler_inconsistent(cols = "completed", seed = 42) |>
        toddler_whitespace(cols = "age_group", prop = 0.2, seed = 42) |>
        toddler_types(cols = "test_score", prop = 0.05, seed = 42) |>
        toddler_duplicate(prop = 0.1, seed = 42) |>
        toddler_names(style = "mixed", seed = 42)

    expect_s3_class(messy_data, "data.frame")
    expect_gt(nrow(messy_data), nrow(clean_data))  # Should have duplicates
    expect_equal(ncol(messy_data), ncol(clean_data))  # Same number of columns

    # Should have some NAs
    expect_gt(sum(is.na(messy_data)), 0)

    # Column names should be different
    expect_false(identical(names(messy_data), names(clean_data)))

    # Test reproducibility of pipeline
    set.seed(42)
    messy_data2 <- clean_data |>
        toddler_missing(prop = 0.1, seed = 42) |>
        toddler_inconsistent(cols = "completed", seed = 42) |>
        toddler_whitespace(cols = "age_group", prop = 0.2, seed = 42) |>
        toddler_types(cols = "test_score", prop = 0.05, seed = 42) |>
        toddler_duplicate(prop = 0.1, seed = 42) |>
        toddler_names(style = "mixed", seed = 42)

    expect_equal(messy_data, messy_data2)
})

test_that("pipeline with splitting works", {
    test_data <- data.frame(
        id = 1:10,
        group = rep(c("A", "B"), 5),
        value1 = runif(10),
        value2 = runif(10),
        category = rep(c("yes", "no"), 5)
    )

    # Test row splitting pipeline
    set.seed(123)
    split_messy <- test_data |>
        toddler_missing(prop = 0.1, seed = 123) |>
        toddler_inconsistent(cols = "category", seed = 123) |>
        toddler_unjoin(by_rows = "id", prop = 0.7, seed = 123)

    expect_type(split_messy, "list")
    expect_length(split_messy, 2)
    expect_named(split_messy, c("data1", "data2"))

    # Both pieces should be data frames
    expect_s3_class(split_messy$data1, "data.frame")
    expect_s3_class(split_messy$data2, "data.frame")

    # Total rows should equal original (plus any duplicates from missing step)
    total_rows <- nrow(split_messy$data1) + nrow(split_messy$data2)
    expect_gte(total_rows, nrow(test_data))

    # Test column splitting pipeline
    set.seed(123)
    named_data <- test_data |>
        toddler_names(style = "title", seed = 123)

    # Get the new column names after transformation
    new_names <- names(named_data)
    id_col <- new_names[1]  # Should be "id" transformed
    group_col <- new_names[2]  # Should be "group" transformed
    value1_col <- new_names[3]  # Should be "value1" transformed

    col_split_messy <- named_data |>
        toddler_unjoin(by_cols = TRUE, id_cols = id_col,
                       cols1 = c(group_col, value1_col), seed = 123)

    expect_type(col_split_messy, "list")
    expect_length(col_split_messy, 2)
    expect_true(id_col %in% names(col_split_messy$data1))
    expect_true(id_col %in% names(col_split_messy$data2))
})

test_that("functions handle data frame with all data types", {
    complex_data <- data.frame(
        int_col = 1:5,
        num_col = c(1.1, 2.2, 3.3, 4.4, 5.5),
        char_col = letters[1:5],
        factor_col = factor(c("A", "B", "A", "B", "A")),
        logical_col = c(TRUE, FALSE, TRUE, FALSE, TRUE),
        stringsAsFactors = FALSE
    )

    # Test each function on complex data
    expect_s3_class(toddler_missing(complex_data), "data.frame")
    expect_s3_class(toddler_duplicate(complex_data), "data.frame")
    expect_s3_class(toddler_names(complex_data), "data.frame")
    expect_s3_class(toddler_whitespace(complex_data), "data.frame")
    expect_s3_class(toddler_inconsistent(complex_data), "data.frame")
    expect_s3_class(toddler_types(complex_data), "data.frame")
    expect_s3_class(toddler_units(complex_data), "data.frame")
    expect_s3_class(toddler_extra(complex_data), "data.frame")

    # Test pipeline on complex data
    set.seed(456)
    complex_messy <- complex_data |>
        toddler_missing(prop = 0.2, seed = 456) |>
        toddler_duplicate(prop = 0.2, seed = 456) |>
        toddler_names(style = "mixed", seed = 456)

    expect_s3_class(complex_messy, "data.frame")
    expect_equal(ncol(complex_messy), ncol(complex_data))
})

test_that("extreme parameter values are handled gracefully", {
    test_data <- data.frame(x = 1:10, y = letters[1:10])

    # Test extreme proportions
    expect_s3_class(toddler_missing(test_data, prop = 0), "data.frame")
    expect_s3_class(toddler_missing(test_data, prop = 1), "data.frame")
    expect_s3_class(toddler_duplicate(test_data, prop = 0), "data.frame")
    expect_s3_class(toddler_duplicate(test_data, prop = 5), "data.frame")  # 500% duplicates
    expect_s3_class(toddler_whitespace(test_data, prop = 0), "data.frame")
    expect_s3_class(toddler_whitespace(test_data, prop = 1), "data.frame")

    # Very large data
    large_data <- data.frame(
        x = 1:1000,
        y = sample(letters, 1000, replace = TRUE)
    )

    expect_s3_class(toddler_missing(large_data, prop = 0.1), "data.frame")
    expect_s3_class(toddler_duplicate(large_data, prop = 0.1), "data.frame")
})

test_that("error conditions are handled appropriately", {
    test_data <- data.frame(x = 1:5, y = letters[1:5])

    # Test invalid column specifications
    expect_error(toddler_missing(test_data, cols = c("x", "y"), prop = c(0.1, 0.2, 0.3)))
    expect_error(toddler_unjoin(test_data, by_cols = TRUE, id_cols = "x"))  # Missing cols1
    expect_error(toddler_unjoin(test_data, by_cols = TRUE, cols1 = "y"))    # Missing id_cols

    # Functions should not error on empty inputs (except where logically impossible)
    empty_df <- data.frame()
    expect_no_error(toddler_missing(empty_df))
    expect_no_error(toddler_duplicate(empty_df))
    expect_no_error(toddler_names(empty_df))
    expect_no_error(toddler_whitespace(empty_df))
    expect_no_error(toddler_inconsistent(empty_df))
    expect_no_error(toddler_types(empty_df))
    expect_no_error(toddler_units(empty_df))
    expect_no_error(toddler_extra(empty_df))
})

test_that("seed functionality works consistently", {
    test_data <- data.frame(
        x = 1:10,
        y = sample(letters[1:5], 10, replace = TRUE)
    )

    # Test that same seed produces same results
    set.seed(999)
    result1 <- toddler_missing(test_data, seed = 999)
    set.seed(999)
    result2 <- toddler_missing(test_data, seed = 999)
    expect_equal(result1, result2)

    # Test that different seeds produce different results (with high probability)
    set.seed(999)
    result_a <- toddler_missing(test_data, prop = 0.5, seed = 999)
    set.seed(888)
    result_b <- toddler_missing(test_data, prop = 0.5, seed = 888)

    # With 50% missing and 10 rows, very unlikely to be identical
    expect_false(identical(result_a, result_b))

    # Test seed inheritance in pipeline
    set.seed(777)
    pipeline1 <- test_data |>
        toddler_missing(prop = 0.2, seed = 777) |>
        toddler_duplicate(prop = 0.2, seed = 777)

    set.seed(777)
    pipeline2 <- test_data |>
        toddler_missing(prop = 0.2, seed = 777) |>
        toddler_duplicate(prop = 0.2, seed = 777)

    expect_equal(pipeline1, pipeline2)
})
