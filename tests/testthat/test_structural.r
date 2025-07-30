# Tests for structural functions: toddler_wide, toddler_unjoin, toddler_extra

test_that("toddler_wide works correctly", {
    # Create test data
    tidy_data <- data.frame(
        id = rep(1:3, each = 2),
        measure = rep(c("height", "weight"), 3),
        value = c(170, 70, 165, 65, 180, 80)
    )

    # Test basic functionality
    wide_data <- toddler_wide(tidy_data, "measure", "value", "id")

    expect_s3_class(wide_data, "data.frame")
    expect_equal(nrow(wide_data), 3)
    expect_true("value_height" %in% names(wide_data))
    expect_true("value_weight" %in% names(wide_data))
    expect_equal(wide_data$value_height, c(170, 165, 180))

    # Test with automatic id_cols detection
    wide_data2 <- toddler_wide(tidy_data, "measure", "value")
    expect_equal(wide_data, wide_data2)

    # Test with multiple id columns
    tidy_data$group <- rep(c("A", "B"), length.out = nrow(tidy_data))
    wide_data3 <- toddler_wide(tidy_data, "measure", "value", c("id", "group"))
    expect_true("group" %in% names(wide_data3))
})

test_that("toddler_unjoin splits data correctly", {
    test_data <- data.frame(
        id = 1:10,
        name = letters[1:10],
        value1 = runif(10),
        value2 = runif(10)
    )

    # Test row splitting
    set.seed(123)
    split_rows <- toddler_unjoin(test_data, by_rows = "id", prop = 0.7, seed = 123)

    expect_type(split_rows, "list")
    expect_length(split_rows, 2)
    expect_named(split_rows, c("data1", "data2"))
    expect_equal(nrow(split_rows$data1) + nrow(split_rows$data2), nrow(test_data))

    # Test column splitting
    split_cols <- toddler_unjoin(test_data, by_cols = TRUE,
                                 id_cols = "id", cols1 = c("name", "value1"))

    expect_type(split_cols, "list")
    expect_length(split_cols, 2)
    expect_true("id" %in% names(split_cols$data1))
    expect_true("id" %in% names(split_cols$data2))
    expect_true("name" %in% names(split_cols$data1))
    expect_true("value2" %in% names(split_cols$data2))
    expect_false("value2" %in% names(split_cols$data1))

    # Test group splitting
    test_data$group <- rep(c("A", "B", "C"), length.out = 10)
    split_groups <- toddler_unjoin(test_data, by_groups = "group")

    expect_type(split_groups, "list")
    expect_length(split_groups, 3)
    expect_named(split_groups, c("group_A", "group_B", "group_C"))

    # Test error conditions
    expect_error(toddler_unjoin(test_data, by_cols = TRUE, id_cols = "id"))
    expect_error(toddler_unjoin(test_data, by_cols = TRUE, cols1 = "name"))

    # Test no splitting returns original data
    no_split <- toddler_unjoin(test_data)
    expect_equal(no_split, test_data)
})

test_that("toddler_extra adds bonus content", {
    test_data <- data.frame(
        name = c("Alice", "Bob"),
        score = c(85, 92),
        age = c(25, 30)
    )

    # Test adding all extras
    set.seed(123)
    extra_data <- toddler_extra(test_data, add_totals = TRUE,
                                add_empty = TRUE, add_header = TRUE, seed = 123)

    expect_s3_class(extra_data, "data.frame")
    expect_gt(nrow(extra_data), nrow(test_data))

    # Check for header row
    expect_true(any(grepl("Data Report", extra_data[, 1])))

    # Check for totals row
    expect_true(any(grepl("TOTAL", extra_data[, 1])))

    # Test individual options
    header_only <- toddler_extra(test_data, add_totals = FALSE,
                                 add_empty = FALSE, add_header = TRUE)
    expect_equal(nrow(header_only), nrow(test_data) + 1)

    totals_only <- toddler_extra(test_data, add_totals = TRUE,
                                 add_empty = FALSE, add_header = FALSE)
    expect_equal(nrow(totals_only), nrow(test_data) + 1)

    # Test with no additions
    no_extras <- toddler_extra(test_data, add_totals = FALSE,
                               add_empty = FALSE, add_header = FALSE)
    expect_equal(no_extras, test_data)
})

test_that("structural functions handle edge cases", {
    # Test with single row
    single_row <- data.frame(id = 1, value = 10)
    expect_s3_class(toddler_extra(single_row), "data.frame")

    # Test with empty data frame
    empty_df <- data.frame(id = integer(0), value = numeric(0))
    expect_s3_class(toddler_extra(empty_df), "data.frame")

    # Test unjoin with single row
    single_split <- toddler_unjoin(single_row, by_rows = "id", prop = 0.5)
    expect_type(single_split, "list")
})
