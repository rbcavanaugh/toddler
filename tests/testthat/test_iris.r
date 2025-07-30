# Tests using the iris dataset - realistic data scenarios

test_that("toddler functions work on iris dataset", {

    # Test each function individually on iris
    expect_s3_class(toddler_missing(iris, prop = 0.1), "data.frame")
    expect_s3_class(toddler_duplicate(iris, prop = 0.1), "data.frame")
    expect_s3_class(toddler_names(iris, style = "mixed"), "data.frame")
    expect_s3_class(toddler_whitespace(iris), "data.frame")
    expect_s3_class(toddler_inconsistent(iris, prop = 0.1), "data.frame")
    expect_s3_class(toddler_types(iris, prop = 0.05), "data.frame")
    expect_s3_class(toddler_units(iris, prop = 0.1), "data.frame")
    expect_s3_class(toddler_extra(iris), "data.frame")

    # Test new toddler_missing functionality on iris
    iris_with_na <- iris
    iris_with_na$Sepal.Length[c(1, 5, 10)] <- NA
    iris_with_na$Species[c(2, 7)] <- NA

    # Test modify_missing on iris
    modified_iris <- toddler_missing(iris_with_na, add_missing = FALSE, modify_missing = TRUE)
    expect_equal(sum(is.na(modified_iris)), 0)  # No more NAs

    # Test extra_tricky on iris (numeric columns stay numeric)
    tricky_iris <- toddler_missing(iris_with_na, add_missing = FALSE,
                                   modify_missing = TRUE, extra_tricky = TRUE)
    expect_true(is.numeric(tricky_iris$Sepal.Length))  # Should stay numeric with -999
    expect_true(-999 %in% tricky_iris$Sepal.Length)

    # Test that data integrity is maintained
    missing_iris <- toddler_missing(iris, prop = 0.2)
    expect_equal(ncol(missing_iris), ncol(iris))
    expect_equal(nrow(missing_iris), nrow(iris))
    expect_gt(sum(is.na(missing_iris)), sum(is.na(iris)))  # More NAs than original

    # Test duplicates actually add rows
    dup_iris <- toddler_duplicate(iris, prop = 0.3)
    expect_gt(nrow(dup_iris), nrow(iris))
    expect_equal(ncol(dup_iris), ncol(iris))

    # Test names are actually changed
    set.seed(123)
    named_iris <- toddler_names(iris, style = "mixed", seed = 123)
    expect_false(identical(names(named_iris), names(iris)))
    expect_equal(ncol(named_iris), ncol(iris))
    expect_equal(nrow(named_iris), nrow(iris))
})

test_that("iris factor column (Species) is handled correctly", {

    # Test that Species factor column is handled properly
    expect_true(is.factor(iris$Species))

    # whitespace should only affect character columns, not factors
    whitespace_iris <- toddler_whitespace(iris, prop = 1)
    expect_true(is.factor(whitespace_iris$Species))  # Should remain factor
    expect_equal(levels(whitespace_iris$Species), levels(iris$Species))

    # inconsistent should only affect character columns, not factors
    inconsistent_iris <- toddler_inconsistent(iris, prop = 0.5)
    expect_true(is.factor(inconsistent_iris$Species))  # Should remain factor

    # types should only affect numeric columns
    types_iris <- toddler_types(iris, prop = 0.5)
    expect_true(is.factor(types_iris$Species))  # Should remain factor
    expect_true(is.character(types_iris$Sepal.Length))  # Should be converted

    # units should only affect numeric columns
    units_iris <- toddler_units(iris, prop = 0.5)
    expect_true(is.factor(units_iris$Species))  # Should remain factor
    expect_true(is.character(units_iris$Sepal.Length))  # Should be converted
})

test_that("iris numeric columns are handled correctly", {

    # Test that numeric columns are properly targeted by relevant functions
    numeric_cols <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

    # types function should convert numeric to character
    types_iris <- toddler_types(iris, cols = numeric_cols, prop = 1)
    for (col in numeric_cols) {
        expect_true(is.character(types_iris[[col]]))
    }

    # units function should add units to numeric columns
    units_iris <- toddler_units(iris, cols = numeric_cols, prop = 1)
    for (col in numeric_cols) {
        expect_true(is.character(units_iris[[col]]))
        # Should contain some unit strings
        expect_true(any(grepl("kg|lbs|cm|in|sec|min", units_iris[[col]])))
    }

    # missing should work on numeric columns
    missing_iris <- toddler_missing(iris, cols = numeric_cols, prop = 0.5)
    for (col in numeric_cols) {
        expect_gt(sum(is.na(missing_iris[[col]])), 0)
    }
})

test_that("complete iris pipeline works", {

    # Test complete transformation pipeline on iris
    set.seed(42)
    messy_iris <- iris |>
        toddler_missing(prop = 0.05, seed = 42) |>
        toddler_inconsistent(prop = 0.2, seed = 42) |>  # Now has prop parameter
        toddler_types(cols = c("Sepal.Length", "Petal.Length"), prop = 0.1, seed = 42) |>
        toddler_duplicate(prop = 0.1, seed = 42) |>
        toddler_names(style = "title", seed = 42) |>
        toddler_extra(add_random = TRUE, seed = 42)

    expect_s3_class(messy_iris, "data.frame")
    expect_gt(nrow(messy_iris), nrow(iris))  # Should have duplicates and empty rows
    expect_equal(ncol(messy_iris), ncol(iris))  # Same number of columns
    expect_gt(sum(is.na(messy_iris)), sum(is.na(iris)))  # More NAs

    # Test that names are actually changed (force a change by testing individual styles)
    set.seed(123)
    title_iris <- toddler_names(iris, style = "title", seed = 123)
    expect_false(identical(names(title_iris), names(iris)))
    expect_true(any(grepl(" ", names(title_iris))))  # Title style should have spaces

    upper_iris <- toddler_names(iris, style = "upper", seed = 123)
    expect_false(identical(names(upper_iris), names(iris)))
    expect_true(all(names(upper_iris) == toupper(names(iris))))

    # Column names should be different (title case)
    expect_false(identical(names(messy_iris), names(iris)))

    # Should be reproducible
    set.seed(42)
    messy_iris2 <- iris |>
        toddler_missing(prop = 0.05, seed = 42) |>
        toddler_inconsistent(prop = 0.2, seed = 42) |>
        toddler_types(cols = c("Sepal.Length", "Petal.Length"), prop = 0.1, seed = 42) |>
        toddler_duplicate(prop = 0.1, seed = 42) |>
        toddler_names(style = "title", seed = 42) |>
        toddler_extra(add_random = TRUE, seed = 42)

    expect_equal(messy_iris, messy_iris2)
})

test_that("iris wide transformation works", {

    # Create a subset of iris in long format for testing toddler_wide
    iris_long <- data.frame(
        id = rep(1:10, each = 4),
        Species = rep(iris$Species[1:10], each = 4),
        measure = rep(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 10),
        value = c(
            iris$Sepal.Length[1:10],
            iris$Sepal.Width[1:10],
            iris$Petal.Length[1:10],
            iris$Petal.Width[1:10]
        )
    )

    # Test wide transformation
    iris_wide <- toddler_wide(iris_long, "measure", "value", c("id", "Species"))

    expect_s3_class(iris_wide, "data.frame")
    expect_equal(nrow(iris_wide), 10)
    expect_true("value_Sepal.Length" %in% names(iris_wide))
    expect_true("value_Petal.Width" %in% names(iris_wide))
    expect_true("Species" %in% names(iris_wide))
})

test_that("iris data splitting works", {

    # Add an ID column to iris for testing splits
    iris_with_id <- iris
    iris_with_id$id <- 1:nrow(iris)

    # Test row splitting
    set.seed(123)
    iris_split <- toddler_unjoin(iris_with_id, by_rows = "id", prop = 0.7, seed = 123)

    expect_type(iris_split, "list")
    expect_length(iris_split, 2)
    expect_named(iris_split, c("data1", "data2"))

    total_rows <- nrow(iris_split$data1) + nrow(iris_split$data2)
    expect_equal(total_rows, nrow(iris_with_id))

    # Test column splitting
    iris_col_split <- toddler_unjoin(iris_with_id,
                                     by_cols = TRUE,
                                     id_cols = "id",
                                     cols1 = c("Sepal.Length", "Sepal.Width"))

    expect_type(iris_col_split, "list")
    expect_length(iris_col_split, 2)
    expect_true("id" %in% names(iris_col_split$data1))
    expect_true("id" %in% names(iris_col_split$data2))
    expect_true("Sepal.Length" %in% names(iris_col_split$data1))
    expect_true("Petal.Length" %in% names(iris_col_split$data2))
    expect_false("Petal.Length" %in% names(iris_col_split$data1))

    # Test group splitting by Species
    iris_species_split <- toddler_unjoin(iris, by_groups = "Species")

    expect_type(iris_species_split, "list")
    expect_length(iris_species_split, 3)  # setosa, versicolor, virginica
    expect_named(iris_species_split, c("group_setosa", "group_versicolor", "group_virginica"))

    # Each group should only contain that species
    expect_true(all(iris_species_split$group_setosa$Species == "setosa"))
    expect_true(all(iris_species_split$group_versicolor$Species == "versicolor"))
    expect_true(all(iris_species_split$group_virginica$Species == "virginica"))
})

test_that("iris edge cases are handled", {

    # Test with single row from iris
    single_iris <- iris[1, ]
    expect_s3_class(toddler_missing(single_iris), "data.frame")
    expect_s3_class(toddler_duplicate(single_iris), "data.frame")
    expect_s3_class(toddler_extra(single_iris), "data.frame")

    # Test with small subset
    small_iris <- iris[1:3, ]
    set.seed(999)
    small_messy <- small_iris |>
        toddler_missing(prop = 0.3, seed = 999) |>
        toddler_duplicate(prop = 0.5, seed = 999)

    expect_s3_class(small_messy, "data.frame")
    expect_gt(nrow(small_messy), nrow(small_iris))

    # Test with very high proportions
    expect_s3_class(toddler_missing(iris[1:5, ], prop = 0.9), "data.frame")
    expect_s3_class(toddler_duplicate(iris[1:5, ], prop = 3), "data.frame")
})

test_that("iris data types are preserved appropriately", {

    # Original data types
    expect_true(is.numeric(iris$Sepal.Length))
    expect_true(is.numeric(iris$Sepal.Width))
    expect_true(is.numeric(iris$Petal.Length))
    expect_true(is.numeric(iris$Petal.Width))
    expect_true(is.factor(iris$Species))

    # Functions that should preserve types
    missing_iris <- toddler_missing(iris, prop = 0.2)
    expect_true(is.numeric(missing_iris$Sepal.Length))
    expect_true(is.factor(missing_iris$Species))

    dup_iris <- toddler_duplicate(iris, prop = 0.2)
    expect_true(is.numeric(dup_iris$Sepal.Length))
    expect_true(is.factor(dup_iris$Species))

    named_iris <- toddler_names(iris)
    expect_true(is.numeric(named_iris[[1]]))  # First column should still be numeric
    # Find the Species column (originally column 5) and check it's still a factor
    species_col_index <- which(sapply(named_iris, is.factor))
    expect_length(species_col_index, 1)  # Should have exactly one factor column
    expect_true(is.factor(named_iris[[species_col_index]]))  # Should still be factor

    # Functions that should change specific types
    types_iris <- toddler_types(iris, prop = 0.5)
    expect_true(is.character(types_iris$Sepal.Length))  # Should be converted
    expect_true(is.factor(types_iris$Species))  # Should remain factor

    units_iris <- toddler_units(iris, prop = 0.5)
    expect_true(is.character(units_iris$Sepal.Length))  # Should be converted
    expect_true(is.factor(units_iris$Species))  # Should remain factor
})
