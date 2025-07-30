# Tests for camelCase splitting functionality

test_that("camelCase splitting works correctly", {

    # Test basic camelCase splitting
    expect_equal(toddler:::.split_camel_case("firstName"), "first Name")
    expect_equal(toddler:::.split_camel_case("lastName"), "last Name")
    expect_equal(toddler:::.split_camel_case("ageInYears"), "age In Years")

    # Test with multiple capital letters
    expect_equal(toddler:::.split_camel_case("XMLHttpRequest"), "XML Http Request")
    expect_equal(toddler:::.split_camel_case("getHTMLElement"), "get HTML Element")

    # Test edge cases
    expect_equal(toddler:::.split_camel_case("a"), "a")  # Single letter
    expect_equal(toddler:::.split_camel_case("A"), "A")  # Single uppercase
    expect_equal(toddler:::.split_camel_case(""), "")    # Empty string
    expect_equal(toddler:::.split_camel_case("alllowercase"), "alllowercase")  # No capitals
    expect_equal(toddler:::.split_camel_case("ALLUPPERCASE"), "ALLUPPERCASE")  # All capitals

    # Test that non-camelCase strings are unchanged
    expect_equal(toddler:::.split_camel_case("snake_case"), "snake_case")
    expect_equal(toddler:::.split_camel_case("dot.notation"), "dot.notation")
    expect_equal(toddler:::.split_camel_case("space separated"), "space separated")
})

test_that("toddler_names handles camelCase input correctly", {

    # Test with camelCase data frame
    camel_data <- data.frame(
        firstName = "Alice",
        lastName = "Smith",
        ageInYears = 25,
        testScore = 95.5
    )

    # Test title case conversion
    title_result <- toddler_names(camel_data, style = "title")
    expected_names <- c("First Name", "Last Name", "Age In Years", "Test Score")
    expect_equal(names(title_result), expected_names)

    # Test upper case (should just be uppercase, no splitting)
    upper_result <- toddler_names(camel_data, style = "upper")
    expected_upper <- c("FIRSTNAME", "LASTNAME", "AGEINYEARS", "TESTSCORE")
    expect_equal(names(upper_result), expected_upper)

    # Test lower case (should just be lowercase, no splitting)
    lower_result <- toddler_names(camel_data, style = "lower")
    expected_lower <- c("firstname", "lastname", "ageinyears", "testscore")
    expect_equal(names(lower_result), expected_lower)

    # Data should be unchanged, only names should change
    expect_equal(title_result$`First Name`, camel_data$firstName)
    expect_equal(title_result$`Last Name`, camel_data$lastName)
})

test_that("toddler_names handles mixed input formats correctly", {

    # Test with mixed naming conventions
    mixed_data <- data.frame(
        snake_case_name = 1:5,      # snake_case
        camelCaseName = 1:5,        # camelCase
        dot.notation.name = 1:5,    # dot.notation
        regularname = 1:5           # no separators
    )

    # Test title case handles all formats
    title_result <- toddler_names(mixed_data, style = "title")
    expected_names <- c(
        "Snake Case Name",    # snake_case -> spaces
        "Camel Case Name",    # camelCase -> split and spaces
        "Dot Notation Name",  # dot.notation -> spaces
        "Regularname"         # no change needed
    )
    expect_equal(names(title_result), expected_names)

    # Test that data integrity is preserved
    expect_equal(ncol(title_result), ncol(mixed_data))
    expect_equal(nrow(title_result), nrow(mixed_data))
    expect_equal(title_result$`Snake Case Name`, mixed_data$snake_case_name)
})

test_that("edge cases in camelCase splitting are handled", {

    # Test with numbers in names

    # Test with single letter words
    expect_equal(toddler:::.split_camel_case("aVeryLongName"), "a Very Long Name")
    expect_equal(toddler:::.split_camel_case("iPhoneApp"), "i Phone App")

    # Test with consecutive capitals
    expect_equal(toddler:::.split_camel_case("HTTPSConnection"), "HTTPS Connection")
    expect_equal(toddler:::.split_camel_case("JSONParser"), "JSON Parser")
})
