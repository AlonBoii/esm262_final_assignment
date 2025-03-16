#' Test file for: Compute Injury Risk During Climbing Sessions
#'
#' @param hours A numeric value representing the number of hours spent in a climbing session. The value should be between 1 and 6.
#' @param experience A character string indicating the climber's experience level. It can be one of the following:
#'        "beginner", "intermediate", "skilled", or "pro". More experienced climbers have a lower risk of injury.
#' @param hold_size A character string indicating the size of the holds used in the climbing session. It can be one of:
#'        "tiny", "small", "medium", or "large". Larger holds are associated with a lower risk of injury.
#' @param hold_type A character string indicating the type of holds used in the climbing session. It can be one of:
#'        "crimp", "pinch", or "jug". Crimp holds are riskier compared to pinch and jug holds.
#' 
#' @return injury_risk


library(tidyverse)
library(devtools)
library(testthat)

test_that("Compute Injury Risk During Climbing Sessions", {
  
  # Test case 1: Beginner climber with tiny holds for 2 hours
  result1 <- tryCatch({
    result <- capture_warnings(compute_injury_risk(2, "beginner", "tiny", "crimp"))
    result
  }, warning = function(w) {
    # Return the warning message as part of the result
    return(list(warning = conditionMessage(w), result = NULL))
  })
  
  # Check if warning occurred
  expect_true(grepl("A beginner isn't likely to be training on crimps", result1$warning))  # Check for warning message
  expect_true(result1$result$injury_risk > 0.1)  # Ensure risk value is non-zero
  
  # Test case 2: Intermediate climber with small holds for 4 hours
  result2 <- compute_injury_risk(4, "intermediate", "small", "pinch")
  expect_true(result2$injury_risk > 0.1)  # Risk should be greater than 0.1
  
  # Test case 3: Skilled climber with medium holds for 4 hours
  result3 <- compute_injury_risk(4, "skilled", "medium", "jug")
  expect_true(result3$injury_risk < 0.1)  # Risk should be low
  
  # Test case 4: Pro climber with large holds for 5 hours (warning expected)
  result4 <- tryCatch({
    result <- capture_warnings(compute_injury_risk(5, "pro", "large", "jug"))
    result
  }, warning = function(w) {
    # Return the warning message as part of the result
    return(list(warning = conditionMessage(w), result = NULL))
  })
  
  # Check if warning occurred
  expect_true(grepl("A pro isn't likely to be training on jugs", result4$warning))  # Check for warning message
  
  # Test case 5: Intermediate climber with tiny holds for 6 hours (high injury risk)
  result5 <- compute_injury_risk(6, "intermediate", "tiny", "crimp")
  expect_true(result5$injury_risk >= 0.45)  # Ensure the risk is high enough to cause injury
  expect_equal(result5$status, "Injured")  # Should return "Injured"
  
  # Test case 6: Invalid hold type (should return an error message)
  expect_error(
    compute_injury_risk(4, "skilled", "medium", "invalid_hold"), 
    "Invalid hold type. Please choose from: crimp, pinch, or jug."
  )
})
