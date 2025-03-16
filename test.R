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
#' 
library(tidyverse)
library(devtools)
library(testthat)

#test the comoute_injury_risk function

test_that("Compute Injury Risk During Climbing Sessions", {
  # Test case 1: Beginner climber with tiny holds for 2 hours
  expect_warning(compute_injury_risk(2, "beginner", "tiny", "crimp"))
  
  # Test case 2: Intermediate climber with small holds for 3 hours
  expect_true(compute_injury_risk(4, "intermediate", "small", "pinch")> 0.1)
  
  # Test case 3: Skilled climber with medium holds for 4 hours
  expect_true(compute_injury_risk(4, "skilled", "medium", "jug")< 0.1)
  
  # Test case 4: Pro climber with large holds for 5 hours
  expect_warning(compute_injury_risk(5, "pro", "large", "jug"))
  
  #Test case 5: Beginner gets "Injured" warning
  expect_warning(compute_injury_risk(6, "intermediate", "tiny", "crimp"))
})