#' Compute Injury Risk During Climbing Sessions
#'
#' This function calculates the potential injury risk based on the duration of a climbing session, the climber's experience level,
#' the size and type of holds used, and returns an injury risk score. The function also includes error checks for unrealistic inputs.
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

compute_injury_risk <- function(hours, experience, hold_size, hold_type) {
  
  # Error checks:
  # Check if hours is 0 or negative
  if (hours <= 0) {
    warning("Hours must be a positive value, unrealistic")
  }
  
  # Check if hours exceed 6
  if (hours > 6) {
    warning("A climbing session normally would not go over 6 hours at the most extreme circumstances, unrealistic")
  }
  
  # Check if "beginner" is climbing more than 3 hours
  if (experience == "beginner" && hours > 3) {
    warning("A beginner isn't likely to climb more than 3 hours, unrealistic")
  }
  
  # Check if "pro" is training on "jug" hold
  if (experience == "pro" && hold_type == "jug") {
    warning("A pro isn't likely to be training on jugs, unrealistic")
  }
  
  # Check if "beginner" is using "crimp"
  if (experience == "beginner" && hold_type == "crimp") {
    warning("A beginner isn't likely to be training on crimps, unrealistic")
  }
  
  # Check if experience level is valid
  valid_experience <- c("beginner", "intermediate", "skilled", "pro")
  if (!(experience %in% valid_experience)) {
    stop("Invalid experience level. Please choose from: beginner, intermediate, skilled, or pro.")
  }
  
  # Check if hold size is valid
  valid_hold_size <- c("tiny", "small", "medium", "large")
  if (!(hold_size %in% valid_hold_size)) {
    stop("Invalid hold size. Please choose from: tiny, small, medium, or large.")
  }
  
  # Check if hold type is valid
  valid_hold_type <- c("crimp", "pinch", "jug")
  if (!(hold_type %in% valid_hold_type)) {
    return("Invalid hold type. Please choose from: crimp, pinch, or jug.")
  }
  
  # Hours factor (more hours = more risk)
  hours_factor <- 0.1 * hours
  
  # Experience level factor (beginner is highest risk, pro is lowest)
  experience_factor <- switch(experience,
                              "beginner" = 1.0,
                              "intermediate" = 0.75,
                              "skilled" = 0.5,
                              "pro" = 0.25)
  
  # Hold size factor (smaller holds = higher risk)
  hold_size_factor <- switch(hold_size,
                             "tiny" = 1.0,
                             "small" = 0.8,
                             "medium" = 0.5,
                             "large" = 0.2)
  
  # Hold type factor (crimp = highest risk)
  hold_type_factor <- switch(hold_type,
                             "crimp" = 1.0,
                             "pinch" = 0.8,
                             "jug" = 0.2)
  
  # Combine all factors to compute injury risk
  injury_risk <- hours_factor * experience_factor * hold_size_factor * hold_type_factor
  
  #if injury risk is greater than or equal to 0.45, return "Injured"
  if (injury_risk >= 0.45) {
    warning("Injured")
  }
  
  return(injury_risk)
}


