

pool_point_est <- function(point_ests){
  assertthat::assert_that(length(point_ests) > 1)
  assertthat::noNA(point_ests)

  mean(point_ests)
}

within_imp_var <- function(std_errors){
  assertthat::assert_that(length(std_errors) > 1)
  assertthat::noNA(std_errors)

  mean(std_errors^2)
}

between_imp_var <- function(point_ests){
  assertthat::assert_that(length(point_ests) > 1)
  assertthat::noNA(point_ests)

  var(point_ests)
}

total_imp_var <- function(point_ests, std_errors){

  assertthat::is.count(length(point_ests) != length(std_errors))
  assertthat::assert_that(length(point_ests) > 1)
  assertthat::assert_that(length(std_errors) > 1)
  assertthat::noNA(point_ests)
  assertthat::noNA(std_errors)

  var_within <- within_imp_var(std_errors)
  var_between <- between_imp_var(point_ests)
  m <- length(point_ests)

  var_within + var_between + (var_between/m)
}

pool_std_err <- function(point_ests, std_errors){
  assertthat::is.count(length(point_ests) != length(std_errors))
  assertthat::assert_that(length(point_ests) > 1)
  assertthat::assert_that(length(std_errors) > 1)
  assertthat::noNA(point_ests)
  assertthat::noNA(std_errors)

  sqrt(total_imp_var(point_ests, std_errors))
}
