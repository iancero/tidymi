# Example imputations provided by Enders (2010), Chapter 8
# Check your functions against his results described in the book's examples
test_imps <- read.csv('enders_imputation_examples.csv')

testthat::test_that(
  desc = 'within_imp_var() aligns with Enders (2010) examples in Chaper 8',
  code = {
    testthat::expect_equal(
      object = within_imp_var(test_imps$se) %>%
        round(digits = 5), # precision in Enders' example result
      expected = 0.00215)
  }
)

testthat::test_that(
  desc = 'between_imp_var() aligns with Enders (2010) examples in Chaper 8',
  code = {
    testthat::expect_equal(
      object = between_imp_var(test_imps$param_est) %>%
        round(digits = 5), # precision in Enders' example result
      expected = 0.00515)
  }
)

testthat::test_that(
  desc = 'total_imp_var() aligns with Enders (2010) examples in Chaper 8',
  code = {
    testthat::expect_equal(
      object = total_imp_var(
          point_ests = test_imps$param_est,
          std_errors = test_imps$se) %>%
        round(digits = 4), # precision in Enders' example result (hand-checked)
      expected = round(0.00756, 4))
  }
)


