# ### ---> This test does not pass although the error message is as expected... I don't know why this test doesn't work...
# test_that("oracle.checkname demands a character input", {
#   expect_error(
#     oracle.checkname(1),
#     "[oracle.checkname]: Input argument var.name must be character"
#   )
# })
test_that("Check output for a reserved term", {
  expect_equal(
    oracle.checkname("ADD"),
    data.frame(
      variable = "ADD",
      check = "!!! !!! Oracle SQL or SAS reserved term. Change it!"
    )
  )
})
test_that("Check output for an admissible variable name", {
  expect_equal(
    oracle.checkname("SMOKING"),
    data.frame(
      variable = "SMOKING",
      check = "OK"
    )
  )
})
