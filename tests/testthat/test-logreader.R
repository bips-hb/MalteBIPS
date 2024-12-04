test_that("Check that logreader corectly detects operation on missing values note in SAS log", {
  expect_equal(
    logreader(test_path("../testdata/logreader.log")),
    data.frame(
      log = c(
        "NOTE: Missing values were generated as a result of performing an operation on missing values.",
        "ERROR: File WORK.DOESNOTEXIST.DATA does not exist.",
        "NOTE: The SAS System stopped processing this step because of errors.",
        "WARNING: The data set WORK.TEST may be incomplete.  When this step was stopped there were 0 ",
        "WARNING: Data set WORK.TEST was not replaced because this step was stopped."
      ),
      rownumber = c(28,42,45,46,48)
    )
  )
})
