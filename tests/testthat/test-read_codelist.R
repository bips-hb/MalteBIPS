test_that("Import ICD example codelist to check that the function reconizes the file structure", {
  expect_equal(
    read.codelist(test_path("../testdata/codelist_icd.csv")),
    data.frame(
      VAR_GRP = rep("COV", 3),
      VAR_NAME = rep("CHOLERA", 3),
      LABEL_NAME = rep("Cholera", 3),
      TEXT = c("Cholera durch Vibrio cholerae O:1, Biovar cholerae",
               "Cholera durch Vibrio cholerae O:1, Biovar eltor",
               "Cholera, nicht näher bezeichnet"),
      VAL_BEG = rep(2004, 3),
      VAL_END = rep(2023, 3),
      SUBSTRING = rep(0, 3),
      domain = rep("ICD", 3),
      code = c("A000", "A001", "A009")
    )
  )
})
