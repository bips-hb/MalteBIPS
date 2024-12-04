test_that("Load dummy codelists and check that the function correctly identifies the domains", {
  expect_equal(
    detect.domains(
      rbind(
        read.codelist(test_path("../testdata/codelist_icd.csv"))[,c("VAR_GRP",
                                                                    "VAR_NAME",
                                                                    "TEXT",
                                                                    "LABEL_NAME",
                                                                    "SUBSTRING",
                                                                    "domain",
                                                                    "code")],
        read.codelist(test_path("../testdata/codelist_atc.csv"))[,c("VAR_GRP",
                                                                    "VAR_NAME",
                                                                    "TEXT",
                                                                    "LABEL_NAME",
                                                                    "SUBSTRING",
                                                                    "domain",
                                                                    "code")]
      )
    ),
    data.frame(
      VAR_GRP = "COV",
      VAR_NAME = "CHOLERA",
      relevant.domains = "ATC, ICD"
    )
  )
})
