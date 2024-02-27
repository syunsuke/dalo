test_that("fetch link test", {
  file_path <- "html/test.html"
  expect_equal(dalo_fetch_links(file_path,
                                selector = ".dog",
                                filename_pattern = ".pdf",
                                encoding = "utf-8"),
               c("/dog002.pdf"))
})
