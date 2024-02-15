test_that("check downloaded files", {

  target <- c("https://syunsuke.github.io/assets/images/001.jpg",
              "https://syunsuke.github.io/assets/images/002.jpg")

  target_list <- dir("data/")
  target_list_no_dir <- dir("dataa/")
  target_list_empty_dir <- dir("datab/")

  expect_equal(exists_dlfile(target[1], file_list = target_list),TRUE)
  expect_equal(exists_dlfile(target[2], file_list = target_list),FALSE)
  expect_equal(exists_dlfile(target[1], file_list = target_list_no_dir),FALSE)
  expect_equal(exists_dlfile(target[1], file_list = target_list_empty_dir),FALSE)

})

test_that("check filter url", {

  target <- c("https://syunsuke.github.io/assets/images/001.jpg",
              "https://syunsuke.github.io/assets/images/002.jpg")

  target_dir <- "data/"
  target_list <- dir(target_dir)

  ans <- c("https://syunsuke.github.io/assets/images/002.jpg")
  expect_equal(newfiles_filter(target, target_dir),ans)

})
