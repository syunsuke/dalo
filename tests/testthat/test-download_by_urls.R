test_that("check filter url", {

  target <- c("https://syunsuke.github.io/assets/images/001.jpg",
              "https://syunsuke.github.io/assets/images/002.jpg",
              "https://syunsuke.github.io/assets/images/002.jpg",
              "https://syunsuke.github.io/assets/images/002.jpg",
              "https://syunsuke.github.io/assets/images/003.jpg"
              )

  # 存在するファイル名の集合
  data_dir <- "test_data/"
  downloaded_files <- c("001.jpg")
  dir.create(data_dir)
  file.create(paste0(data_dir,downloaded_files))

  ans <- c("https://syunsuke.github.io/assets/images/002.jpg",
           "https://syunsuke.github.io/assets/images/003.jpg")

  expect_equal(newfiles_filter(target, data_dir), ans)

  # 後処理
  unlink(data_dir, recursive = TRUE)
})

test_that("ダウンロード関数が正しく動作すること", {

  mock_download.file <- function(url, destfile, mode, ...){
    file.create(destfile)
  }

  testthat::with_mocked_bindings(
    code = {
      target <- c("https://syunsuke.github.io/assets/images/001.jpg",
                  "https://syunsuke.github.io/assets/images/003.jpg")

      #####################################################
      # ディレクトリがない場合作成して、ダウンロード
      #####################################################
      dalo_download_by_urls(target, "dldata2/", make_dir = TRUE)

      # ダウンロードが成功したかどうかを確認
      expect_true(all(file.exists(paste0("dldata2/",basename(target)))))

    # 後処理
    unlink("dldata2/", recursive = TRUE)

      #####################################################
      # ディレクトリがない場合にエラーになる
      # デフォルトでディレクトリを作らない
      #####################################################
      expect_error(dalo_download_by_urls(target, "dldata2"))

    },
    download.file = mock_download.file
  )

})
