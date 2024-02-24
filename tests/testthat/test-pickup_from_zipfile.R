test_that("pick up a file", {

  sub_create_zipfile <- function(prestr){
    target_filename <- paste0(prestr,c(".txt",".pdf",".xlsx"))
    target_dir <- paste0(prestr,"tmpdir/")
    zipname <- paste0("my",prestr,".zip")

    dir.create(target_dir, recursive = TRUE)
    target_filename <- paste0(target_dir,target_filename)
    file.create(target_filename)

    zip(zipname,target_filename)
    unlink(target_dir,recursive = TRUE)
  }

  # zipファイルの作成
  sub_create_zipfile("001")
  sub_create_zipfile("002")

  # テスト
  dest_dir = "dest/"
  dir.create(dest_dir)

  # パターンで抜き出す
  dalo_pickup_from_zipfile(c("my001.zip","my002.zip"),
                      pattern = "\\.xlsx$",
                      dest_dir = dest_dir)
  expect_true(file.exists(paste0(dest_dir,"001.xlsx")))
  expect_true(file.exists(paste0(dest_dir,"002.xlsx")))

  # dest_dirがない場合エラーになる
  expect_error(dalo_pickup_from_zipfile("myzip.zip", dest_dir = "hogehoge"))


  # 後処理
  unlink("my001.zip")
  unlink("my002.zip")
  unlink(dest_dir, recursive = TRUE)
})
