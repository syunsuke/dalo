#' Download Files Easily
#'
#' @param urls url string vector
#' @param dest_dir directory which files get into
#' @param check if TRUE, don't download exist files
#' @param make_dir if TRUE, make a directory
#'
#' @importFrom utils download.file
#' @export
dalo_download_by_urls <-
  function(urls, dest_dir = "./", check = TRUE, make_dir = FALSE){

  # dest_dirが存在しない場合エラーで終了

  if(!dir.exists(paths = dest_dir)){
    if(make_dir){
      dir.create(dest_dir, recursive = TRUE)
      #message((sprintf("%s has been created.", dest_dir)))
    }else{
      stop("dest_dir does not exist.")
    }
  }

  # 既に存在するファイルをダウンロードしない場合
  if (check){
    urls <- newfiles_filter(urls, dest_dir)
  }

  # エラーURLを確認できるようにする
  e_url = vector()

  for (i in seq_along(urls) ){
    dest_path <- paste0(dest_dir, "/", basename(urls[i]))

    Sys.sleep(0.5)

    tryCatch({
      download.file(url = urls[i], mode = "wb", destfile = dest_path)
    },
    error = function(e){
      message("Error!!")
      message(e)
      message("")
      e_url <<- c(e_url, urls[i])
    }
    )
  }

  if(length(e_url)>0){
    message("following urls are error.")
    print(e_url)
  }
}

# subfunction
# 任意のディレクトリにあるファイル名と一致する
# basenameをもつURLをベクトルから外す
# 重複を外す
newfiles_filter <- function(urls, dest_dir){
  downloaded_files <- dir(dest_dir)
  ans <- c()

  for(i in seq_along(urls)){
    if (!(basename(urls[i]) %in% downloaded_files)){
      ans <- c(ans,urls[i])
    }
  }

  ans <- unique(ans)
  return(ans)
}

