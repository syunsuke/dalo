#' Download Files Easily
#'
#' @param urls url string vector
#' @param dest_dir directory which files get into
#' @param check if TRUE, check downloaded files
#'
#' @export
dalo_download_by_urls <- function(urls, dest_dir = "./", check = TRUE){

  # dest_dirが存在しない場合エラーで終了
  if(!dir.exists(paths = dest_dir)){
    stop("dest_dir does not exist.")
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
      utils::download.file(url = urls[i], mode = "wb", destfile = dest_path)
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

newfiles_filter <- function(urls, dest_dir){
  downloaded_files <- dir(dest_dir)
  ans <- c()

  for(i in seq_along(urls)){
    if (!exists_dlfile(urls[i], downloaded_files)){
      ans <- c(ans,urls[i])
    }
  }
  return(ans)
}

exists_dlfile <- function(url, file_list){

  if (length(url) > 1 ){
    message("Warnning exists_dlfile: url has more than one contents.")
  }

  base_name <- basename(url[1])
  downloaded_files <- file_list

  ans <- FALSE
  if (base_name %in% downloaded_files) {
    ans <- TRUE
  }

  return(ans)
}
