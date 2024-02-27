#//////////////////////////////////////////////////////////////////
# チェック用のルーチン
#//////////////////////////////////////////////////////////////////

# ページの存在確認をして、なければエラーで終了する
sub_url_check <- function(url){

  # サイトの存在を確認
  res <- httr::HEAD(url)
  if (httr::status_code(res) != 200){
    stop("Download Site is something wrong.")
  }
  return(url)
}



#' fetch link urls from web page
#'
#' @param url WebPage url or html file path
#' @param selector CSS selector
#' @param filename_pattern Regular expressions
#' @param encoding utf-8 or shift-jis and so on
#' @param prefix prefix strings
#'
#' @return vector of links
#' @export
dalo_fetch_links <- function(url,
                             selector = "",
                             filename_pattern = ".*",
                             encoding = "utf-8",
                             prefix = ""){

  if (stringr::str_detect(url, "^http")){
    node_data <- sub_url_check(url) %>%
      rvest::read_html(encoding = encoding)
  }else{
    node_data <- url %>%
      rvest::read_html(encoding = encoding)
  }

  ## フィルタリング
  node_select <- paste(selector,"a",sep = " ")
  link_data <-
    node_data %>%
    rvest::html_nodes(node_select) %>%
    rvest::html_attr("href") %>%
    stringr::str_subset(filename_pattern)

  # ページからスクレイピングしたURLは、一部の文字列なので、完全なURLにする。
  link_data <- paste0(prefix, link_data)

  return(link_data)
}


