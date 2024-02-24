#' PickUp some files From Zipfile
#'
#' @param zipfiles vector of zipfiles path
#' @param pattern pattern of files which you want
#' @param dest_dir destination where pickuped file goes
#'
#' @export
dalo_pickup_from_zipfile <-
  function(zipfiles, pattern = "", dest_dir = "."){

  lapply(zipfiles, pickup_from_single_zipfile, pattern, dest_dir)
}


# subroutine
pickup_from_single_zipfile <-
  function(zipfile, pattern = "", dest_dir = "."){

  file_list <-
    utils::unzip(zipfile, list = TRUE) %>%
    dplyr::pull("Name") %>%
    stringr::str_subset(pattern)

  utils::unzip(zipfile = zipfile,
               files = file_list,
               junkpaths = TRUE,
               exdir = dest_dir)
}
