#' @importFrom fs file_delete file_move
delete_then_move <- function(file1, file2) {
  if (file.exists(file2)) {
    file_delete(file2)
  }

  file_move(file1, file2)
}

is_older <- function(file1, file2) {
  file_info(file1)$modification_time < file_info(file2)$modification_time
}

strip_ext <- function(x) sub("\\.[^.]*$", "", basename(x))
