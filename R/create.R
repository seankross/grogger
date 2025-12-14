#' Create a Grogger Blog
#'
#' @param path Path to
#' @importFrom here here
#' @importFrom fs dir_create path file_copy
#' @export
create_grogger <- function(path = here(), edit = TRUE) {
  posts <- path(path, "posts")
  docs <- path(path, "docs")
  docs <- path(path, "docs")
  dir_create(posts)
  dir_create(docs)
  index <- system.file(path("grogger-templates", "index.Rmd"),
                       package = "grogger")
  post <- system.file(path("grogger-templates", "post.Rmd"),
                      package = "grogger")
  file_copy(index, path)
  file_copy(post, path(posts, "post.Rmd"))

  if (edit) {
    
  }
}
