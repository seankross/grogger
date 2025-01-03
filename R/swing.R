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

#' Build Grogger Blog
#'
#' @param index Path to the `index.Rmd` file.
#' @param posts Path to the directory that contains the blog posts.
#' @param output_dir Path to the directory where the output files will be
#' created.
#' @importFrom here here
#' @importFrom fs dir_ls dir_create path
#' @importFrom rmarkdown render pandoc_available
swing <- function(index = here("index.Rmd"),
                    posts = here("posts"),
                    output_dir = here("docs")) {
  post_rmds <- dir_ls(posts, regexp = "*.[R|r]md$") %>% sort()

  ordered_posts <- post_rmds %>%
    lapply(yaml_front_matter) %>%
    (function(x) x[order(sapply(x, function(y) y$date), decreasing = TRUE)])()

  post_dirs <- ordered_posts %>%
    names() %>%
    basename() %>%
    (function(x) sub("\\.[^.]*$", "", basename(x)))()

  post_htmls <- path(output_dir, post_dirs, "index.html") %>% sort()
  temp_post_htmls <- path(posts, post_dirs) %>% paste0(".html") %>% sort()

  posts_were_built <- FALSE
  for (i in seq_along(post_rmds)) {
    if (!file.exists(post_htmls[i]) || is_older(post_htmls[i], post_rmds[i])) {
      posts_were_built <- TRUE # sloppy but it works
      fs::dir_create(dirname(post_htmls[i]), recurse = TRUE)
      render(post_rmds[i], html_document(
        theme = NULL,
        template = "templates/post.html" %>% here(),
        self_contained = rmarkdown::pandoc_available("2.8"))
      )

      delete_then_move(temp_post_htmls[i], post_htmls[i])
    }
  }

  posts_yaml <- post_rmds %>%
    lapply(yaml_front_matter) %>%
    (function(x) x[order(sapply(x, function(y) y$date), decreasing = TRUE)])() %>%
    lapply(function(x){
      paste0("  - title: ", x$title, "\n",
             "    ", "date: ", x$date, "\n",
             "    ", "link:")
    }) %>%
    unname() %>%
    unlist() %>%
    paste(post_dirs) %>%
    paste0(collapse = "\n") %>%
    (function(x) paste0("posts:\n", x))()

  pandoc_metadata <- tempfile(fileext = ".yaml")
  writeLines(posts_yaml, pandoc_metadata)

  temp_home_html <- sub("\\.[^.]*$", "", index) %>% paste0(".html")
  home_html <- path(output_dir, "index.html")

  if(!file.exists(home_html) || is_older(home_html, index) || posts_were_built) {
    render(index, html_document(
      theme = NULL,
      template = "templates/index.html",
      pandoc_args = c("--metadata-file", pandoc_metadata)))

    delete_then_move(temp_home_html, home_html)
  }
}
