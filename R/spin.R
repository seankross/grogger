is_older <- function(file1, file2) {
  file_info(file1)$modification_time < file_info(file2)$modification_time
}

swing <- function(index = here("index.Rmd"), 
                    posts = here("posts"), 
                    output_dir = here("docs")) {
  post_rmds <- dir_ls(posts, regexp = "*.[R|r]md$")
  
  ordered_posts <- post_rmds %>% 
    lapply(yaml_front_matter) %>%
    (function(x) x[order(sapply(x, function(y) y$date), decreasing = TRUE)])()
  
  post_htmls <- ordered_posts %>%
    names() %>%
    basename() %>% 
    (function(x) sub("\\.[^.]*$", ".html", basename(x)))()
  
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
    paste(post_htmls) %>% 
    paste0(collapse = "\n") %>%
    (function(x) paste0("posts:\n", x))()
  
  pandoc_metadata <- tempfile(fileext = ".yaml")
  writeLines(posts_yaml, pandoc_metadata)
  
  index_html <- path(output_dir, sub("\\.[^.]*$", ".html", basename(index)))
  no_index <- index_html %>% Negate(file_exists)() %>% unname()
  old_index <- is_older(index_html, index)
  any_post_newer_than_index <- Map(c, rep(index_html, length(post_rmds)), post_rmds) %>%
    sapply(function(x){ is_older(x[1], x[2]) }) %>%
    unname() %>%
    any()
  
  
  render(index, html_document(
    theme = NULL,
    template = "templates/index.html",
    pandoc_args = c("--metadata-file", pandoc_metadata)),
    output_dir = output_dir)
  
  
  
}

library(magrittr)
library(rmarkdown)
library(here)
library(fs)

post_rmds <- dir_ls(here("posts"), regexp = "*.[R|r]md$")
post_htmls <- basename(post_rmds) %>% 
  tools::file_path_sans_ext() %>% 
  paste0(".html")

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
  paste(post_htmls) %>% 
  paste0(collapse = "\n") %>%
  (function(x) paste0("posts:\n", x))()

pandoc_metadata <- tempfile(fileext = ".yaml")
writeLines(posts_yaml, pandoc_metadata)

render("index.Rmd", html_document(
  theme = NULL,
  template = "templates/index.html",
  pandoc_args = c("--metadata-file", pandoc_metadata)),
  output_dir = "docs/")

# for (i in post_rmds) {
#   render(i, html_document(
#     theme = NULL,
#     template = here("templates", "post.html")
#   ), output_dir = "docs/")
# }

post_rmds %>%
  lapply(render, html_document(
    theme = NULL,
    template = here("templates", "post.html")
  ), output_dir = "docs/")

