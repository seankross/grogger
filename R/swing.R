#' Build Grogger Blog
#'
#' @param index Path to the `index.Rmd` file.
#' @param posts Path to the directory that contains the blog posts.
#' @param tags Create a page that collates posts according to tags?
#' @param output_dir Path to the directory where the output files will be
#' created.
#' @importFrom here here
#' @importFrom fs dir_ls dir_create path file_info
#' @importFrom rmarkdown render pandoc_available yaml_front_matter html_document
#' @importFrom cli cli_alert_info cli_alert_success
#' @importFrom yaml as.yaml
#' @export
swing <- function(index = here("index.Rmd"),
                  posts = here("posts"),
                  tags = TRUE,
                  output_dir = here("docs")) {
  
  # swing() builds the entire site, which at minimumn is composed of the 
  # homepage and posts. # Optionally the site can include a page of tags, and 
  # pages that list posts that include a specific tag. Eventually, I would like
  # to include the option to build an atom feed.

  ###############
  # Build posts #
  ###############
  
  # Paths to the posts
  post_rmds <- dir_ls(posts, regexp = "*.[R|r]md$")

  # The yamls for each post, in date order from newest to oldest. 
  posts_date_desc <- post_rmds %>%
    lapply(yaml_front_matter) %>%
    (function(x) x[order(sapply(x, function(y) y$date), decreasing = TRUE)])() %>%
    (function(x) Map(function(rmd, i) c(i, list(rmd = rmd)), names(x), x))() %>%
    (function(x) Map(function(rmd, i) {
      c(i, list(link = path(basename(posts), strip_ext(basename(rmd)))))
    }, names(x), x))() %>%
    unname()

  # Each page on the site has the same nav, which is set in the index.Rmd file.
  # We need to "inject" the info for this nav into each post rmd, which we can 
  # do via the --metadata-file pandoc arg.
  nav_yaml <- yaml_front_matter(index)["nav"] %>% as.yaml()
  nav_metadata <- tempfile(fileext = ".yaml")
  writeLines(nav_yaml, nav_metadata)
  
  # Build all the posts
  posts_built <- 0
  for (i in seq_len(length(posts_date_desc))) {
    post <- posts_date_desc[[i]]
    post_dir <- post[["rmd"]] %>% basename() %>% strip_ext()
    post_html <- path(output_dir, "posts", post_dir, "index.html")
    temp_post_html <- path(posts, post_dir) %>% paste0(".html")
    
    if (!file.exists(post_html) || is_older(post_html, post[["rmd"]])) {
      posts_built <- posts_built + 1
      fs::dir_create(dirname(post_html), recurse = TRUE)
      render(post[["rmd"]], html_document(
        theme = NULL,
        template = "inst/pandoc-templates/post.html" %>% here(), # TODO
        pandoc_args = c("--metadata-file", nav_metadata),
        self_contained = pandoc_available("2.8"),
        md_extensions = "-autolink_bare_uris"), quiet = TRUE)
      delete_then_move(temp_post_html, post_html)
    }
  }

  if (posts_built < 1) {
    cli_alert_info("No posts were built.")
  } else {
    post_s <- ifelse(posts_built == 1, 'post', 'posts')
    cli_alert_success("Built {posts_built} {post_s}.")
  }

  ##################
  # Build homepage #
  ##################
  
  # Similar strategy here as what we did with the nav. We're going to inject
  # metadata for all of the posts into the homepage rmd, and we will use that
  # information to knit the homepage.
  posts_yaml <- list(posts = posts_date_desc) |> as.yaml()
  pandoc_metadata <- tempfile(fileext = ".yaml")
  writeLines(posts_yaml, pandoc_metadata)

  temp_home_html <- sub("\\.[^.]*$", "", index) %>% paste0(".html")
  home_html <- path(output_dir, "index.html")

  if (!file.exists(home_html) || is_older(home_html, index) || posts_built > 0) {
    render(index, html_document(
      theme = NULL,
      template = "inst/pandoc-templates/index.html" %>% here(), # TODO
      pandoc_args = c("--metadata-file", pandoc_metadata),
      self_contained = pandoc_available("2.8"),
      md_extensions = "-autolink_bare_uris"), quiet = TRUE)

    delete_then_move(temp_home_html, home_html)
    cli_alert_success("Homepage was built.")
  } else {
    cli_alert_info("Homepage was not built.")
  }

  ##################
  # Build tag page #
  ##################

  # Exit early it we're not building tags
  if (!tags) {
    return(invisible(output_dir))
  }

  # Build the list of tags that contain info about each post
  tag_list <- list()
  for (i in seq_len(length(posts_date_desc))) {
    post <- posts_date_desc[[i]]

    for (j in post[["tags"]]) {
      if (!(j %in% names(tag_list))) {
        tag_list[[j]] <- list(post)
      } else {
        tag_list[[j]] <- c(tag_list[[j]], list(post))
      }
    }

  }

  # Buld the page that lists all tags
}
