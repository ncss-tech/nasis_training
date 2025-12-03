## quarto post-render hook

# flattens source/ structure into content/ and cleans up build artifacts
{
  f <- list.files("_site/source", full.names = TRUE, recursive = TRUE)
  dir.create("content/", showWarnings = FALSE)
  file.copy(f, "content/")
  file.remove(f)
  f2 <- list.files("_site", full.names = TRUE, recursive = TRUE)
  sapply(unique(dirname(gsub("_site/", "", f2))), dir.create, recursive = TRUE, showWarnings = FALSE)
  file.rename(f2, gsub("_site/", "", f2))
  x <- readLines("index.html")
  x <- gsub("source/", "content/", x)
  writeLines(x, "index.html")
  x <- readLines("search.json", warn = FALSE)
  x <- gsub("source/", "content/", x)
  writeLines(x, "search.json")
  unlink("_site", recursive = TRUE)
}
