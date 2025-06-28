#' Upload posters from the mediux files
#'
#' @param file The .yaml file with the MediUX posters.
#'
#' @return Verbatim for each poster if it uploaded or not
#' @export
#'
#' @examples
#' \dontrun{
#' upload_poster("test.yaml")
#' }
upload_poster <- function(file){
  # Cheks on the .yaml file
  if(!file.exists(file)){
    stop("File does not exist.",
         call. = F)
  }
  if(!grepl("\\.yaml$|\\.yml",file)){
    stop("Use a .yaml or .yml file.",
         call. = F)
  }

  # File
  fichier <-
    yaml::read_yaml(file)

  # Movies
  movies <-
    sapply(fichier,function(.x){"seasons" %in% names(.x)},
           USE.NAMES = F, simplify = T) |>
    (\(.){fichier[!.]})()

  if(length(movies) >= 1){
    movies <-
      data.frame(
        tmdb = names(movies),
        poster = sapply(movies,function(.x){
          ifelse(is.null(.x[["url_poster"]]),NA,.x[["url_poster"]])
        },simplify = T,USE.NAMES = F),
        bg = sapply(movies,function(.x){
          ifelse(is.null(.x[["url_background"]]),NA,.x[["url_background"]])
        },simplify = T,USE.NAMES = F),
        row.names = NULL
      )

    upload_poster_movies(movies)
    cat("Movies done.\n")
  }

  # Shows
  shows <-
    sapply(fichier,function(.x){"seasons" %in% names(.x)},
           USE.NAMES = F, simplify = T) |>
    (\(.){fichier[.]})()


}
