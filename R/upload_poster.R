#' Upload posters from MediUX YAML file
#'
#' @param file The `.yaml` file containing the MediUX poster definitions.
#' @param section Which section of the file you want to update.
#'   By default, all sections are processed. Accepts `"movies"`, `"shows"`, or `"all"`.
#'
#' @return Prints a message for each poster indicating whether it was successfully uploaded.
#' @export
#'
#' @examples
#' \dontrun{
#' upload_poster("test.yaml")
#' }
upload_poster <- function(file,
                          section = "all"){
  # Checks on the .yaml file
  if(!file.exists(file)){
    stop("File does not exist.",
         call. = F)
  }
  if(!grepl("\\.yaml$|\\.yml",file)){
    stop("Use a .yaml or .yml file.",
         call. = F)
  }
  # Check the parameter
  if(is.null(section) | is.na(section) | !(section %in% c("all","movies","shows"))){
    stop('Please set section as one of "all","movies" or "shows".',
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

  if(length(movies) >= 1 & section %in% c("all","movie")){
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
    cat("Movies done.\n\n")
  }

  # Shows
  shows <-
    sapply(fichier,function(.x){"seasons" %in% names(.x)},
           USE.NAMES = F, simplify = T) |>
    (\(.){fichier[.]})()

  if(length(shows) >= 1 & section %in% c("all","shows")){
    upload_poster_series(shows)
    cat("TV Shows done.\n\n")
  }


}
