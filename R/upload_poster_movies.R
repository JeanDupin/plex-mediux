#' Generic function to upload the movies posters
#'
#' @param input A data.frame of movies and poster url
#'
#' @return The verbatim
upload_poster_movies <- function(input){

  # Plex parameters (Token & Url)
  if(is.null(getOption("plex-url"))){
    stop("Set your Plex URL using `options('plex-url' = 'http://111.222.333.444:32400')`",
         call. = F)
  }
  if(is.null(getOption("x-plex-token"))){
    stop("Set your Plex token using `options('x-plex-token' = 'loremipsum123456')`",
         call. = F)
  }
  plex.url <- getOption("plex-url")
  plex.token <- getOption("x-plex-token")

  # Find movie section id
  cat("Loading movies database...")
  id.section <-
    paste0(plex.url,"/library/sections/all") |>
    httr2::request() |>
    httr2::req_headers("X-Plex-Token" = plex.token) |>
    httr2::req_perform() |>
    httr2::resp_body_xml() |>
    xml2::as_list() |>
    (\(.z){
      sapply(.z[[1]],function(.x){attr(.x,"type")}) |>
        (\(.){which(. == "movie")})() |>
        (\(.){attr(.z[[1]][[.]],"key")})()
    })()

  # Extract all the movies ratingKey
  movies.RK <-
    paste0(plex.url,"/library/sections/",id.section,"/all") |>
    httr2::request() |>
    httr2::req_headers("X-Plex-Token" = plex.token) |>
    httr2::req_perform() |>
    httr2::resp_body_xml() |>
    xml2::as_list() |>
    (\(.){
      sapply(.[[1]],function(.x){attr(.x,'ratingKey')},
             simplify = T, USE.NAMES = FALSE)
    })()

  # Change RK to TMDB
  movies.GUID <- vector("character", length = length(movies.RK))
  for(i in seq_along(movies.RK)){
    if(i %% 10 == 0 | i == length(movies.RK)){
      cat("\rLoading movies database... ", i, "/",length(movies.RK), sep = "")
    }
    movies.GUID[i] <-
      paste0(plex.url,"/library/metadata/",movies.RK[i]) |>
      httr2::request() |>
      httr2::req_headers("X-Plex-Token" = plex.token) |>
      httr2::req_perform() |>
      httr2::resp_body_xml() |>
      xml2::as_list() |>
      (\(.){
        .[[1]][[1]][which(names(.[[1]][[1]]) == "Guid")]
      })() |>
      (\(.){
        sapply(.,function(.x){attr(.x,"id")},
               simplify = T,USE.NAMES = F)
      })() |>
      (\(.){names(.) <- NULL; .})() |>
      (\(.){
        gsub("[^0-9]","",.[grepl("tmdb",.)])
      })()
  }
  cat("\n")

  movies.db <-
    data.frame(
      rk = movies.RK,
      tmdb = movies.GUID
    )

  # Keep the RK requested
  movies.db <-
    merge(
      movies.db,
      input,
      by = "tmdb"
    )

  # Upload posters
  if(nrow(movies.db) == 0){
    stop("No movies requested found in your database.",
         call. = F)
  }
  if(nrow(movies.db) < nrow(input)){
    not_found <-
      input$tmdb[!(input$tmdb %in% movies.db$tmdb)] |>
      paste(collapse = ", ")
    warning(paste0("Some movies requested were not found : ",not_found,"."),
            call. = F)
  }

  cat("Uploading posters...")
  for(i in seq_len(nrow(movies.db))){
    cat("\rUploading posters... ", i, "/",nrow(movies.db), sep = "")
    paste0(plex.url,"/library/metadata/",
           movies.db[i,"rk"],"/posters?url=",movies.db[i,"poster"]) |>
      httr2::request() |>
      httr2::req_headers("X-Plex-Token" = plex.token) |>
      httr2::req_method("POST") |>
      httr2::req_perform(verbosity = 0)
  }
  cat("\n")

}
