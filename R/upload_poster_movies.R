#' Upload posters for movies from MediUX YAML files
#'
#' @param input The `data.frame` of movies.
#'
#' @return Prints a message for each movie poster indicating whether it was successfully uploaded.
#'
#' @keywords internal
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
    httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
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
    httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
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
      httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
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
      })() |>
      (\(.){
        ifelse(length(.) == 0,"000",.)
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
      by = "tmdb",
      sort = F
    )

  # Get the titles for verbose
  for(i in seq_len(nrow(movies.db))){
    movies.db[i,"title"] <-
      paste0(plex.url,"/library/metadata/",movies.db[i,"rk"]) |>
      httr2::request() |>
      httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
      httr2::req_perform() |>
      httr2::resp_body_xml() |>
      (\(.){xml2::as_list(.)[[1]][[1]]})() |>
      (\(.){attr(.,"title")})()
  }

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

  movies.db.poster <-
    movies.db[!is.na(movies.db$poster),]
  movies.db.bg <-
    movies.db[!is.na(movies.db$bg),]

  if(nrow(movies.db.poster) >= 1){
    cat("Uploading posters...\n")
    max.title.size = max(nchar(movies.db.poster$title))+1
    movies.db.poster$title <- sprintf("%-*s", max.title.size, movies.db.poster$title)
    for(i in seq_len(nrow(movies.db.poster))){
      cat("\r", i, "/",nrow(movies.db.poster)," - ",
          movies.db.poster[i,"title"],sep = "")
      paste0(plex.url,"/library/metadata/",
             movies.db.poster[i,"rk"],"/posters?url=",movies.db.poster[i,"poster"]) |>
        httr2::request() |>
        httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
        httr2::req_method("POST") |>
        httr2::req_perform(verbosity = 0)
    }
    cat("\n")
  }

  if(nrow(movies.db.bg) >= 1){
    cat("Uploading backgrounds...\n")
    max.title.size = max(nchar(movies.db.bg$title))+1
    movies.db.bg$title <- sprintf("%-*s", max.title.size, movies.db.bg$title)
    for(i in seq_len(nrow(movies.db.bg))){
      cat("\r", i, "/",nrow(movies.db.bg)," - ",
          movies.db.bg[i,"title"],sep = "")
      paste0(plex.url,"/library/metadata/",
             movies.db.bg[i,"rk"],"/arts?url=",movies.db.bg[i,"bg"]) |>
        httr2::request() |>
        httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
        httr2::req_method("POST") |>
        httr2::req_perform(verbosity = 0)
    }
    cat("\n")
  }


}
