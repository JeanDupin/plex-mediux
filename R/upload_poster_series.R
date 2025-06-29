#' Upload posters for TV series from MediUX YAML files
#'
#' @param input The `.yaml` file containing the MediUX shows as a `list` format.
#'
#' @return Prints a message for each series poster indicating whether it was successfully uploaded.
#'
#' @keywords internal
upload_poster_series <- function(input){

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
  cat("Loading TV Shows database...")
  id.section <-
    paste0(plex.url,"/library/sections/all") |>
    httr2::request() |>
    httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
    httr2::req_perform() |>
    httr2::resp_body_xml() |>
    xml2::as_list() |>
    (\(.z){
      sapply(.z[[1]],function(.x){attr(.x,"type")}) |>
        (\(.){which(. == "show")})() |>
        (\(.){attr(.z[[1]][[.]],"key")})()
    })()

  # Extract all the shows ratingKey
  series.RK <-
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

  # Change RK to TVDB
  series.GUID <- vector("character", length = length(series.RK))
  for(i in seq_along(series.RK)){
    if(i %% 10 == 0 | i == length(series.RK)){
      cat("\rLoading shows database... ", i, "/",length(series.RK), sep = "")
    }
    series.GUID[i] <-
      paste0(plex.url,"/library/metadata/",series.RK[i]) |>
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
        gsub("[^0-9]","",.[grepl("tvdb",.)])
      })() |>
      (\(.){
        ifelse(length(.) == 0,"000",.)
      })()
  }
  cat("\n")

  series.db <-
    data.frame(
      rk = series.RK,
      tvdb = series.GUID,
      row.names = NULL
    )

  # Keep the RK requested, then add posters and background
  series.db <-
    series.db[series.db$tvdb %in% names(input),]

  if(nrow(series.db) < length(input)){
    not_found <-
      names(input)[!names(input) %in% series.db$tvdb]
    warning(paste0("Some shows requested were not found : ",not_found,"."),
            call. = F)
  }

  series.db <-
    input[names(input) %in% series.db$tvdb] |>
    (\(.){
      data.frame(
        tvdb = names(.),
        poster = sapply(.,function(.x){
          ifelse(is.null(.x[["url_poster"]]),NA,.x[["url_poster"]])
        },simplify = T,USE.NAMES = F),
        bg = sapply(.,function(.x){
          ifelse(is.null(.x[["url_background"]]),NA,.x[["url_background"]])
        },simplify = T,USE.NAMES = F),
        row.names = NULL
      )
    })() |>
    (\(.){
      merge(
        series.db,
        .,
        by = "tvdb"
      )
    })()

  # Add title to series
  for(i in seq_len(nrow(series.db))){
    series.db[i,"title"] <-
      paste0(plex.url,"/library/metadata/",series.db[i,"rk"]) |>
      httr2::request() |>
      httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
      httr2::req_perform() |>
      httr2::resp_body_xml() |>
      xml2::as_list() |>
      (\(.){attr(.[[1]][[1]],"title")})()
  }


  # Get the seasons for the series
  seasons.db <-
    vector("list", nrow(series.db))
  for(i in seq_len(nrow(series.db))){
    seasons.db[[i]] <-
      merge(
        input[[series.db[i,"tvdb"]]]$seasons |>
          (\(.){
            data.frame(
              tvdb = series.db[i,"tvdb"],
              index = names(.),
              poster = sapply(.,function(.x){.x[["url_poster"]]},USE.NAMES = F,simplify = T)
            )
          })(),
        paste0(plex.url,"/library/metadata/",series.db[i,"rk"],"/children") |>
          httr2::request() |>
          httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
          httr2::req_perform() |>
          httr2::resp_body_xml() |>
          (\(.){xml2::as_list(.)[[1]][-1]})() |>
          lapply(function(.x){
            data.frame(
              "index" = attr(.x,"index"),
              "rk" = attr(.x,"ratingKey"),
              "title" = paste0(attr(.x,"parentTitle")," - ",
                               "Season ", attr(.x,"index"))
            )
          }) |>
          (\(.){do.call(rbind,.)})(),
        by = "index"
      ) |>
      (\(.){.[!is.na(.$poster),]})()
  }
  seasons.db <-
    do.call(rbind, seasons.db)

  # Get the episodes for the series
  episodes.db <- vector("list", nrow(seasons.db))
  for(i in seq_len(nrow(seasons.db))){
    episodes.temp <-
      input[[seasons.db[i,"tvdb"]]][["seasons"]][[seasons.db[i,"index"]]]$episodes
    if(is.null(episodes.temp)){
      next
    }
    episodes.db[[i]] <-
      merge(
        episodes.temp |>
          (\(.) {
            data.frame(
              tvdb = seasons.db[i, "tvdb"],
              index = names(.),
              poster = sapply(., function(.x) {
                .x[["url_poster"]]
              }, USE.NAMES = F, simplify = T)
            )
          })(),
        paste0(plex.url,"/library/metadata/",seasons.db[i,"rk"],"/children") |>
          httr2::request() |>
          httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
          httr2::req_perform() |>
          httr2::resp_body_xml() |>
          (\(.){xml2::as_list(.)[[1]]})() |>
          lapply(function(.x){
            data.frame(
              "index" = attr(.x,"index"),
              "rk" = attr(.x,"ratingKey"),
              "title" = paste0(attr(.x,"grandparentTitle")," - ",
                               attr(.x,"parentTitle"), " - ",
                               "Episode ", attr(.x,"index"))
            )
          }) |>
          (\(.){do.call(rbind,.)})(),
        by = "index"
      ) |>
      (\(.){.[order(as.numeric(.$index)),]})()
  }
  episodes.db <-
    do.call(rbind, episodes.db)

  # Upload all the posters
  # Serie poster and background
  for(i in series.db$tvdb){
    poster = series.db[series.db$tvdb == i,"poster"]
    if(!is.na(poster)){
      cat(paste0("Uploading ",series.db[series.db$tvdb == i,"title"]," poster..."))
      paste0(plex.url,"/library/metadata/",
             series.db[series.db$tvdb == i,"rk"],"/posters?url=",poster) |>
        httr2::request() |>
        httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
        httr2::req_method("POST") |>
        httr2::req_perform(verbosity = 0)
      cat(" OK\n")
    }
    bg = series.db[series.db$tvdb == i,"bg"]
    if(!is.na(bg)){
      cat(paste0("Uploading ",series.db[series.db$tvdb == i,"title"]," background..."))
      paste0(plex.url,"/library/metadata/",
             series.db[series.db$tvdb == i,"rk"],"/arts?url=",bg) |>
        httr2::request() |>
        httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
        httr2::req_method("POST") |>
        httr2::req_perform(verbosity = 0)
      cat(" OK\n")
    }
  }
  # Seasons posters
  if(!is.null(seasons.db)){
    for(i in seq_len(nrow(seasons.db))){
      cat(paste0("Uploading ",seasons.db[i,"title"]," poster..."))
      paste0(plex.url,"/library/metadata/",
             seasons.db[i,"rk"],"/posters?url=",poster) |>
        httr2::request() |>
        httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
        httr2::req_method("POST") |>
        httr2::req_perform(verbosity = 0)
      cat(" OK\n")
    }
  }
  # Episodes posters
  if(!is.null(episodes.db)){
    for(i in seq_len(nrow(episodes.db))){
      cat(paste0("Uploading ",episodes.db[i,"title"]," poster..."))
      paste0(plex.url,"/library/metadata/",
             episodes.db[i,"rk"],"/posters?url=",poster) |>
        httr2::request() |>
        httr2::req_headers_redacted("X-Plex-Token" = plex.token) |>
        httr2::req_method("POST") |>
        httr2::req_perform(verbosity = 0)
      cat(" OK\n")
    }
  }
}
