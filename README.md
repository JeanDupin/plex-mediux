# `plexapi`: R package to update Plex posters from MediUX yaml

`plexapi` is an **R package** that links your [MediUX](https://github.com/mediUX/mediUX) YAML files with your Plex server and automatically updates the posters for your movies and TV shows.

This project is inspired by:

- [plex-mediUX-title-cards](https://github.com/RiyeUK/plex-mediUX-title-cards), which automates applying MediUX title cards to Plex using Python and plexapi.
- The [plexapi](https://github.com/pkkid/python-plexapi) Python library, which provides rich functionality to interact with Plex servers.

---

## Features

- Read MediUX YAML configuration files describing artwork for movies or TV shows.
- Connect to your Plex server to locate the relevant library items.
- Automatically update posters (and backgrounds) in Plex based on the definitions in the YAML files.

---

## Installation

```r
# install.packages("devtools")
devtools::install_github("jeandupin/plex-mediux")
```

---

## Basic usage

### 1. Set your Plex connection options
Before using the functions, set your Plex server URL and authentication token :

```r
options(
  "x-plex-token" = "loremipsum123456",
  "plex-url"     = "http://111.222.333.444:32400"
)
```

### 2. Prepare your YAML file
You just need to copy-paste the YAMLs you get from the [MediUX](https://github.com/mediUX/mediUX) into a single `.yaml` file on your computer.

For example: 

```yaml
  73244: # TVDB id for The Office. Set by willtong93 on MediUX. https://mediux.pro/sets/14124
    url_poster: https://api.mediux.pro/assets/1ffe6b88-9e07-4f31-b675-9a9bb39aa7a8
    url_background: https://api.mediux.pro/assets/df10f480-0c0d-4998-9314-221093f637c9
    seasons:
      0:
        url_poster: https://api.mediux.pro/assets/049faa3f-fa25-4179-b4f1-03678d39a4b7
      1:
        url_poster: https://api.mediux.pro/assets/2e1839c2-f4bd-4472-900b-68aa8f415bff
        episodes:
          1:
            url_poster: https://api.mediux.pro/assets/e1555bdf-e9a5-4ef0-af19-19bcd11df0e9
          2:
            url_poster: https://api.mediux.pro/assets/9d977e87-a5d6-48b1-952f-29b8ffec2cca
          3:
            url_poster: https://api.mediux.pro/assets/3184f088-7cb9-40ad-a5c6-64c32eacaa34
          4:
            url_poster: https://api.mediux.pro/assets/42bfcbde-3c69-4652-a571-656221c227e5
          5:
            url_poster: https://api.mediux.pro/assets/f37fb9ff-9cb1-4f02-8231-0033ab3c7bcc
          6:
            url_poster: https://api.mediux.pro/assets/6e58ebf4-f0df-40e2-91be-c9668a69da80
  671: # Harry Potter and the Philosopher's Stone (2001) Poster by r3draid3r04 on MediUX. https://mediux.pro/sets/29090
    url_poster: https://api.mediux.pro/assets/43664402-c112-40b8-b8ec-f0c1b166b4cc
    url_background: https://api.mediux.pro/assets/594c5a35-cc4d-48dc-83bf-8efe97609cec
    
```

### 3. Upload posters
Then simply call:

```r
library(plexapi)
upload_posters("your_file.yaml")
```

The package will parse the YAML file, match TVDB/TMDB index on your Plex server, and update the posters accordingly.


