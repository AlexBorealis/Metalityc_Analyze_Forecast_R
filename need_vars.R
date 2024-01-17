api_flashlive <- data.table(read.table("api_flashlive_vars.txt", header = T))[name == "api_ya", value] # API key

host_flashlive <- data.table(read.table("api_flashlive_vars.txt", header = T))[name == "host", value] # Main host

endpoints <- jsonlite::read_json("endpoints.json", simplifyVector = T) # List needed endpoints

locale <- data.table(read.table("locale_vars.txt", header = T)) # Table of locales

standing_type <- data.table(read.table("standing_type_vars.txt", header = T)) # Table of standings