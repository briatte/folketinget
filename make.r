# hi Denmark

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

if (file.exists("photos.zip"))
  unzip("photos.zip")

dir.create("photos", showWarnings = FALSE)

if (file.exists("raw.zip"))
  unzip("raw.zip")

dir.create("raw"            , showWarnings = FALSE)
dir.create("raw/bill-lists" , showWarnings = FALSE)
dir.create("raw/bill-pages" , showWarnings = FALSE)
dir.create("raw/mp-pages"   , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE # thematic graphs only
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Denmark",
  "lang" = "da", # Wikipedia language for chamber and constituencies
  "ch" = "Folketinget",
  "type" = "Unicameral",
  "ipu" = 2087,
  "seats" = 179
)

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

save(list = ls(pattern = "^(co)?(net|edges|bills)_dk\\d{4}$"),
     file = "data/net_dk.rda")

# have a nice day
