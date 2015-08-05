# hi Denmark

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"   , showWarnings = FALSE)
dir.create("photos" , showWarnings = FALSE)
dir.create("plots"  , showWarnings = FALSE)
dir.create("raw"    , showWarnings = FALSE)

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

# have a nice day
