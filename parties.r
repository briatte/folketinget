# party colors

colors = c(
  "E"   = "#4DAF4A", # Ø   - Enhedslisten                -- green
  "SFP" = "#E41A1C", # F   - Socialistisk Folkeparti     -- red
  "SD"  = "#F781BF", # A   - Socialdemokratiet           -- pink
  "RV" =  "#C51B7D", # B   - Det Radikale Venstre        -- magenta
  "KD"  = "#FF7F00", # K   - Kristendemokraterne         -- orange
  "LA"  = "#FDB462", # I   - Liberal Alliance            -- light orange
  "KFP" = "#B3DE69", # C   - Det Konservative Folkeparti -- light green
  "V"   = "#01665E", # V   - Venstre                     -- dark green/teal
  "DFP" = "#FFFF33", # Z/O - Dansk Folkeparti            -- yellow (Z color)
  "IA"  = "#80B1D3", #     - Inuit Ataqatigiit           -- light blue
  "S"   = "#FB8072", #     - Siumut                      -- light red
  "JF"  = "#A65628", #     - Javnaðarflokkurin           -- brown
  "SF"  = "#377EB8", #     - Sambandsflokkurin           -- blue
  "IND" = "#AAAAAA"  #     - Independent, light grey
)

# ParlGov Left/Right scores

scores = c(
  "E"   = 0.9,
  "SFP" = 2.1,
  "SD"  = 3.8,
  "RV"  = 4.9,
  "KD"  = 5.7,
  "LA"  = 6,
  "KFP" = 7.2,
  "V"   = 7.3,
  "DFP" = 9,
  # Faroe Islands and Greenland
  "IA"  = 1.3,
  "S"   = 3.3,
  "JF"  = 3.3,
  "SF"  = 7.4,
  "IND" = Inf
)

stopifnot(names(colors) == names(scores))
order = names(colors) # keep Faroe Islands and Greenland at the end
