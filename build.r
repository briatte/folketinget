themes = c(2:4, # full legislatures, excluding the first (very incomplete) one
           "Agriculture|Fisheries|Food|Land",
           "Culture",
           "Economy|Taxation|Trade|Consumer Affairs|Employment",
           "Education|Science|Higher Education",
           "Environment|Energy|Climate|Transport",
           "Foreign Affairs|Defence|Development",
           "Health",
           "Housing|Building",
           "Immigration|Integration|Refugees",
           "Institutional",
           "Justice|Interior",
           "Technology|Innovation",
           "Welfare|Social Affairs|Social Security|Family|Children|Gender Equality")

# Danish MPs can submit both bills or 'motions', i.e. bills that are
# not fully drafted and that are quicker to submit; both are used in
# the graphs. Resolutions do not have keywords (no ministerial area)
# and are excluded from both types of graphs.
d = subset(d, type != "resolution")

for (ii in themes) { # rev(sort(unique(d$legislature)))
  
  cat(ifelse(nchar(ii) > 1, ii, c("1" = "2001-2004", # not used, missing 3 years
                                  "2" = "2005-2007", 
                                  "3" = "2007-2011",
                                  "4" = "2011-2015")[ ii ]))
  
  if (nchar(ii) > 1)
    data = subset(d, grepl(ii, theme))
  else
    data = subset(d, legislature == ii)
  
  data = subset(data, n_au > 1)
  
  cat(":", nrow(data), "cosponsored documents, ")

  # check for missing sponsors
  u = unlist(strsplit(data$links, ";"))
  u = na.omit(u[ !u %in% s$url ])
  if (length(u)) {
    cat("Missing", length(u), "sponsors:")
    print(table(u))
  }
  
  # reset row names (changed when setting vertex attributes)
  s = data.frame(s, row.names = s$url)

  # ============================================================================
  # DIRECTED EDGE LIST
  # ============================================================================
  
  edges = lapply(data$links, function(i) {
    
    w = unlist(strsplit(i, ";"))
    d = s[ w, "name" ]
    
    d = expand.grid(i = d, j = d[ 1 ], stringsAsFactors = FALSE)
    
    return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    
  }) %>% bind_rows
  
  # ============================================================================
  # EDGE WEIGHTS
  # ============================================================================
  
  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)
  
  # count number of bills per first author
  n_au = table(self$j)
  
  # remove self-loops from directed edge list
  edges = subset(edges, i != j)
  
  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)
  
  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
  
  # raw edge counts
  raw = table(edges$ij)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
  
  # expand to edge list
  edges = data_frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # sanity check
  stopifnot(edges$gsw <= 1)
  
  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)
  
  cat(nrow(edges), "edges, ")
  
  # ============================================================================
  # DIRECTED NETWORK
  # ============================================================================
  
  n = network(edges[, 1:2 ], directed = TRUE)
  
  n %n% "country" = meta[ "cty" ] %>% as.character
  n %n% "lang" = meta[ "lang" ] %>% as.character
  
  if(nchar(ii) == 1)
    n %n% "years" = c("1" = "2001-2004", # not used, missing 3 years
                      "2" = "2005-2007", 
                      "3" = "2007-2011",
                      "4" = "2011-2015")[ ii ] %>% as.character
  
  n %n% "legislature" = NA
  n %n% "chamber" = meta[ "ch" ] %>% as.character
  n %n% "type" = meta[ "type" ] %>% as.character
  n %n% "ipu" = meta[ "ipu" ] %>% as.integer
  n %n% "seats" = meta[ "seats" ] %>% as.integer
  
  n %n% "n_cosponsored" = nrow(data)
  
  if (nchar(ii) > 1)
    n %n% "n_sponsors" = table(subset(d, grepl(ii, theme))$n_au)
  else
    n %n% "n_sponsors" = table(subset(d, legislature == ii)$n_au)

  # ============================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ============================================================================

  n_au = as.vector(n_au[ network.vertex.names(n) ])

  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  rownames(s) = s$name
  
  n %v% "born" = s[ network.vertex.names(n), "born" ]
  n %v% "sex" = s[ network.vertex.names(n), "sex" ]
  
  if (nchar(ii) == 1) {
    
    s$nyears = sapply(s$mandate, function(x) {
      sum(unlist(strsplit(x, ";")) <= 
            min(substr(names(legislature)[ legislature == ii ], 1, 4)))
    })
    
    n %v% "nyears" = s[ network.vertex.names(n), "nyears" ] %>% as.integer
    # print(table(n %v% "nyears"))
    
  }
  
  n %v% "party" = s[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
  n %v% "url" = paste0("http://www.ft.dk/folketinget/findmedlem/",
                       s[ network.vertex.names(n), "url" ], ".aspx")
  n %v% "photo" = s[ network.vertex.names(n), "photo" ]
  n %v% "constituency" = s[ network.vertex.names(n), "constituency" ]
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
    
  # ============================================================================
  # SAVE PLOTS
  # ============================================================================
    
  if (plot) {
    
    save_plot(n, ifelse(nchar(ii) > 1,
                        paste0("plots/net_dk", gsub("(\\w)\\|(.*)", "\\1", ii)),
                        paste0("plots/net_dk", paste0(range(substr(data$year, 1, 4)), collapse = "-"))),
              i = colors[ s[ n %e% "source", "party" ] ],
              j = colors[ s[ n %e% "target", "party" ] ],
              mode, colors)
    
  }
  
  # ============================================================================
  # SAVE OBJECTS (legislatures only)
  # ============================================================================
  
  if (nchar(ii) == 1) {
    
    assign(paste0("net_dk", substr(min(data$year), 1, 4)), n)
    assign(paste0("edges_dk", substr(min(data$year), 1, 4)), edges)
    assign(paste0("bills_dk", substr(min(data$year), 1, 4)), data)
    
  }
  
  # ============================================================================
  # SAVE GEXF (themes only)
  # ============================================================================
  
  if (gexf & nchar(ii) > 1)
    save_gexf(n, paste0("net_dk", gsub("(\\w)\\|(.*)", "\\1", ii)), mode, colors)
  
}

# zip GEXF graphs (themes only)
if (gexf)
  zip("net_dk.zip", dir(pattern = "^net_\\w+\\.gexf$"))
