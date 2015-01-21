meta = c("Denmark", "Folketing")
mode = "fruchtermanreingold"

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

for(ii in themes) { # rev(sort(unique(d$legislature)))
  
  cat(ifelse(nchar(ii) > 1, ii, c("1" = "2001-2004", # not used, missing 3 years
                                  "2" = "2005-2007", 
                                  "3" = "2007-2011",
                                  "4" = "2011-2015")[ ii ]))
  
  if(nchar(ii) > 1)
    data = subset(d, grepl(ii, theme))
  else
    data = subset(d, legislature == ii)
  
  data = subset(data, n_au > 1)
  
  cat(":", nrow(data), "cosponsored documents, ")

  # check for missing sponsors
  u = unlist(strsplit(data$links, ";"))
  u = na.omit(u[ !u %in% s$url ])
  if(length(u)) {
    cat("Missing", length(u), "sponsors:")
    print(table(u))
  }
  
  # reset row names (changed when setting vertex attributes)
  rownames(s) = s$url
  
  #
  # directed edge list
  #
  
  edges = bind_rows(lapply(data$links, function(i) {
    
    w = unlist(strsplit(i, ";"))
    d = s[ w, "name" ]
    
    d = expand.grid(i = d, j = d[ 1 ], stringsAsFactors = FALSE)
    
    return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    
  }))
  
  #
  # edge weights
  #
  
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
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # final edge set: cosponsor, first author, weights
  edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]
  
  cat(nrow(edges), "edges, ")
  
  #
  # directed network
  #
  
  n = network(edges[, 1:2 ], directed = TRUE)
  
  n %n% "country" = meta[1]
  n %n% "title" = paste(meta[2], paste0(range(unique(substr(data$year, 1, 4))),
                                        collapse = " to "))
  
  n %n% "n_bills" = nrow(data)
  
  if(nchar(ii) > 1)
    n %n% "n_sponsors" = table(subset(d, grepl(ii, theme))$n_au)
  else
    n %n% "n_sponsors" = table(subset(d, legislature == ii)$n_au)
  
  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  rownames(s) = s$name
  
  n %v% "born" = as.numeric(s[ network.vertex.names(n), "born" ])
  n %v% "sex" = as.character(s[ network.vertex.names(n), "sex" ])
  
  if(nchar(ii) == 1) {
    
    s$nyears = sapply(s$mandate, function(x) {
      sum(unlist(strsplit(x, ";")) <= 
            min(substr(names(legislature)[ legislature == ii ], 1, 4)))
    })
    
    n %v% "nyears" = s[ network.vertex.names(n), "nyears" ]
    # print(table(n %v% "nyears"))
    
  }
  
  n %v% "party" = as.character(s[ network.vertex.names(n), "party" ])
  n %v% "partyname" = as.character(s[ network.vertex.names(n), "partyname" ])
  n %v% "lr" = as.numeric(scores[ n %v% "party" ])
  n %v% "url" = as.character(s[ network.vertex.names(n), "url" ])
  n %v% "photo" = as.character(s[ network.vertex.names(n), "photo" ])
  n %v% "constituency" = as.character(s[ network.vertex.names(n), "constituency" ])
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
  
  #
  # weighted measures
  #
  
  n = get_modularity(n, weights = "raw")
  n = get_modularity(n, weights = "nfw")
  n = get_modularity(n, weights = "gsw")
  
  n = get_centrality(n, weights = "raw")
  n = get_centrality(n, weights = "nfw")
  n = get_centrality(n, weights = "gsw")
  
  #
  # network plot
  #
    
  if(plot) {
    
    q = n %v% "degree"
    q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
    
    ggnet_save(n, file = ifelse(nchar(ii) > 1,
                                paste0("plots/net_dk", gsub("(\\w)\\|(.*)", "\\1", ii)),
                                paste0("plots/net_dk", paste0(range(substr(data$year, 1, 4)), collapse = "-"))),
               i = colors[ s[ n %e% "source", "party" ] ],
               j = colors[ s[ n %e% "target", "party" ] ],
               q, colors, order)
    
  }
  
  #
  # save objects (legislatures only)
  #
  
  if(nchar(ii) == 1) {
    
    assign(paste0("net_dk", substr(min(data$year), 1, 4)), n)
    assign(paste0("edges_dk", substr(min(data$year), 1, 4)), edges)
    assign(paste0("bills_dk", substr(min(data$year), 1, 4)), data)
    
  }
  
  #
  # export gexf (themes only)
  #
  
  if(gexf & nchar(ii) > 1)
    get_gexf(paste0("net_dk", gsub("(\\w)\\|(.*)", "\\1", ii)),
             n, meta, mode, colors, extra = "constituency")
  
}

save(list = ls(pattern = "^(net|edges|bills)_dk\\d{4}$"), file = "data/net_dk.rda")

# zip GEXF graphs (themes only)
if(gexf)
  zip("net_dk.zip", dir(pattern = "^net_\\w+\\.gexf$"))
