# hi Denmark

dir.create("data", showWarnings = FALSE)
dir.create("photos", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

library(XML)
library(qdap)
library(rgexf)
library(stringr)
library(tnet)
library(plyr)
library(network)
library(sna)
library(GGally)
library(grid)

plot = TRUE
gexf = TRUE # themes only

colors = c(
  # Left
  "E"   = "#4DAF4A", # Ø   -- Enhedslisten                -- green
  "SFP" = "#67001F", # F   -- Socialistisk Folkeparti     -- dark red
  "SD"  = "#E41A1C", # A   -- Socialdemokratiet           -- red
  # Centre
  "RV" =  "#F781BF", # B   -- Det Radikale Venstre        -- pink
  "LA"  = "#FDB462", # I   -- Liberal Alliance            -- light orange
  "KD"  = "#FF7F00", # K   -- Kristendemokraterne         -- orange
  # Right
  "V"   = "#1B9E77", # V   -- Venstre                     -- teal
  "KFP" = "#B3DE69", # C   -- Det Konservative Folkeparti -- light green
  "DFP" = "#FFFF33", # Z/O -- Dansk Folkeparti            -- yellow (Z color)
  # Other
  "IA"  = "#80B1D3", #     -- Inuit Ataqatigiit           -- light blue
  "S"   = "#FB8072", #     -- Siumut                      -- light red
  "JF"  = "#A65628", #     -- Javnaðarflokkurin           -- brown
  "SF"  = "#377EB8", #     -- Sambandsflokkurin           -- blue
  "IND" = "#AAAAAA"  #     -- Independent, light grey
)
order = names(colors)

root = "http://www.ft.dk"
years = rev(sort(c(paste0(2014:2004, 1), paste0(c(2004, 2007, 2010), 2))))

for(type in c("Forslag_til_vedtagelse", "Beslutningsforslag", "Lovforslag")) {
  
  data = data.frame()
  name = ifelse(type == "Beslutningsforslag", "motions", 
                ifelse(type == "Forslag_til_vedtagelse", "resolutions",
                       "bills"))

  for(i in years) {
    
    file = paste0("data/", name, i, ".html")
    
    if(!file.exists(file) & name == "resolutions")
      download.file(paste0("http://www.ft.dk/Dokumenter/Vis_efter_type/", type, ".aspx?session=", i, "&ministerArea=-1&proposer=&caseStatus=-1&startDate=&endDate=&dateRelatedActivity=&sortColumn=&sortOrder=&startRecord=&numberOfRecords=500&totalNumberOfRecords=#dok"),
                    file, mode = "wb", quiet = TRUE)
    else if(!file.exists(file))
      download.file(paste0("http://www.ft.dk/Dokumenter/Vis_efter_type/", type, ".aspx?session=", i, "&caseStatus=-1&ministerArea=-1&committee=&proposedBy=-1&startDate=&endDate=&dateRelatedActivity=&sortColumn=caseNumber&sortOrder=desc&startRecord=&totalNumberOfRecords=&numberOfRecords=500&pageNr=#dok"),
                    file, mode = "wb", quiet = TRUE)
    
    h = htmlParse(file)
    
    urls = xpathSApply(h, "//tr[contains(@onmouseover, 'docListing')]/@onclick")
    urls = gsub("document.location=\\('|'\\);$", "", urls)
    
    if(length(urls)) {
      
      data = rbind(data, readHTMLTable(h, stringsAsFactors = FALSE)[[1]])
      
      file = gsub("html$", "csv", file)
      if(!file.exists(file)) {
        
        links = data.frame()
        
      } else {
        
        links = read.csv(file, stringsAsFactors = FALSE)
        urls = urls[ !urls %in% links$url ]
        
      }
      
      cat("Year", substr(i, 1, 4), "session", substr(i, 5, 5), ":",
          sprintf("%3.0f", length(urls)), name, "left to scrape ")
      
      if(length(urls)) {
        
        for(j in urls) { # c()
          
          h = try(htmlParse(paste0(root, j)))
          if("try-error" %in% class(h)) {
            
            cat("x")
            
          } else {
            
            t = scrubber(xpathSApply(h, "//h1[1]", xmlValue))
            
            l = paste0(scrubber(xpathSApply(h, "//a[contains(@href, 'findMedlem')]/@href")), collapse = ";")
            
            h = scrubber(xpathSApply(h, "//p", xmlValue))
            a = gsub("Af (.*)", "\\1", h[ grepl("^Af ", h) ])
            s = gsub("Resumé (.*)", "\\1", h[ grepl("Resumé", h) ])
            v = h[ grepl("^Vedtagetd", h) ]
            v = paste0(v, h[ grepl("^Forkastet", h) ], collapse = ". ")
            
            h = data.frame(title = t,
                           uid = gsub("/samling/(\\d+)/(vedtagelse|beslutningsforslag|lovforslag)/(L|B|V)(\\d+)/index.htm",
                                      "\\1-\\3\\4", j),
                           ministry = gsub("Ministerområde (.*)", "\\1", h[ grepl("Ministerområde", h) ]),
                           year = gsub("Samling: ([0-9-]+)(.*)", "\\1", h[ grepl("Samling", h) ]),
                           status = gsub("(.*)Status: (.*)", "\\2", h[ grepl("Samling", h) ]),
                           authors = ifelse(length(a), a, NA),
                           links = gsub("/Folketinget/findMedlem/|\\.aspx", "", l),
                           vote = ifelse(length(v), v, NA),
                           summary = ifelse(length(s), s, NA), url = j,
                           stringsAsFactors = FALSE)
            
            links = rbind(links, h)
            cat(".")
            
          }
          
        }
        
      }
      
      write.csv(links, file, row.names = FALSE)
      cat("", sprintf("%3.0f", nrow(links)), "saved\n")
      
    }
    
  }

  write.csv(data, paste0("data/", name, ".csv"), row.names = FALSE)
  
}

d = rbind.fill(lapply(dir("data", pattern = "^(bills|motions|resolutions)\\d+.csv", full.names = TRUE),
                      read.csv, stringsAsFactors = FALSE))

legislature = c("2004-05" = 1, "2005-06" = 2, "2006-07" = 2,
                "2007-08" = 3, "2008-09" = 3, "2009-10" = 3, "2010-11" = 3,
                "2011-12" = 4, "2012-13" = 4, "2013-14" = 4)

d$legislature = legislature[ d$year ]

d$type = ifelse(grepl("-B", d$uid), "motion",
                ifelse(grepl("-V", d$uid), "resolution", "bill"))

d$year[ nchar(d$year) > 7 ] = NA

print(table(d$type, d$legislature, exclude = NULL))

# sponsors

u = unlist(strsplit(d$links, ";"))
u = paste0("/Folketinget/findMedlem/", unique(na.omit(u)), ".aspx")

if(!file.exists("data/medlem.csv")) {
 
  medlem = data.frame()
  l = sapply(1:4, function(j) {
    h = htmlParse(paste0("http://www.ft.dk/Folketinget/searchResults.aspx?letter=ALLE&pageSize=50&pageNr=",
                         j, "#search"))
    xpathSApply(h, "//a[contains(@href, '/findMedlem/')]/@href")
  })
  u = unique(c(unlist(l), u))
  cat("Finding", length(u), "new sponsors\n")
  
} else {
  
  medlem = read.csv("data/medlem.csv", stringsAsFactors = FALSE)
  u = u[ !u %in% medlem$url ]
  cat("Finding", length(u), "missing sponsors\n")
  
}

for(k in rev(u)) {
  
  h = try(htmlParse(paste0(root, k)))
  if("try-error" %in% class(h)) {
    
    cat(" failed", paste0(root, k), "\n")
  
  } else {
    
    cat(sprintf("%3.0f", which(u == k)),
        gsub("Folketinget - ", "",
             scrubber(xpathSApply(h, "//title", xmlValue))), "\n")
    
    img = xpathSApply(h, "//img[contains(@src, '/media/')]/@src")
    medlem = rbind(medlem,
                   data.frame(
                     name = xpathSApply(h, "//meta[@name='Fullname']/@content"),
                     func = xpathSApply(h, "//meta[@name='Function']/@content"),
                     party = xpathSApply(h, "//meta[@name='Party']/@content"),
                     mandate = xpathSApply(h, "//meta[@name='MfPeriod']/@content"),
                     job = xpathSApply(h, "//div[contains(@class, 'person')]/p[2]/strong", xmlValue),
                     photo = ifelse(length(img), img, NA),
                     url = k,
                     bio = xpathSApply(h, "//div[contains(@class, 'tabContent')]/p[1]", xmlValue),
                     stringsAsFactors = FALSE))
    
  }
  
}

write.csv(medlem, "data/medlem.csv", row.names = FALSE)

medlem$mandate[ medlem$mandate == "" ] = "20111,20141"
medlem$from = as.numeric(sapply(strsplit(gsub("(\\d{4})(1|2)", "\\1", medlem$mandate), ","), min))
medlem$to = as.numeric(sapply(strsplit(gsub("(\\d{4})(1|2)", "\\1", medlem$mandate), ","), max))
medlem$nyears = medlem$to - medlem$from + 1

medlem$sex = str_extract(medlem$bio, "(D|d)atter af|(S|s)øn af")
medlem$sex = ifelse(grepl("(D|d)atter af", medlem$sex), "F", "M")
medlem$sex[ !grepl("(D|d)atter af|(S|s)øn af", medlem$bio) ] = NA

# fill in a few missing values
medlem$sex[ is.na(medlem$sex) & 
                 grepl("^(Anne|Annika|Dorrit|Erika|Fatma|Ida|Karin|Linda|Lise|Lykke|Marlene|Mette|Mie|Sanne|Özlem Sara|Pia|Sofia|Stine)", medlem$name) ] = TRUE
medlem$sex[ is.na(medlem$sex) & 
                 grepl("^(Erling|Eyvind|Hans|Jacob|Jens|Jeppe|Johs\\.|Jørgen|Kamal|Kuupik|Niels|Nikolaj|Per|Peter|Thomas|Uffe)", medlem$name) ] = FALSE

medlem$born = str_extract(medlem$bio, "født [0-9\\.]+ [a-z\\.]+ \\d{4}")
medlem$born = sapply(str_extract_all(medlem$born, "[0-9]{4}"), length)
medlem$born[ medlem$born != 1 ] = 0
medlem$born[ medlem$born == 1 ] = str_extract(medlem$bio[ medlem$born == 1 ], "[0-9]{4}")
medlem$born[ medlem$born == 0 ] = NA

medlem$party[ is.na(medlem$party) | medlem$party %in% c("", "Indep") ] = "Independent"
medlem$party[ medlem$party == "Ny Alliance" ] = "Liberal Alliance"

# download photos
for(i in which(!is.na(medlem$photo))) {
  photo = gsub("/Folketinget/findMedlem/(\\w+)\\.aspx", "photos/\\1.jpg", medlem$url[ i ])
  # special cases
  if(grepl("Thor Moger Pedersen", medlem$url[ i ]))   photo = "photos/scSFTMP.jpg"
  if(grepl("Charlotte Sahl-Madsen", medlem$url[ i ])) photo = "photos/scKFCSM.jpg"
  if(!file.exists(photo) | !file.info(photo)$size) {
    try(download.file(paste0(root, "/Folketinget/findMedlem/", gsub("\\s", "%20", medlem$photo[ i ])),
                      photo, mode = "wb", quiet = TRUE), silent = TRUE)
  }
  if(!file.exists(photo) | !file.info(photo)$size) {
    file.remove(photo) # will warn if missing
    medlem$photo[ i ] = NA
  } else {
    medlem$photo[ i ] = gsub("photos/|.jpg$", "", photo)
  }
}

medlem$url = gsub("/Folketinget/findMedlem/|\\.aspx", "", medlem$url)
medlem$url = gsub("\\s", "%20", medlem$url)

medlem$partyname = medlem$party
medlem$party[ medlem$partyname == "Enhedslisten" ] = "E"
medlem$party[ medlem$partyname == "Socialistisk Folkeparti" ] = "SFP"
medlem$party[ medlem$partyname == "Socialdemokratiet" ] = "SD"
medlem$party[ medlem$partyname == "Radikale Venstre" ] = "RV"
medlem$party[ medlem$partyname == "Kristendemokraterne" ] = "KD"
medlem$party[ medlem$partyname == "Liberal Alliance" ] = "LA"
medlem$party[ medlem$partyname == "Venstre" ] = "V"
medlem$party[ medlem$partyname == "Det Konservative Folkeparti" ] = "KFP"
medlem$party[ medlem$partyname == "Dansk Folkeparti" ] = "DFP"
medlem$party[ medlem$partyname == "Inuit Ataqatigiit" ] = "IA"
medlem$party[ medlem$partyname == "Siumut" ] = "S"
medlem$party[ medlem$partyname == "Sambandsflokkurin" ] = "SF"
medlem$party[ medlem$partyname == "Javnaðarflokkurin" ] = "JF"
medlem$party[ medlem$partyname == "Independent" ] = "IND"

cat("Found", nrow(read.csv("data/bills.csv")), "bills",
    nrow(read.csv("data/motions.csv")), "motions",
    nrow(read.csv("data/resolutions.csv")), "resolutions",
    nrow(medlem), "MPs", nrow(d), "texts ")

d$n_au = 1 + str_count(d$links, ";")

d = subset(d, !grepl("minister", d$authors)) # resolutions are included only in legislature graphs

cat(sum(d$n_au > 1), "cosponsored bills\n")

d$theme = d$ministry
d$theme[ d$ministry %in% c("Erhvervs- og Vækstministeriet", "Handels- og Udviklingsministeriet", "Økonomi- og Erhvervsministeriet",  "Økonomi- og Indenrigsministeriet", "Finansministeriet") ] = "Economy"
d$theme[ d$ministry == "Beskæftigelsesministeriet" ] = "Employment"
d$theme[ d$ministry == "Europaministeriet" ] = "EU"
d$theme[ d$ministry == "Forsvarsministeriet" ] = "Defence"
d$theme[ d$ministry == "Indenrigs- og Socialministeriet" ] = "Interior,Social Affairs"
d$theme[ d$ministry == "Indenrigs- og Sundhedsministeriet" ] = "Health"
d$theme[ d$ministry == "Ministeriet for Sundhed og Forebyggelse" ] = "Interior,Health"
d$theme[ d$ministry == "Justitsministeriet" ] = "Justice"
d$theme[ d$ministry == "Kirkeministeriet" ] = "Church"
d$theme[ d$ministry == "Klima- og Energiministeriet" ] = "Climate,Energy"
d$theme[ d$ministry == "Klima-, Energi- og Bygningsministeriet" ] = "Climate,Energy,Building"
d$theme[ d$ministry == "Miljøministeriet" ] = "Environment"
d$theme[ d$ministry == "Kulturministeriet" ] = "Culture" # culture
d$theme[ d$ministry == "Ministeriet for Børn og Undervisning" ] = "Children,Education" # culture
d$theme[ d$ministry == "Ministeriet for Børn, Ligestilling, Integration og Sociale Forhold" ] = "Children,Gender Equality,Integration,Social Affairs" # culture
d$theme[ d$ministry == "Ministeriet for By, Bolig og Landdistrikter" ] = "Housing,Land" # culture
d$theme[ d$ministry == "Ministeriet for Familie- og Forbrugeranliggender" ] = "Family,Consumer Affairs"
d$theme[ d$ministry == "Ministeriet for Flygtninge, Indvandrere og Integration" ] = "Refugees,Immigration,Integration"
d$theme[ d$ministry == "Ministeriet for Fødevarer, Landbrug og Fiskeri" ] = "Food,Agriculture,Fisheries"
d$theme[ d$ministry == "Ministeriet for Forskning, Innovation og Videregående Uddannelser" ] = "Science,Innovation,Higher Education"
d$theme[ d$ministry == "Ministeriet for Videnskab, Teknologi og Udvikling" ] = "Science,Technology,Innovation"
d$theme[ d$ministry == "Ministeriet for Ligestilling" ] = "Gender Equality"
d$theme[ d$ministry == "Ministeriet for Ligestilling og Kirke" ] = "Church,Gender Equality"
d$theme[ d$ministry %in% c("Ministerområde", "Statsministeriet", "Folketinget") ] = "Institutional"
d$theme[ d$ministry == "Skatteministeriet" ] = "Taxation"
d$theme[ d$ministry == "Socialministeriet" ] = "Social Affairs"
d$theme[ d$ministry == "Social- og Integrationsministeriet" ] = "Social Security,Integration"
d$theme[ d$ministry == "Social-, børne- og integrationsministeriet" ] = "Social Affairs,Children,Integration"
d$theme[ d$ministry %in% c("Transportministeriet", "Trafikministeriet") ] = "Transport" # and roads
d$theme[ d$ministry == "Transport- og Energiministeriet" ] = "Transport,Energy"
d$theme[ d$ministry == "Uddannelses- og Forskningsministeriet" ] = "Education,Science"
d$theme[ d$ministry == "Udenrigsministeriet" ] = "Foreign Affairs"
d$theme[ d$ministry == "Undervisningsministeriet" ] = "Education"
d$theme[ d$ministry == "Velfærdsministeriet" ] = "Welfare"

print(table(unlist(strsplit(d$theme, ","))))

themes = c(1:4, # full legislatures
           "Economy|Taxation|Consumer Affairs|Employment",
           "Education|Science",
           "Environment|Energy|Climate|Transport",
           "Justice|Interior", "Immigration|Integration",
           "Welfare|Social Affairs|Social Security|Family|Children",
           "Health")

for(ii in themes) { # rev(sort(unique(d$legislature)))
  
  if(nchar(ii) > 1)
    data = subset(d, grepl(ii, theme) & n_au > 1)
  else
    data = subset(d, legislature == ii & n_au > 1)
  
  print(table(data$type, data$year))
  
  rownames(medlem) = gsub("/Folketinget/findMedlem/|\\.aspx", "", medlem$url)
  
  edges = rbind.fill(lapply(data$links, function(i) {
    
    w = unlist(strsplit(i, ";"))
    d = medlem[ w, "name" ]

    d = subset(expand.grid(d, d), Var1 != Var2)
    d = unique(apply(d, 1, function(x) paste0(sort(x), collapse = "_")))

    if(length(d))
      return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    else
      return(data.frame())
    
  }))
    
  # raw edge counts
  count = table(edges$d)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ d, function(x) sum(1 / x), data = edges)
  
  # raw counts
  edges$count = as.vector(count[ edges$d ])
  
  edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$d),
                     j = gsub("(.*)_(.*)", "\\2", edges$d),
                     w = edges$w, n = edges[, 3])
  
  # network
  
  n = network(edges[, 1:2 ], directed = FALSE)
  
  n %n% "title" = paste("Folketing", paste0(range(substr(data$year, 1, 4)), collapse = " to "))
  n %n% "n_bills" = nrow(data)
  
  if(nchar(ii) > 1)
    n %n% "n_sponsors" = table(subset(d, grepl(ii, theme))$n_au)
  else
    n %n% "n_sponsors" = table(subset(d, legislature == ii)$n_au)
  
  rownames(medlem) = medlem$name
  n %v% "name" = medlem[ network.vertex.names(n), "name" ]
  n %v% "born" = medlem[ network.vertex.names(n), "born" ]
  n %v% "sex" = medlem[ network.vertex.names(n), "sex" ]
  n %v% "nyears" = medlem[ network.vertex.names(n), "nyears" ]
  n %v% "party" = medlem[ network.vertex.names(n), "party" ]
  n %v% "partyname" = medlem[ network.vertex.names(n), "partyname" ]
  n %v% "url" = medlem[ network.vertex.names(n), "url" ]
  n %v% "photo" = medlem[ network.vertex.names(n), "photo" ]
  
  network::set.edge.attribute(n, "source", as.character(edges[, 1]))
  network::set.edge.attribute(n, "target", as.character(edges[, 2]))
  
  network::set.edge.attribute(n, "weight", edges[, 3])
  network::set.edge.attribute(n, "count", edges[, 4])
  network::set.edge.attribute(n, "alpha",
                              as.numeric(cut(n %e% "count", c(1:4, Inf),
                                             include.lowest = TRUE)) / 5)
  
  # modularity
  
  nn = graph.edgelist(as.matrix(edges[, 1:2 ]), directed = FALSE)
  E(nn)$weight = edges[, 3]
  
  i = medlem[ V(nn)$name, "party" ]
  i[ i %in% c("IND", "S", "IA", "JF", "SF") ] = NA # unaffiliateds and regionalists
  
  nn = nn - which(is.na(i))
  i = as.numeric(factor(i[ !is.na(i) ]))
  
  n %n% "modularity" = modularity(nn, membership = i, weights = E(nn)$weight)

  walktrap = lapply(1:50, function(x) walktrap.community(nn, steps = x))
  
  # max. partition
  maxwalks = order(sapply(walktrap, modularity), decreasing = TRUE)[1]
  walktrap = walktrap[[ maxwalks ]]
  
  n %n% "modularity_walktrap" = modularity(walktrap)
  
  louvain = multilevel.community(nn)
  
  n %n% "modularity_louvain" = modularity(louvain)
  
  n %n% "modularity_maximized" = n %n% "modularity" /
    max(c(n %n% "modularity_walktrap", n %n% "modularity_louvain"))
  
  cat("Maximized modularity:", n %n% "modularity_maximized", "\n")
  
  # weighted adjacency matrix to tnet
  tnet = as.tnet(as.sociomatrix(n, attrname = "weight"), type = "weighted one-mode tnet")
  
  # weighted degree and distance
  wdeg = as.data.frame(degree_w(tnet, measure = "degree"))
  
  dist = distance_w(tnet)
  wdeg$distance = NA
  wdeg[ attr(dist, "nodes"), ]$distance = colMeans(dist, na.rm = TRUE)
  
  wdeg = cbind(wdeg, clustering_local_w(tnet)[, 2])
  names(wdeg) = c("node", "degree", "distance", "clustering")
  
  n %v% "degree" = wdeg$degree
  n %n% "degree" = mean(wdeg$degree, na.rm = TRUE)
  
  n %v% "distance" = wdeg$distance
  n %n% "distance" = mean(wdeg$distance, na.rm = TRUE)
  
  n %v% "clustering" = wdeg$clustering    # local
  n %n% "clustering" = clustering_w(tnet) # global
  
  i = colors[ medlem[ n %e% "source", "party" ] ]
  j = colors[ medlem[ n %e% "target", "party" ] ]
  
  party = as.vector(i)
  party[ i != j ] = "#AAAAAA"
  
  print(table(n %v% "party", exclude = NULL))
  
  # number of bills cosponsored
  nb = sapply(n %v% "url", function(x) {
    sum(unlist(strsplit(data$links, ";")) == x) # ids are varying-length letters
  })
  n %v% "n_bills" = as.vector(nb)
  
  if(plot) {
    
    n %v% "size" = as.numeric(cut(n %v% "degree", quantile(n %v% "degree"), include.lowest = TRUE))
    g = suppressWarnings(ggnet(n, size = 0, segment.alpha = 1/2, # mode = "kamadakawai",
                               segment.color = party) +
                           geom_point(alpha = 1/3, aes(size = n %v% "size", color = n %v% "party")) +
                           geom_point(alpha = 1/2, aes(size = min(n %v% "size"), color = n %v% "party")) +
                           scale_size_continuous(range = c(6, 12)) +
                           scale_color_manual("", values = colors, breaks = order) +
                           theme(legend.key.size = unit(1, "cm"),
                                 legend.text = element_text(size = 16)) +
                           guides(size = FALSE, color = guide_legend(override.aes = list(alpha = 1/3, size = 6))))
    
    ggsave(ifelse(nchar(ii) > 1,
                  paste0("plots/net_", gsub("(\\w)\\|(.*)", "\\1", ii), ".pdf"),
                  paste0("plots/net_dk", paste0(range(substr(data$year, 1, 4)), collapse = "-"), ".pdf")),
           g + theme(legend.key = element_blank()), width = 10, height = 9)
    
    ggsave(ifelse(nchar(ii) > 1,
                  paste0("plots/net_", gsub("(\\w)\\|(.*)", "\\1", ii), ".jpg"),
                  paste0("plots/net_dk", paste0(range(substr(data$year, 1, 4)), collapse = "-"), ".jpg")),
           g + theme(legend.position = "none"), width = 9, height = 9)
    
  }
  
  assign(ifelse(nchar(ii) > 1,
                paste0("net_dk", gsub("(\\w)\\|(.*)", "\\1", ii)),
                paste0("net_dk", substr(min(data$year), 1, 4))), n)

  if(nchar(ii) == 1) {
    assign(paste0("edges_dk", substr(min(data$year), 1, 4)), edges)
    assign(paste0("bills_dk", substr(min(data$year), 1, 4)), data)
  }
  
  # gexf (themes only)
  if(gexf & nchar(ii) > 1) {
    
    rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
    mode = "fruchtermanreingold"
    meta = list(creator = "rgexf",
                description = paste(mode, "placement", nrow(data), "bills"),
                keywords = "parliament, denmark")
    
    node.att = data.frame(party = n %v% "partyname",
                          bills = n %v% "n_bills",
                          distance = round(n %v% "distance", 1),
                          url = n %v% "url",
                          photo = n %v% "photo",
                          stringsAsFactors = FALSE)
    
    people = data.frame(id = as.numeric(factor(network.vertex.names(n))),
                        label = network.vertex.names(n),
                        stringsAsFactors = FALSE)
    
    relations = data.frame(
      source = as.numeric(factor(n %e% "source", levels = levels(factor(people$label)))),
      target = as.numeric(factor(n %e% "target", levels = levels(factor(people$label)))),
      weight = round(n %e% "weight", 2), count = n %e% "count")
    relations = na.omit(relations)
    
    # check all weights are positive after rounding
    stopifnot(all(relations$weight > 0))
    
    nodecolors = lapply(n %v% "party", function(x)
      data.frame(r = rgb[x, 1], g = rgb[x, 2], b = rgb[x, 3], a = .5))
    nodecolors = as.matrix(rbind.fill(nodecolors))
    
    # node placement
    position = do.call(paste0("gplot.layout.", mode),
                       list(as.matrix.network.adjacency(n), NULL))
    position = as.matrix(cbind(round(position, 1), 1))
    colnames(position) = c("x", "y", "z")
    
    # save with compressed floats
    write.gexf(nodes = people, nodesAtt = node.att,
               edges = relations[, 1:2 ], edgesWeight = relations[, 3],
               nodesVizAtt = list(position = position, color = nodecolors,
                                  size = round(n %v% "degree", 1)),
               # edgesVizAtt = list(size = relations[, 4]),
               defaultedgetype = "undirected", meta = meta,
               output = paste0("net_", gsub("(\\w)\\|(.*)", "\\1", ii), ".gexf"))
    
  }
  
}

save(list = ls(pattern = "^(net|edges|bills)_dk\\d{4}$"), file = "data/net_dk.rda")

if(gexf)
  zip("net_dk.zip", dir(pattern = "^net_\\w+\\.gexf$"))

# kthxbye
