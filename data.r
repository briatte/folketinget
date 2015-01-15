root = "http://www.ft.dk"
legislature = c("2004-05" = 1, "2005-06" = 2, "2006-07" = 2,
                "2007-08" = 3, "2008-09" = 3, "2009-10" = 3, "2010-11" = 3,
                "2011-12" = 4, "2012-13" = 4, "2013-14" = 4, "2014-15" = 4)

sponsors = "data/sponsors.csv"

for(type in c("Forslag_til_vedtagelse", "Beslutningsforslag", "Lovforslag")) {
  
  data = data.frame()
  name = ifelse(type == "Beslutningsforslag", "motions", 
                ifelse(type == "Forslag_til_vedtagelse", "resolutions",
                       "bills"))
  
  for(i in rev(sort(c(paste0(2014:2004, 1), paste0(c(2004, 2007, 2010), 2))))) {
    
    file = paste0("raw/", name, i, ".html")
    
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
      
      file = gsub("raw/", "data/", gsub("html$", "csv", file))
      if(!file.exists(file)) {
        
        links = data.frame()
        
      } else {
        
        links = read.csv(file, stringsAsFactors = FALSE)
        urls = urls[ !urls %in% links$url ]
        
      }
      
      cat("Year", substr(i, 1, 4), "session", substr(i, 5, 5), ":",
          sprintf("%3.0f", length(urls)), name, "left to scrape ")
      
      if(length(urls)) {
        
        for(j in urls) {
          
          h = try(htmlParse(paste0(root, j)))
          if("try-error" %in% class(h)) {
            
            cat("x")
            
          } else {
            
            t = str_clean(xpathSApply(h, "//h1[1]", xmlValue))
            
            l = paste0(str_clean(xpathSApply(h, "//a[contains(@href, 'findMedlem')]/@href")), collapse = ";")
            
            h = str_clean(xpathSApply(h, "//p", xmlValue))
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

d = rbind_all(lapply(dir("data", pattern = "^(bills|motions|resolutions)\\d+.csv", full.names = TRUE),
                      read.csv, stringsAsFactors = FALSE))

d$legislature = legislature[ d$year ]

d$type = ifelse(grepl("-B", d$uid), "motion",
                ifelse(grepl("-V", d$uid), "resolution", "bill"))

d$year[ nchar(d$year) > 7 ] = NA

print(table(d$type, d$legislature, exclude = NULL))

# sponsors

u = unlist(strsplit(d$links, ";"))
u = paste0("/Folketinget/findMedlem/", unique(na.omit(u)), ".aspx")

if(!file.exists(sponsors)) {
  
  s = data.frame()
  l = sapply(1:4, function(j) {
    h = htmlParse(paste0("http://www.ft.dk/Folketinget/searchResults.aspx?letter=ALLE&pageSize=50&pageNr=",
                         j, "#search"))
    xpathSApply(h, "//a[contains(@href, '/findMedlem/')]/@href")
  })
  u = unique(c(unlist(l), u))
  cat("Finding", length(u), "new sponsors\n")
  
} else {
  
  s = read.csv(sponsors, stringsAsFactors = FALSE)
  u = u[ !u %in% s$url ]
  cat("Finding", length(u), "missing sponsors\n")
  
}

u = gsub("\\s", "%20", u)

for(k in rev(u)) {
  
  cat(sprintf("%3.0f", which(u == k)))
  file = gsub("aspx$", "html", gsub("/Folketinget/findMedlem/", "raw/mp-", k))
  if(!file.exists(file))
    try(download.file(paste0(root, k), file, mode = "wb", quiet = TRUE), silent = TRUE)
  
  if(!file.info(file)$size) {
    
    cat(": failed", paste0(root, k), "\n")
    file.remove(file)
    
  } else {
    
    h = htmlParse(file)
    cat(":", file, ":",
        gsub("Folketinget - ", "",
             str_clean(xpathSApply(h, "//title", xmlValue))), "\n")
    
    constit = xpathSApply(h, "//strong[text()='Medlemsperiode']/..", xmlValue)
    constit = gsub("(.*)\\si\\s(.*)(\\s(Amts|Stor)kreds)(.*)", "\\2\\3", constit)
    
    img = xpathSApply(h, "//img[contains(@src, '/media/')]/@src")
    s = rbind(s,
              data.frame(file,
                         name = xpathSApply(h, "//meta[@name='Fullname']/@content"),
                         func = xpathSApply(h, "//meta[@name='Function']/@content"),
                         party = xpathSApply(h, "//meta[@name='Party']/@content"),
                         mandate = xpathSApply(h, "//meta[@name='MfPeriod']/@content"),
                         constituency = ifelse(length(constit), constit, NA),
                         job = xpathSApply(h, "//div[contains(@class, 'person')]/p[2]/strong", xmlValue),
                         photo = ifelse(length(img), img, NA),
                         url = k,
                         bio = xpathSApply(h, "//div[contains(@class, 'tabContent')]/p[1]", xmlValue),
                         stringsAsFactors = FALSE))
    
  }
  
}

# special constituencies
s$constituency[ grepl("Grønland", s$constituency) ] = "Grønland"
s$constituency[ grepl("Færøerne", s$constituency) ] = "Færøerne"

# remove two strictly ministerial sponsors
s = subset(s, func != "exmin")

write.csv(s, sponsors, row.names = FALSE)

s$mandate = sapply(s$mandate, function(x) {
  x = str_extract_all(x, "[0-9]{4}")
  paste0(unique(unlist(x)), collapse = ";")
})

s$mandate[ s$mandate == "" ] = "2011;2012;2013;2014"

# old method (approximate)
# s$from = as.numeric(sapply(strsplit(gsub("(\\d{4})(1|2)", "\\1", s$mandate), ","), min))
# s$to = as.numeric(sapply(strsplit(gsub("(\\d{4})(1|2)", "\\1", s$mandate), ","), max))
# s$nyears = s$to - s$from + 1

s$sex = str_extract(s$bio, "(D|d)atter af|(S|s)øn af")
s$sex = ifelse(grepl("(D|d)atter af", s$sex), "F", "M")
s$sex[ !grepl("(D|d)atter af|(S|s)øn af", s$bio) ] = NA

# fill in a few missing values
s$sex[ is.na(s$sex) & 
              grepl("^(Anne|Annika|Dorrit|Erika|Fatma|Ida|Karin|Linda|Lise|Lykke|Marlene|Mette|Mie|Sanne|Özlem Sara|Pia|Sofia|Stine)", s$name) ] = "F"
s$sex[ is.na(s$sex) & 
              grepl("^(Erling|Eyvind|Hans|Jacob|Jens|Jeppe|Johs\\.|Jørgen|Kamal|Kuupik|Niels|Nikolaj|Per|Peter|Thomas|Uffe)", s$name) ] = "M"

s$born = str_extract(s$bio, "født [0-9\\.]+ [a-z\\.]+ \\d{4}")
s$born = sapply(str_extract_all(s$born, "[0-9]{4}"), length)
s$born[ s$born != 1 ] = 0
s$born[ s$born == 1 ] = str_extract(s$bio[ s$born == 1 ], "[0-9]{4}")
s$born[ s$born == 0 ] = NA

s$party[ is.na(s$party) | s$party %in% c("", "Indep") ] = "Independent"
s$party[ s$party == "Ny Alliance" ] = "Liberal Alliance"

# download photos
for(i in which(!is.na(s$photo))) {
  photo = gsub("/Folketinget/findMedlem/(\\w+)\\.aspx", "photos/\\1.jpg", s$url[ i ])
  # special cases
  if(grepl("Thor Moger Pedersen", s$url[ i ]))   photo = "photos/scSFTMP.jpg"
  if(grepl("Charlotte Sahl-Madsen", s$url[ i ])) photo = "photos/scKFCSM.jpg"
  if(!file.exists(photo) | !file.info(photo)$size) {
    try(download.file(paste0(root, "/Folketinget/findMedlem/", gsub("\\s", "%20", s$photo[ i ])),
                      photo, mode = "wb", quiet = TRUE), silent = TRUE)
  }
  if(!file.exists(photo) | !file.info(photo)$size) {
    file.remove(photo) # will warn if missing
    s$photo[ i ] = NA
  } else {
    s$photo[ i ] = gsub("photos/|.jpg$", "", photo)
  }
}

s$url = gsub("/Folketinget/findMedlem/|\\.aspx", "", s$url)
s$url = gsub("\\s", "%20", s$url)
rownames(s) = s$url

s$partyname = s$party
s$party = c("Enhedslisten" = "E",
            "Socialistisk Folkeparti" = "SFP",
            "Socialdemokratiet" = "SD",
            "Radikale Venstre" = "RV",
            "Kristendemokraterne" = "KD",
            "Liberal Alliance" = "LA",
            "Venstre" = "V",
            "Det Konservative Folkeparti" = "KFP",
            "Dansk Folkeparti" = "DFP",
            "Inuit Ataqatigiit" = "IA",
            "Siumut" = "S",
            "Sambandsflokkurin" = "SF",
            "Javnaðarflokkurin" = "JF",
            "Independent" = "IND")[ s$partyname ]

# categorize bills

cat("Found", nrow(read.csv("data/bills.csv")), "bills",
    nrow(read.csv("data/motions.csv")), "motions",
    nrow(read.csv("data/resolutions.csv")), "resolutions",
    nrow(s), "MPs", nrow(d), "texts ")

d$n_au = 1 + str_count(d$links, ";")

d = subset(d, !grepl("minister", d$authors, ignore.case = TRUE))

# Danish MPs can submit both bills or 'motions', i.e. bills that are
# not fully drafted and that are quicker to submit; both are used in
# the graphs. Resolutions do not have keywords (no ministerial area)
# and are excluded from both types of graphs.

print(table(d$type, d$legislature))

d = subset(d, type != "resolution")

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
