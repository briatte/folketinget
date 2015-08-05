# add committee co-memberships

get_committees <- function(x) {

  l = str_clean(gsub("Parlamentarisk karriere", "", x))
  l = str_extract_all(l, "i udvalget\\.|71( udvalg)?|\\w+\\s(U|u)dvalg(\\s|,|\\.)|\\w+valg(et|sposter)?(\\s(for|om|til|vedr\\.|vedrørende)\\s\\w+)?")
  l = tolower(unlist(l))
  l = str_trim(gsub("[[:punct:]]|(vedr|vedrørende)\\s", "", l))
  l = gsub("(ud)?valget", "udvalg", l)
  # supervision of inmates and psychiatric institutions
  l[ l %in% c("71", "71 udvalg") ] = "§ 71-tilsynet"
  # below: Åland Islands committees (all three for n = 1)
  l[ l %in% c("industriudvalg", "samfærdselsudvalg", "markedsudvalg") ] = "Lagtingets"
  l[ l == "arbejdsmarkedsudvalg" ] = "Arbejdsmarked" # labour market
  l[ l == "beskæftigelsesudvalg" ] = "Beskæftigelse" # occupation
  l[ l == "boligudvalg" ] = "Bolig"
  # first option is a special case (n = 1)
  l[ l %in% c("udvalg tilsyn", "udvalg efterretningstjenesterne") ] = "Efterretningstjenesterne" # MI5
  l[ l %in% c("udvalg etisk", "udvalg vedr etisk", "udvalg det") ] = "Det Etiske Råd" # ethics
  l[ l %in% c("kontaktudvalg", "kontaktudvalg for det") ] = "Det Tyske Mindretal" # German minority
  l[ l == "erhvervsudvalg" ] = "Erhverv" # professions
  l[ l == "eksportudvalg" ] = "Eksport" # business/trade/export
  l[ l %in% c("energipolitisk udvalg", "energipolitiske udvalg", "energiudvalg") ] = "Energipolitisk"
  # last option below is special case (n = 1)
  l[ l %in% c("europaudvalg", "underudvalg") ] = "Europa"
  l[ l == "finansudvalg" ] = "Finans"
  l[ l == "forsvarsudvalg" ] = "Forsvar" # defence
  l[ l %in% c("forskningsudvalg", "udvalg for forskning") ] = "Forskning" # research
  l[ l %in% c("fødevareudvalg", "udvalg for fødevarer") ] = "Fødevarer" # food
  # last option below is for mp-VPILA, special case
  l[ l %in% c("færøudvalg", "udvalg færøerne", "udvalg færøske", "i udvalg") ] = "Færøerne"
  l[ l %in% c("udvalg grønlandske", "grønlandsudvalg", "udvalg vedr grønland") ] = "Grønland"
  l[ l == "indfødsretsudvalg" ] = "Indfødsret" # citizenship
  l[ l == "kirkeudvalg" ] = "Kirke"
  l[ l == "bygningsudvalg" ] = "Klima-Energi-Bygnings"
  l[ l == "kommunaludvalg" ] = "Kommunal"
  l[ l == "kulturudvalg" ] = "Kultur"
  l[ l %in% c("udvalg for landdidstrikter", "udvalg for landdistrikter") ] = "Landdistrikter"
  # last option below is a recoding (n = 1)
  l[ l %in% c("ligestilingsudvalg", "ligestillingsudvalg", "kvindeudvalg") ] = "Ligestiling" # equal opp
  l[ l == "miljøudvalg" ] = "Miljø" # envir
  # below: # (physical) planning; last one is a recoding
  l[ l %in% c("planlægningsudvalg", "udvalg om fysisk") ] = "Planlægning"
  # below: last one is a recoding (currency; n = 1)
  l[ l %in% c("økonomisk udvalg", "økonomiske udvalg", "økonomiudvalg", "valutaudvalg") ] = "Politisk-Økonomisk"
  # last options below are both recodings (n = 1)
  l[ l %in% c("retsudvalg", "juridisk udvalg", "forfatningsudvalg") ] = "Ret" # law
  l[ l %in% c("afgiftsudvalg", "skatteudvalg") ] = "Skatte" # taxes
  l[ l == "socialudvalg" ] = "Social"
  # last one below is recoded (n = 1)
  l[ l %in% c("forbyggelsesudvalg", "forebyggelsesudvalg", "sundhedsudvalg", "alkoholpolitisk udvalg") ] = "Sundhed-Forebyggelse"
  l[ l %in% c("udvalg danske", "udvalg vedr danske", "sydslesvigudvalg", "mandsudvalg vedr sydslesvig") ] = "Sydslesvig" # German region
  l[ l == "trafikudvalg" ] = "Trafik"
  l[ l == "transportudvalg" ] = "Transport"
  l[ l == "udenrigsudvalg" ] = "Udenrigs" # foreign affairs
  l[ l == "uddannelsesudvalg" ] = "Uddannelse" # education
  # below: teaching (second is a recoding, n = 1)
  l[ l %in% c("undervisningsudvalg", "undervisningsministeriets udvalg") ] = "Undervisnings"
  # below: foreigners (last is a recoding: integration, n = 1)
  l[ l %in% c("udvalg for udlændinge", "integrationsudvalg") ] = "Udlændinge"
  l[ l %in% c("udvalg for forretningsordenen", "forretningsudvalg") ] = "Forretningsordenen" # legal/exec
  l[ l %in% c("udvalg til prøvelse", "udvalg til valgs") ] = "Valgs Prøvelse" # elections
  l[ l %in% c("udvalg for videnskab", "teknologiudvalg") ] = "Videnskab-Teknologi" # sci/tech
  l[ l == "økontaktudvalg" ] = "Ø-Kontakt" # liaison committee (unsure; n = 2)
  l[ l == "udvalg for småøer" ] = "Småøer" # Islands (n = 1)
  
  stopifnot(length(l) == str_count(r, "71|\\w+valg") | l == "§ 71-tilsynet")
  stopifnot(!l %in% c("Udvalg", "Udvalget", "udvalg", "udvalget"))
  l = l[ l != "udvalgsposter" ] # mp-SLEJE, special case: no details
  return(l)
  
}

load("data/net_dk.rda")
comm = data.frame()

# find unique committees

stopifnot(s$file %in% list.files("raw/mp-pages", full.names = TRUE))
for (i in list.files("raw/mp-pages", full.names = TRUE)) {
  
  h = htmlParse(i, encoding = "UTF-8")
  r = xpathSApply(h, "//strong[contains(text(), 'Parlamentarisk karriere')]/..", xmlValue)
  l = get_committees(r)

  if (length(l)) # ~ 20% MPs have no career details
    comm = rbind(comm, data_frame(i, l))
  
}

comm = unique(comm) %>%
  arrange(l)

comm = data_frame(committee = unique(comm$l))

# add sponsor columns
for (i in list.files("raw/mp-pages", full.names = TRUE))
  comm[, gsub("raw/mp-pages/mp-|\\.html", "", i) ] = 0

for (i in list.files("raw/mp-pages", full.names = TRUE)) {
  
  h = htmlParse(i, encoding = "UTF-8")
  r = xpathSApply(h, "//strong[contains(text(), 'Parlamentarisk karriere')]/..", xmlValue)
  l = get_committees(r)

  comm[ comm$committee %in% l, names(comm) == gsub("raw/mp-pages/mp-|\\.html", "", i) ] = 1
  
}

# no flat list to save, and committees are coded as identical for all legislatures

for (i in ls(pattern = "^net_dk")) {
  
  cat("Network:", i)
  
  n = get(i)
  sp = network.vertex.names(n)
  names(sp) = gsub("http://www.ft.dk/folketinget/findmedlem/|\\.aspx", "", n %v% "url")
  
  m = comm[, names(comm) %in% names(sp) ]
  cat(":", nrow(m), "committees", ncol(m), "MPs")
  M = m
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  e = data_frame(i = n %e% "source", j = n %e% "target")
  e$committee = NA
  
  for (j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]
  
  cat(" co-memberships:", 
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"), 
      sum(e$committee == 0), "null,", 
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")
  
  nn = network(e[, 1:2], directed = FALSE)
  nn %e% "committee" = e$committee
  
  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(network.size(n) == network.size(nn))
  
  n %e% "committee" = e$committee
  assign(i, n)
  
  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("co", i), nn)
  
}

save(list = ls(pattern = "^((co)?net|edges|bills)_dk\\d{4}$"),
     file = "data/net_dk.rda")
