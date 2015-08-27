# all bills (but no information on how many sponsors):
# https://raw.githubusercontent.com/teampopong/data-for-rnd/master/bills/names.csv

# ==============================================================================
# DOWNLOAD BILL INDEXES
# ==============================================================================

if (!file.exists("data/bills.csv")) {
  
  for (l in 19:1) {

    bills = paste0("data/bills-", l, ".csv")
    if (!file.exists(bills)) {

      h = html(paste0("http://en.pokr.kr/bill/?assembly_id=", l))
      p = html_nodes(h, ".btn-next")[[2]] %>% html_attr("href")
      p = gsub("(.*)page=(\\d+)(.*)", "\\2", p) %>% as.integer

      b = data_frame()

      cat("Legislature", l, ": downloading", p, "bill pages...\n")
      pb = txtProgressBar(0, p, style = 3)

      for (i in 1:p) {

        h = html(paste0("http://en.pokr.kr/bill/?page=", i, "&assembly_id=", l))
        t = html_node(h, "#bill-list table")

        # keywords
        k = html_nodes(t, xpath = "//tr/td[2]") %>% sapply(html_node, "p.keywords")
        k[ !sapply(k, is.null) ] = sapply(k[ !sapply(k, is.null) ], function(x) {
          paste0(html_nodes(x, "a") %>% html_text, collapse = ";")
        })
        k[ sapply(k, is.null) ] = NA

        b = rbind(b, data_frame(
          legislature = l,
          date = html_nodes(t, xpath = "//tr/td[1]") %>% html_text,
          url = html_nodes(t, xpath = "//tr/td[2]/a[1]") %>% html_attr("href"),
          title = html_nodes(t, xpath = "//tr/td[2]/a[1]") %>% html_text,
          origin = html_nodes(t, xpath = "//tr/td[3]") %>% html_text,
          status = html_nodes(t, xpath = "//tr/td[4]") %>% html_text,
          keywords = unlist(k)
        ))
        setTxtProgressBar(pb, i)

      }
      cat("\n")

      write.csv(b, bills, row.names = FALSE)

    }

  }

  # ============================================================================
  # DOWNLOAD (COSPONSORED) BILLS
  # ============================================================================

  b = list.files("data", pattern = "bills-", full.names = TRUE) %>%
    lapply(read.csv, stringsAsFactors = FALSE) %>%
    bind_rows

  for (l in unique(b$legislature) %>% sort %>% rev) {

    j = b$url[ b$legislature == l & grepl("\\d인$", b$origin) ]
    # j = sample(j, ifelse(length(j) >= 10000, length(j) / 100, length(j))) ## -------- TEMP!!!!!

    cat("Legislature", l, ": downloading", length(j), "bills...\n")
    pb = txtProgressBar(0, length(j), style = 3)

    for (i in j) {

      f = paste0("raw/bills/bill-", gsub("\\D", "", i), ".html")
      if (!file.exists(f))
        try(download.file(paste0("http://en.pokr.kr", i), f, mode = "wb", quiet = TRUE), silent = TRUE)

      if (!file.info(f)$size)
        file.remove(f)

      setTxtProgressBar(pb, which(j == i))

    }
    cat("\n")

  }

  b$sponsors = NA
  b$cosponsors = NA
  b$url = gsub("\\D", "", b$url)

  j = list.files("raw/bills", full.names = TRUE)

  cat("Parsing", length(j), "bills...\n")
  pb = txtProgressBar(0, length(j), style = 3)

  for (f in j) {

    # TEMP
    if (!file.info(f)$size) {
      file.remove(f)
      next
    }

    h = html(f) %>%
      html_nodes("#section-sponsors td")

    # primary sponsor(s)
    p = html_nodes(h[[1]], "a.person-link") %>%
      html_attr("href")

    if (!length(p))
      next # cat(f, ": no sponsors\n")
    else if (!length(p) & length(h) > 1)
      cat(f, ": no primary sponsor, but cosponsor(s)\n")
    else
      b$sponsors[ b$url == gsub("\\D", "", f) ] = paste0(gsub("\\D", "", p), collapse = ";")

    # cosponsor(s)
    if (length(h) > 1) {

      p = html_nodes(h[[2]], "a.person-link") %>%
        html_attr("href")

      b$cosponsors[ b$url == gsub("\\D", "", f) ] = paste0(gsub("\\D", "", p), collapse = ";")

    }

    setTxtProgressBar(pb, which(j == f))

  }

  cat("\n")

  write.csv(b, "data/bills.csv", row.names = FALSE)
  
}

b = read.csv("data/bills.csv")

b$n_a = 1 + str_count(b$sponsors, ";")
b$n_a[ is.na(b$sponsors) ] = 0

b$n_c = 1 + str_count(b$cosponsors, ";")
b$n_c[ is.na(b$cosponsors) ] = 0

b$n_au = b$n_a + b$n_c

table(b$n_a)
table(cut(b$n_c, c(0, 1, 2, max(b$n_c) + 1), right = FALSE))
table(cut(b$n_au, c(0, 1, 2, max(b$n_au) + 1), right = FALSE))

b$authors = b$sponsors
b$authors[ b$n_c > 0 ] = paste0(b$authors[ b$n_c > 0 ], ";",b$cosponsors[ b$n_c > 0 ])
stopifnot(!grepl("NA", b$authors))

# ==============================================================================
# PARSE SPONSORS
# ==============================================================================

j = strsplit(b$sponsors, ";") %>% unlist
j = c(j, strsplit(b$cosponsors, ";") %>% unlist) %>% unique %>% na.omit

cat("Downloading", length(j), "sponsors...\n")
pb = txtProgressBar(0, length(j), style = 3)

s = data_frame()

for (i in j) {

  f = paste0("raw/mps/", gsub("\\D", "", i), ".html")
  if (!file.exists(f))
    try(download.file(paste0("http://en.pokr.kr/person/", i), f, mode = "wb", quiet = TRUE), silent = TRUE)

  # skip very few buggy pages
  if (file.info(f)$size < 6000)
    next

  h = html(f)

  p = html_node(h, css = ".unstyled")
  p = data_frame(
    uid = i,
    name = html_node(h, "h2") %>% html_text,
    legislature = html_nodes(p, "span") %>% html_text,
    party = html_nodes(p, "a")[-1] %>% html_text
  )
  s = rbind(s, p)

  setTxtProgressBar(pb, which(j == i))

}

cat("\n")

# solve party affiliatons that last longer than one legislature
s$legislature = gsub("(\\d+)(st|nd|rd|th)", "\\1", s$legislature)

a = data_frame()
z = c()

for (i in 1:nrow(s)) {

  y = s$legislature[ i ]

  if (grepl("\\D", y)) {

    a = rbind(a, data_frame(
      uid = s$uid[ i ],
      name = s$name[ i ],
      legislature = str_extract_all(y, "\\d+") %>% unlist,
      party = s$party[ i ]
    ))
    z = c(z, i)

  }
}

s = s[ -z, ]
s = rbind(s, a) %>% arrange(name, legislature)

# very small number of missing (impossible to parse) sponsors
table(j[ !j %in% s$uid ])

s$name = str_clean(s$name)
s$party = gsub("\\sParty", "", s$party)

# solve duplicate names
s = arrange(s, legislature, uid, name) %>%
  group_by(legislature, name) %>%
  mutate(n = n_distinct(uid), o = 1:n()) %>%
  group_by %>%
  mutate(name = ifelse(n == 1, name, paste0(name, "-", o)))
