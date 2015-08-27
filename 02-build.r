for (ii in unique(b$legislature) %>% sort) {

  cat("Legislature", ii)

  data = filter(b, legislature == ii, n_au > 1)

  if (!nrow(data)) {
    cat(": no bills\n")
    next
  }

  sp = filter(s, legislature == ii) %>% data.frame
  rownames(sp) = sp$name

  # shortern party name
  sp$party[ nchar(sp$party) > 15 ] = paste0(substr(sp$party[ nchar(sp$party) > 15 ], 1, 15), ".")

  cat(":", nrow(data), "cosponsored documents, ")

  # ============================================================================
  # DIRECTED EDGE LIST
  # ============================================================================

  edges = lapply(unique(data$authors), function(d) {

    w = strsplit(d, ";") %>% unlist

    return(expand.grid(i = sp$name[ sp$uid %in% w ],
                       j = sp$name[ sp$uid == w[1]],
                       w = length(w) - 1, stringsAsFactors = FALSE))

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

  n %n% "n_cosponsored" = nrow(data)
  n %n% "n_sponsors" = table(filter(b, legislature == ii)$n_au)

  # ============================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ============================================================================

  n_au = as.vector(n_au[ network.vertex.names(n) ])

  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)

  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"

  cat(network.size(n), "nodes\n")

  rownames(sp) = sp$name

  n %v% "url" = sp[ network.vertex.names(n), "uid" ]
  n %v% "party" = sp[ network.vertex.names(n), "party" ]

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author

  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

  # ============================================================================
  # SAVE PLOTS
  # ============================================================================


  if (plot) {

    p = table(n %v% "party")
    colors = rep("#AAAAAA", length(p))
    names(colors) = unique(n %v% "party")
    p = names(p)[ p >= 5 & names(p) != "independent" ]

    colors[ names(colors) %in% p ] = brewer.pal(8, "Set2")[ 1:length(p) ]
    colors[ names(colors) == "independent" ] = "#444444"
    colors = c(colors[ names(colors) != "independent" ] %>% sort,
               colors[ names(colors) == "independent" ])

    save_plot(n, paste0("plots/net_kr", ii),
              i = colors[ sp[ n %e% "source", "party" ] ],
              j = colors[ sp[ n %e% "target", "party" ] ],
              mode, colors)

  }

  # ============================================================================
  # SAVE OBJECTS
  # ============================================================================

  assign(paste0("net_kr", substr(ii, 1, 4)), n)
  assign(paste0("edges_kr", substr(ii, 1, 4)), edges)
  assign(paste0("bills_kr", substr(ii, 1, 4)), data)

}
