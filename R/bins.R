bin.search <- function(map, what, key.len, row.len, sep.len=1L, rowsep.len=1L, na=NA_character_)
  .Call(bin_srch, map, what, key.len, row.len, sep.len, rowsep.len, na)
