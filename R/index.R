ascii.index <- function(map, index, sep="|", i, j) .Call(ascii_index, map, index, sep, i, j)

mindex <- function(file, sep="|", index.file = paste0(file, ".index"), create=!file.exists(index.file)) {
  map <- map.file(file)
  if (create) .Call(mkindex, file, index.file)
  index <- slurp(index.file, 'l')
  structure(list(map=map, index=index, sep=sep), class="mindexf")
}

`[.mindexf` <- function(x, i, j) ascii.index(x$map, x$index, x$sep, if (missing(i)) NULL else i, if (missing(j)) NULL else j)
