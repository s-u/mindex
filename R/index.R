ascii.index <- function(map, index, sep="|", i, j) .Call(ascii_index, map, index, sep, i, j)

mindex <- function(file, sep="|", header=TRUE, index.file = paste0(file, ".index"), create=!file.exists(index.file)) {
  map <- map.file(file)
  if (create) .Call(mkindex, file, index.file)
  index <- slurp(index.file, 'l')
  if (isTRUE(header))
    header <- as.vector(ascii.index(map, index, sep, 1L, NULL))
  else if (is.logical(header))
    header <- NULL    
  structure(list(map=map, index=index, sep=sep, names=header), class="mindexf")
}

`[.mindexf` <- function(x, i, j) ascii.index(x$map, x$index, x$sep, if (missing(i)) NULL else i, if (missing(j)) NULL else j)
