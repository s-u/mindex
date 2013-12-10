ascii.index <- function(map, index, sep="|", i, j) .Call(ascii_index, map, index, sep, i, j)

mindex <- function(file, sep="|", header=TRUE, index.file = paste0(file, ".index"), create=!file.exists(index.file)) {
  map <- map.file(file)
  if (create) .Call(mkindex, file, index.file)
  index <- slurp(index.file, 'l')
  first.line <- ascii.index(map, index, sep, 1L, NULL)
  n <- length(first.line)
  m <- length(index)
  if (m > 1L) m <- m - 1L ## the index should also have the EOL entry so m is one less
  if (isTRUE(header)) header <- first.line else if (is.logical(header)) header <- NULL
    
  structure(list(map=map, index=index, sep=sep, names=header, dim=c(m, n)), class="mindexf")
}

`[.mindexf` <- function(x, i, j) ascii.index(x$map, x$index, x$sep, if (missing(i)) NULL else i, if (missing(j)) NULL else j)

names.mindexf <- function(x) x$names
dim.mindexf <- function(x) x$dim
