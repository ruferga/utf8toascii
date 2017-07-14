#Remueve y convierte los caracteres especiales en español (Funciona principalmente con archivos generado por INEGI)

#Prepara los datos: convierte a minusculas, se asegura de que sea string y lo codifica a latin ascii 
require(foreign)
require(stringi)
require(stringr)
require(stringdist)

x$nombre <- tolower(x$nombre)

x$nombre <- as.character(x$nombre)

Encoding(x$nombre) <- "latin1"

x$nombre <- stri_trans_general(x$nombre, "latin-ascii")

#Funcion que gtransforma los caracteres especiales

utf8toascii <- function(x){
  x <- gsub(' +',' ', x, fixed = TRUE)
  x <- gsub("^\\s+|\\s+$", "", x, fixed = TRUE)
  x <- gsub("^\\s +|\\s +$", "", x, fixed = TRUE)
  x <- gsub("(.*?)($|'|[[:punct:]]+?)(.*?)", "\\1", x, fixed = TRUE)
  x <- gsub("a\u0083a\u0080\u009c", "o", x, fixed = TRUE)
  x <- gsub("\u0083\u0080", "e", x, fixed = TRUE)
  x <- gsub("a\u0083i¿ 1/2", "i", x, fixed = TRUE)
  x <- gsub("a\u0083a\u0080°", "e", x, fixed = TRUE)
  x <- gsub("a\u0083a¡", "u", x, fixed = TRUE)
  x <- gsub("a\u0083a\u0080\u0098", "n", x, fixed = TRUE)
  x <- gsub("A\u0083A³", "o", x, fixed = TRUE)
  x <- gsub("A\u0083A-", "i", x, fixed = TRUE)
  x <- gsub("\u0083A±", "n", x, fixed = TRUE)
  x <- gsub("A\u0083A¡", "a", x, fixed = TRUE)
  x <- gsub("A\u0083A(C)", "e", x, fixed = TRUE)
  x <- gsub("A\u0083Aº", "u", x, fixed = TRUE)
  x <- gsub("A\u0083a\u0080\u009c", "O", x, fixed = TRUE)
  x <- gsub("a\u0083a\u0080\u0099", "o", x, fixed = TRUE)
  x <- gsub("a\u0083a\u0093", "u", x, fixed = TRUE)
  x <- gsub("a\u0082a°", "°", x, fixed = TRUE)
  x <- gsub("a\u0082a´", "\'", x, fixed = TRUE)
  x <- gsub("a\u0082a¨", "\"", x, fixed = TRUE)
  x <- gsub("a\u0082a¿", "¿", x, fixed = TRUE)
  x <- gsub("a\u0083a-", "i", x, fixed = TRUE)
  x <- gsub("a\u0083a\u0080¹", "e", x, fixed = TRUE)
  x <- gsub("a\u0083a³", "o", x, fixed = TRUE)
  x <- gsub("a\u0083a\u0084¢", "c", x, fixed = TRUE)
  x
}

x$nombre1 -> utf8toascii(x$nombre)