# Limpiecito.

#' @title Limpieza de caracteres o strings.
#' @description Es una función para limpiar espacios por demás al inicio, final o intermedio. Asimismo
#' transforma las letras minúsculas en mayúsculas para un mejor tratamiento.
#' @param x un elemento o vector character.
#' @details Esta función se usará para la limpieza de base de datos.
#' @examples
#' limpiecito(c("    raul", " yo uso    R"))
#' # "RAUL" "YO USO R"
#' @export

limpiecito<-function(x){
  x<-gsub("(^\\s+|\\s+$)","",x)
  x<-gsub("\\s+", " ",x)
  x<-toupper(x)
  x<-gsub("Á", "A", x)
  x<-gsub("É", "E", x)
  x<-gsub("Í", "I", x)
  x<-gsub("Ó", "O", x)
  x<-gsub("Ú", "U", x)
  x
}


# UBIGEO PERÚ.

#' @title Ubigeo Peru
#' @description Convertir el código numérico de ubigeo (código de ubicación geográfica del Perú)
#' a nivel departamental.
#' @param x un elemento o vector character.
#' @details El o los elementos de `x` pueden ser los 6 dígitos que brinda el INEI.
#' Incluso el vector `x` puede tener entre 1 y 2 caracteres, por tanto no es necesario expresar
#' el ubigeo con un `0` antes de los primeros 9 departamentos.
#' @examples
#' dpto_peru(c("1", "01", "010405", "12", "120902", "9", "090302"))
#' # "AMAZONAS" "AMAZONAS"  "AMAZONAS" "JUNIN" "JUNIN" "HUANCAVELICA" "HUANCAVELICA"
#' @export

dpto_peru<-function(x) {
    x<-ifelse(grepl("^1$|^01",x),"AMAZONAS",
       ifelse(grepl("^2$|^02",x),"ANCASH",
       ifelse(grepl("^3$|^03",x),"APURIMAC",
       ifelse(grepl("^4$|^04",x),"AREQUIPA",
       ifelse(grepl("^5$|^05",x),"AYACUCHO",
       ifelse(grepl("^6$|^06",x),"CAJAMARCA",
       ifelse(grepl("^7$|^07",x),"CALLAO",
       ifelse(grepl("^8$|^08",x),"CUSCO",
       ifelse(grepl("^9$|^09",x),"HUANCAVELICA",
       ifelse(grepl("^10$|^10",x),"HUANUCO",
       ifelse(grepl("^11$|^11",x),"ICA",
       ifelse(grepl("^12$|^12",x),"JUNIN",
       ifelse(grepl("^13$|^13",x),"LA LIBERTAD",
       ifelse(grepl("^14$|^14",x),"LAMBAYEQUE",
       ifelse(grepl("^15$|^15",x),"LIMA",
       ifelse(grepl("^16$|^16",x),"LORETO",
       ifelse(grepl("^17$|^17",x),"MADRE DE DIOS",
       ifelse(grepl("^18$|^18",x),"MOQUEGUA",
       ifelse(grepl("^19$|^19",x),"PASCO",
       ifelse(grepl("^20$|^20",x),"PIURA",
       ifelse(grepl("^21$|^21",x),"PUNO",
       ifelse(grepl("^22$|^22",x),"SAN MARTÍN",
       ifelse(grepl("^23$|^23",x),"TACNA",
       ifelse(grepl("^24$|^24",x),"TUMBES",
       ifelse(grepl("^25$|^25",x),"UCAYALI",NA)))))))))))))))))))))))))
       x
  }

