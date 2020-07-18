lapply(c('rlang', 'rvest', 'tibble', 'httr', 'stringr', 'tokenizers'), function(x) library(x, character.only = T))
enlaces <- read.csv("enlaces.csv")
enlaces <- unique(enlaces)
imagenes <- enlaces$Imagenes
enlaces <- enlaces$Enlaces
x <- data.frame()
for(enlace in 1:10000){ ######5000) {
  tryCatch({  
    pageSource <- read_html(enlaces[enlace])
    print(paste0("Se obtuvo el código fuente de la página del curso"))
    
    #tablaFinal$`Título curso`[link] <- tryCatch(html_text(html_nodes(html_nodes(read_html(as.character(unlist(pageSource))), "#mod_product_main"), "h1")), err = function(e) { NA })
    tituloCurso <- tryCatch(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".tarjeton"), "h1")), err = function(e) { NA })
    if(!is_empty(tituloCurso)) {
        precio <- tryCatch(str_flatten(unlist(str_extract_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".tarjeton"), ".app_price")), "[0-9]+")), ""), err = function(e) { NA })
        preciosincupon <- tryCatch(as.character(str_remove_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".tarjeton"), ".price-offer-old")), "€")), err = function(e) { NA })
        categoria <- tryCatch(str_trim(str_squish(unlist(str_split(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".breadcrumb__inner"), "a")), pattern = "\\n\\t")))), err = function(e) { NA })
        categoria <- categoria[categoria != ""]
        categoria <- str_flatten(categoria[2:4], "/")
        jTxt <- tryCatch(
          str_trim(str_squish(
            str_flatten(unlist(tokenize_paragraphs(html_text(html_nodes(read_html(as.character(pageSource)), ".order-4")[1]))), " ")
          )), 
          err = function(e) { NA }
        )
        todotexto <- ifelse(is.na(jTxt), NA, str_flatten(unlist(jTxt), " "))
        nombrecentro <- tryCatch(str_trim(str_squish(str_remove_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".tarjeton"), ".school-link")), "\\n"))), err = function(e) { NA })
        plazorealizacion <- tryCatch(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".tarjeton"), "ficha-description")), err = function(e) { NA })
        imagen <- tryCatch(imagenes[enlace], err = function(e) { NA })
        dirigido <- tryCatch(str_trim(str_squish(str_remove_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".order-4"), ".ficha-publico-objetivo")), "\\n"))), err = function(e) { NA })
        temario <- tryCatch(str_trim(str_squish(str_replace_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".order-4"), ".ficha-temario")), "\\n", replacement = " "))), err = function(e) { NA })
        condiciones <- tryCatch(str_trim(str_squish(str_replace_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".order-4"), ".ficha-requisitos")), "\\n", replacement = " "))), err = function(e) { NA })
        descripcion <- tryCatch(str_trim(str_squish(str_replace_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".order-0"), ".ficha-description")), "\\n", replacement = " "))), err = function(e) { NA })
        paginacentro <- " "
        duracionhoras <- " "
      } else {
        tituloCurso <- tryCatch(str_trim(str_squish(str_remove_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".product-name"), "h1")), "\\n"))), err = function(e) { NA })
        precio <- tryCatch(str_flatten(unlist(str_extract_all(html_text(html_nodes(html_nodes(html_nodes(html_nodes(read_html(as.character(pageSource)), ".product-shop"), ".price-box"), ".special-price"), ".price")), "[0-9]+")), ""), err = function(e) { NA })
        precio <- ifelse(is_empty(precio), " ", precio)
        preciosincupon <- tryCatch(as.character(str_remove_all(html_text(html_nodes(html_nodes(html_nodes(html_nodes(read_html(as.character(pageSource)), ".product-shop"), ".price-box"), ".regular-price"), ".price")), "€")), err = function(e) { NA })
        preciosincupon <- ifelse(is_empty(preciosincupon), " ", preciosincupon)
        categoria <- " " ##categoria[categoria != ""]
        nombrecentro <- tryCatch(str_trim(str_squish(str_remove_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".datos"), "strong")), "\\n"))), err = function(e) { NA })
        nombrecentro <- ifelse(is_empty(nombrecentro), " ", nombrecentro)
        tiempo <- tryCatch(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".caracteristicas"), ".tiempo")), err = function(e) { NA })
        if(is_empty(tiempo)) {
          duracionhoras <- " "
          plazorealizacion <- " "
        } else {
          duracionhoras <- unlist(str_split(tiempo, "-"))[1]
          plazorealizacion <- unlist(str_split(tiempo, "-"))[2]
        }
        imagen <- tryCatch(html_attr(html_nodes(read_html(as.character(pageSource)), ".gallery-image"), "src"), err = function(e) { NA })
        imagen <- ifelse(is_empty(imagen), " ", imagen)
        dirigido <- " " #####tryCatch(str_trim(str_squish(str_remove_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".order-4"), ".ficha-publico-objetivo")), "\\n"))), err = function(e) { NA })
        tabs <- tryCatch(str_trim(str_squish(str_replace_all(html_text(html_nodes(html_nodes(read_html(as.character(pageSource)), ".product-collateral"), ".accordion_content")), "\\n", replacement = " "))), err = function(e) { NA })
        temario <- str_remove_all(tabs[2], '\"')
        descripcion <- str_remove_all(tabs[1], '\"')
        condiciones <- str_remove_all(tabs[3], '\"')
        paginacentro <- tryCatch(html_attr(html_nodes(read_html(as.character(pageSource)), ".url_centro"), "href"), err = function(e) { NA })
      }
    
    theROW <- tibble(
      "Título curso" = tituloCurso,
      "Precio" = ifelse(is_empty(precio), " ", precio),
      "Precio Sin cupon" = ifelse(is_empty(preciosincupon), " ", str_replace_all(str_replace_all(preciosincupon, pattern = "[:punct:]", replacement = ""), pattern = '\\"', "-")),
      "Categoría" = ifelse(is_empty(categoria), " ", categoria),
      "Descripción curso corta" = " ",
      "Descripción completa" = str_replace_all(ifelse(is_empty(descripcion), " ", descripcion), pattern = '\\"', "-"),
      "Condiciones" = ifelse(is_empty(condiciones), " ", condiciones),
      "Descripción autor" = " ",
      "Nombre autor" = " ",
      "Temario" = str_replace_all(ifelse(is_empty(temario), " ", temario), pattern = '\\"', "-"),
      "URL Imagen" = ifelse(is_empty(imagen), " ", imagen),
      "Descripcion Centro" = " ", 
      "Caracteristicas curso" = " ",
      "A quien dirigido" = str_replace_all(ifelse(is_empty(dirigido), " ", dirigido), pattern = '\\"', "-"),
      "Página web centro" = ifelse(is_empty(paginacentro), " ", paginacentro),
      "Nombre Centro Fromativo" = str_replace_all(nombrecentro, pattern = '\\"', "-"),
      "Créditos ECTS" = " ",
      "Duración En horas" = ifelse(is_empty(duracionhoras), " ", duracionhoras),
      "Plazo Realizacion" = str_replace_all(ifelse(is_empty(plazorealizacion), " ", plazorealizacion), pattern = '\\"', "-"),
      "URL Curso" = enlaces[enlace],
      "Todo Texto" = str_replace_all(todotexto, pattern = '\\"', "-"))
    if(nrow(theROW) == 0) {
      print("La estructura del sitio es distinta")
      write.table(x = data.frame("enlace" = enlace), 
                  col.names = F, file = "EnlacesDistintos.csv", append = T, sep = ",", row.names = F, quote = T)
    } else {
      write.table(x = theROW, 
                  col.names = F, file = "FinalLectiva.csv", append = T, sep = ",", row.names = F, quote = T)
      
      print(paste0("Guardado curso: ", enlace))
    }
    
    enlace <- enlace + 1
  }, error = function(e) {
    print("Ha ocurrido algun error")
    write.table(x = data.frame("enlace" = enlace, "url" = enlaces[enlace]), 
                col.names = F, file = "EnlacesDistintos.csv", append = T, sep = ",", row.names = F, quote = T)
    print(paste0("Pasando al siguiente curso"))
    
  })
  
}
#t <- read.csv("Scrapers/Lectiva/FinalLectiva.csv", header = T, sep = ",")
