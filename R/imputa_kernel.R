#' Imputação do Modelo de Kernel
#' 
#' Essa função é responsável para por imputar um modelo de KNN e Kernel aos dados
#' faltantes.
#'
#' @param dataset_cleaned 
#'
#' @returns Uma tabela com as colunas tratadas adicionadas. As novas colunas contém
#' o sufixo indetificador `knn` para aplicação do modelo de KNN para o tratamento de 
#' dados faltantes e `kernel` para aplicação do modelo de Kernel.
#' @export
#'
#' @examples
imputa_kernel <- function(dataset_cleaned){
  # Coleta dataset
  segregacao <- dataset_cleaned
  
  # pega setores geobr
  logger::log_info("Construindo os dados geográficos dos Setores Censitários")
  setores_sf <- geobr::read_census_tract("all", year = 2022, showProgress = TRUE) |>
    dplyr::select(code_tract, geom) |>
    dplyr::rename(cd_setor = code_tract) |>
    dplyr::mutate(cd_setor = as.character(cd_setor))
  
  dados_sf <- segregacao |>
    dplyr::left_join(setores_sf) |>
    sf::st_as_sf() |>
    sf::st_make_valid()
  
  
  logger::log_info("Coordenadas dos centróides (para vizinhança kNN)")
  coords <- sf::st_coordinates(sf::st_centroid(dados_sf))
  # APLICACAO DE KNN --------------------------------------------------------
  logger::log_info("Aplicando KNN")
  k_neighbors <- 8
  
  knn_idx <- FNN::get.knn(coords, k = k_neighbors)$nn.index

  logger::log_info("Calculando as médias locais")
  # vizinhança para médias locais (spdep)
  knn_nb <- spdep::knn2nb(spdep::knearneigh(coords, k = k_neighbors))
  lw_W   <- spdep::nb2listw(knn_nb, style = "W")
  
  
  # imputacao dos dados -----------------------------------------------------
  
  ## KNN
  impute_knn_vec <- function(x, nn_index) {
    x_out <- x
    nas   <- which(is.na(x))
    if (length(nas) == 0) return(x_out)
    
    for (i in nas) {
      viz <- nn_index[i, ]
      val <- mean(x[viz], na.rm = TRUE)
      if (is.nan(val)) {
        # fallback: média global da variável (se todos vizinhos também são NA)
        val <- mean(x, na.rm = TRUE)
      }
      x_out[i] <- val
    }
    x_out
  }
  
  ## Kernel
  
  
  impute_kernel_vec <- function(x, listw_W, group_fallback = NULL) {
    
    x2 <- ifelse(is.finite(x), x, NA_real_)
    
    
    loc <- spdep::lag.listw(listw_W, x2, NAOK = TRUE, zero.policy = TRUE)
    
    if (is.null(group_fallback)) {
      fb <- mean(x2, na.rm = TRUE)
      fb_vec <- rep(fb, length(x2))
    } else {
      
      fb_vec <- ave(x2, group_fallback, FUN = function(v) mean(v, na.rm = TRUE))
    }
    
    
    out <- x2
    miss <- is.na(out)
    out[miss] <- loc[miss]                           
    still_miss <- is.na(out)
    out[still_miss] <- fb_vec[still_miss]            
    
    out
  }
  
  
  # Aplicacao de funcoes-------------------------------------------------------------------------
  
  vars_target <- dados_sf |> 
    tibble::tibble() |> 
    dplyr::select(
      dplyr::starts_with("pessoas_")
    ) |> 
    colnames()
  
  # KNN
  logger::log_info("Imputando dados KNN")
  dados_knn <- dados_sf |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars_target),
        ~ {
          if (!anyNA(.x)) return(.x)
          imp <- impute_knn_vec(.x, knn_idx)
          dplyr::coalesce(.x, imp)
        },
        .names = "{.col}_knn_imp"
      )
    )
  
  # Kernel
  logger::log_info("Imputando dados Kernel")
  dados_kernel <- dados_knn |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars_target),
        ~ {
          if (!anyNA(.x)) return(.x)
          imp <- impute_kernel_vec(.x, lw_W)     # opcional: passe group_fallback = CD_MUN
          dplyr::coalesce(.x, imp)
        },
        .names = "{.col}_kernel_imp"
      )
    )
  
  return(dados_kernel)
}

