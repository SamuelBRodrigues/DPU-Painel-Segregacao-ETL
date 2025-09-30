#' Construção da Estrutura do Cálculo dos Índices
#' 
#' Essa função é responsável por construi 
#'
#' @param dataset_selecionado Dataset com colunas selecionadas da tabela `dados_preprocessados`.
#'
#' @returns Uma tabela contendo os níves geográficos, variáveis de referência e variáveis contendo os grupos em que os índices serão baseados.
#' @export
#'
#' @examples
constroi_calculo_indice_estrutura <- function(dataset_selecionado){
  var_normal <- colnames(dataset_selecionado) |> 
    stringr::str_subset("renda", negate = T)
  
  var_renda <- colnames(dataset_selecionado) |> 
    stringr::str_subset("renda")
  
  var_pessoas_brancas <- var_normal |> 
    stringr::str_subset(
      "branca"
    )
  var_pessoas_nao_brancas <- var_normal |> 
    stringr::str_subset(
      "branca|pessoas_total",
      negate = T
    ) |> 
    stringr::str_subset(
      "cor_ou_raca"
    )
  
  var_indices_normal <- var_pessoas_brancas |> stringr::str_extract("\\w*(?<=raca_e_)|\\w*(?<=domicilio_e_)")
  
  tabela_indices_normal <- purrr::map(
    var_indices_normal,
    ~ {
      regex <- stringr::str_glue("{.x}[:alpha:]+$")
      data <- tidyr::expand_grid(
        niveis_geograficos = c("MN", "RM", "UF", "RG", "Brasil"),
        referencias = stringr::str_subset(var_pessoas_brancas, regex),
        grupos = stringr::str_subset(var_pessoas_nao_brancas, regex)
      )
    }
  ) |>
    purrr::list_rbind()
  
  var_pessoas_brancas_renda <- var_renda |> 
    stringr::str_subset(
      "branca"
    )
  var_pessoas_nao_brancas_renda <- var_renda |> 
    stringr::str_subset(
      "branca|pessoas_total",
      negate = T
    ) |> 
    stringr::str_subset(
      "cor_ou_raca"
    )
  
  var_indices_renda <- var_pessoas_brancas_renda |> stringr::str_extract("\\w*(?<=raca_e_)|\\w*(?<=domicilio_e_)")
  
  tabela_indices_renda <- purrr::map(
    var_indices_renda,
    ~ {
      regex <- stringr::str_glue("{.x}[:alpha:]+_renda_estimada")
      data <- tidyr::expand_grid(
        niveis_geograficos = c("MN", "RM", "UF", "RG", "Brasil"),
        referencias = stringr::str_subset(var_pessoas_brancas_renda, regex),
        grupos = stringr::str_subset(var_pessoas_nao_brancas_renda, regex)
      )
    }
  ) |>
    purrr::list_rbind()
  
  tabela_indice <- dplyr::bind_rows(
    tabela_indices_normal,
    tabela_indices_renda
  )
}

