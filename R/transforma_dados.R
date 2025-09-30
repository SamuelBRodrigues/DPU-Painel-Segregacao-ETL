#' Transformação dos Dados
#'
#' Função responsável por aplicar o processo de transformação nos dados, construindo os dados de Índice de Duncan e resultados das variáveis por níveis geográficos.
#' 
#' @param modelo Colunas que serão utilizadas com base no uso de um modelo para o tratamento de daos faltantes. Valores podem ser `knn`, `kernel` ou `NULL` para coluna sem tratamento.
#' @param dados_agregados_setor_censitario Tabela contendo os dados Agregados do Setor Censitário. 
#' @param dicionario_agregados_setor_censitario Tabela contendo o Dicionário dos dados de Agregados do Setor Censitário. 
#' @param dados_regioes_metropolitanas Tabela contendo os dados das Regiões Metropolitanas do Brasil. 
#'
#' @returns Uma tabela contendo os dados transformados, com os índices já calculados.
#' @export
#'
#' @examples
transforma_dados <- function(
    modelo = "kernel",
    dados_agregados_setor_censitario = dpupsetl::dat_agregados_setor_censitario,
    dicionario_agregados_setor_censitario = dpupsetl::dic_agregados_setor_censitario,
    dados_regioes_metropolitanas = dpupsetl::dat_regioes_metropolitanas
){
  # Aplicando preprocessamento dos dados
  dados_preprocessados <- preprocessar_dados_brutos(
    dados_agregados_setor_censitario = dados_agregados_setor_censitario,
    dicionario_agregados_setor_censitario = dicionario_agregados_setor_censitario,
    dados_regioes_metropolitanas = dados_regioes_metropolitanas
  )
  
  # Selecionando apenas as colunas que irão ser usadas para construir os indíces
  if(is.null(modelo)){
    dataset_selecionado <- dados_preprocessados |> 
      dplyr::select(-dplyr::contains("kernel"), -dplyr::contains("knn")) %>%
      constroi_agregacao_variaveis(., "n_branco", c("preta", "parda"))
  } else if(modelo %in% c("knn", "kernel")){
    sufixo_modelo <- stringr::str_glue("_{modelo}_imp")
    dataset_selecionado <- dados_preprocessados |> 
      dplyr::select(1:8, dplyr::contains(modelo)) |> 
      dplyr::rename_with(
        .fn = ~ stringr::str_remove(.x, sufixo_modelo)
      ) %>%
      constroi_agregacao_variaveis(., "naobranco", c("preta", "parda"))
  } else{
    base::stop(
      stringr::str_glue(
        "'{modelo}' não é um input aceitável. Insira 'knn', 'kernel' ou NULL, para usar as variáveis sem nenhum tratamento."
      )
    )
  }
  
  calculo_indice_estrutura <- constroi_calculo_indice_estrutura(dataset_selecionado)
  
  indice_raca <- purrr::pmap(
    calculo_indice_estrutura,
    function(niveis_geograficos, grupos, referencias){
      calcula_indice_duncan(
        dataset_selecionado = dataset_selecionado,
        nivel_geografico = niveis_geograficos,
        referencia = referencias,
        grupo = grupos
      )
    }
  ) |> 
    purrr::list_rbind()

  dados_sumarizados <- purrr::map(
    c("MN", "RM", "UF", "RG", "Brasil"),
    ~ constroi_sumarizacoes(dataset_selecionado, .x)
  ) |> 
    purrr::list_rbind()
  
  dados_transformados <- dplyr::left_join(
    indice_raca,
    dados_sumarizados
  )
}

