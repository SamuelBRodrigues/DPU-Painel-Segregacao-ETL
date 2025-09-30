#' Constroi Novas Colunas por Agregação
#' 
#' Essa função é responsável por criar novas colunas via agregação de outras colunas. Por exemplo "Não Brancas" é o resultado da da soma do resultado para Raça ou Cor Preta e Parda.
#'
#' @param dataset_selecionado Dataset com colunas da tabela `dados_preprocessados` selecionadas.
#' @param nova_variavel Nome da nova variável a ser criadas.
#' @param variaveis_agregadas Nome das colunas que serão somadas para criar a nova coluna
#'
#' @returns Uma tabela com as novas variaveis criadas segundo a agregação especificada.
#' @export
#'
#' @examples
constroi_agregacao_variaveis <- function(dataset_selecionado, nova_variavel, variaveis_agregadas){
  
  colunas <- dataset_selecionado |> 
        colnames() |> 
    stringr::str_subset("branca") 

  nova_variavel_nome <- colunas |> 
    stringr::str_replace("branca", nova_variavel)
  
  variaveis_a_serem_agregadas <- purrr::map(
    variaveis_agregadas,
    ~{
      vars_agg <- colunas |> 
        stringr::str_replace("branca", .x)
    }
  ) |> 
    purrr::pmap_chr(~paste(..., sep = ","))

  dataset_final <- purrr::reduce2(
    .x = nova_variavel_nome,      # A lista sobre a qual vamos iterar
    .y = variaveis_a_serem_agregadas,
    .init = dataset_selecionado,   # O valor inicial: nosso dataset original
    .f = function(dados_acumulados, nome_da_nova_coluna, variaveis_soma) {
      
      colunas_soma <- stringr::str_split_1(variaveis_soma, ",")
      
      if(stringr::str_detect(variaveis_soma, "renda_estimada")){
        dados_acumulados |>
        dplyr::mutate(
          !!nome_da_nova_coluna := rowMeans(dplyr::across(dplyr::all_of(colunas_soma)), na.rm = TRUE)
        )
      } else{
        dados_acumulados |>
        dplyr::mutate(
          !!nome_da_nova_coluna := rowSums(dplyr::across(dplyr::all_of(colunas_soma)), na.rm = TRUE)
        )
      }
      
    }
  )
  
}