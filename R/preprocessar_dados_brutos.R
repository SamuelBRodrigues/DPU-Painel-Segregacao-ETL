#' Preprocessamento dos Dados Brutos
#' 
#' Essa função é responsável por aplicar um tratamento de limpeza e dados
#'
#' @param dados_agregados_setor_censitario Tabela contendo os dados Agregados do Setor Censitário.
#' @param dicionario_agregados_setor_censitario Tabela contendo o Dicionário dos dados de Agregados do Setor Censitário.
#' @param dados_regioes_metropolitanas Tabela contendo os dados das Regiões Metropolitanas do Brasil.
#'
#' @returns Uma tabela contendo os dados de Agregados do Setor Censitário tratados.
#' Esses dados estão pronto para serem inseridos na função `calcula_indice_duncan`.
#' @export
#'
#' @examples
preprocessar_dados_brutos <- function(
    dados_agregados_setor_censitario = dpupsetl::dat_agregados_setor_censitario,
    dicionario_agregados_setor_censitario = dpupsetl::dic_agregados_setor_censitario,
    dados_regioes_metropolitanas = dpupsetl::dat_regioes_metropolitanas
){
  logger::log_info("Aplicando pré-processamento")
  # Aplicando um tratamento inicial aos dados
  dat_agregados_setor_censitario_tratado <- dat_agregados_setor_censitario |> 
    # Removendo os valores faltantes da População Total
    dplyr::filter(!is.na(pop_total)) |> 
    # Renomeando as variáveis para os códigos do dicionário
    dplyr::rename(
      "V01317" = "pop_branca",
      "V01322" = "pop_homem_branco",
      "V01327" = "pop_mulher_branca",
      "V01318" = "preta",
      "V01319" = "amarelo",
      "V01320" = "pardo",
      "V01321" = "indigena"
    ) |> 
    # Selecionando as variáveis de interesse
    dplyr::select(
      CD_REGIAO, NM_REGIAO, CD_UF, NM_UF, CD_MUN, NM_MUN_1, CD_setor,
      "pessoas_total" = pop_total, "V01007"= masculino, "V01008" = feminino,
      dplyr::starts_with("V01"), dplyr::starts_with("pessoas_")
    ) |> 
    # Tratamento do tipo de variáveis
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("CD_"),
        ~ as.character(.)
      ),
      dplyr::across(
        dplyr::starts_with("V01"),
        ~ as.integer(.)
      ),
      pessoas_total = as.integer(pessoas_total)
    ) |> 
    # Aplicando um tratamento inicial aos dados faltantes
    dplyr::mutate(
      V01319 = dplyr::case_when(
        is.na(V01319) ~ pessoas_total - V01318 - V01321 - V01317 - V01320,
        .default = V01319
      ),
      V01318 = dplyr::case_when(
        is.na(V01318) ~ pessoas_total - V01319 - V01321 - V01317 - V01320,
        .default = V01318
      ),
      V01321 = dplyr::case_when(
        is.na(V01321) ~ pessoas_total - V01318 - V01319 - V01317 - V01320,
        .default = V01321
      ),
      V01317 = dplyr::case_when(
        is.na(V01317) ~ pessoas_total - V01318 - V01321 - V01319 - V01320,
        .default = V01317
      ),
      V01320 = dplyr::case_when(
        is.na(V01320) ~ pessoas_total - V01318 - V01321 - V01317 - V01320,
        .default = V01320
      ),
      V01007 = dplyr::case_when(
        is.na(V01007) ~ pessoas_total - V01008,
        .default = V01007
      ),
      V01008 = dplyr::case_when(
        is.na(V01008) ~ pessoas_total - V01007,
        .default = V01008
      ),
      V01322 = dplyr::case_when(
        is.na(V01322) ~ V01317 - V01327,
        .default = V01322
      ),
      V01327 = dplyr::case_when(
        is.na(V01327) ~ V01317 - V01322,
        .default = V01327
      ),
      V01325 = dplyr::case_when(
        is.na(V01325) ~ V01320 - V01330,
        .default = V01325
      ),
      V01330 = dplyr::case_when(
        is.na(V01330) ~ V01320 - V01325,
        .default = V01330
      ),
      V01323 = dplyr::case_when(
        is.na(V01323) ~ V01318 - V01328,
        .default = V01323
      ),
      V01328 = dplyr::case_when(
        is.na(V01328) ~ V01318 - V01323,
        .default = V01328
      ),
      V01324 = dplyr::case_when(
        is.na(V01324) ~ V01319 - V01329,
        .default = V01324
      ),
      V01329 = dplyr::case_when(
        is.na(V01329) ~ V01319 - V01324,
        .default = V01329
      ),
      V01326 = dplyr::case_when(
        is.na(V01326) ~ V01321 - V01331,
        .default = V01326
      ),
      V01331 = dplyr::case_when(
        is.na(V01331) ~ V01321 - V01326,
        .default = V01331
      )
    )
  
  # Construindo a equivalência das variáveis
  dic_agregados_setor_censitario_adaptado <- dic_agregados_setor_censitario |> 
    janitor::clean_names() |> 
    dplyr::mutate(
      descricao = stringr::str_glue("pessoas_{descricao}")
    )
  
  nome_variaveis_correspondencia <- purrr::set_names(dic_agregados_setor_censitario_adaptado$descricao, dic_agregados_setor_censitario_adaptado$variavel)
  
  logger::log_info("Inserindo os dados das Regiões Metropolitanas")
  # Inserindo as Regiões metropolitanas e renomeando as variáveis
  dataset_cleaned <- dat_agregados_setor_censitario_tratado |> 
    dplyr::left_join(
      dat_regioes_metropolitanas,
      by = "CD_MUN"
    ) |> 
    dplyr::relocate(
      NM_RM, .after = NM_UF
    ) |>
    dplyr::rename_with(
      .fn = ~ nome_variaveis_correspondencia[.x],
      .cols = dplyr::starts_with("V01")
    ) |> 
    janitor::clean_names()
  
  dataset_tratado <- imputa_kernel(dataset_cleaned = dataset_cleaned) |> 
    sf::st_drop_geometry()

  
}

