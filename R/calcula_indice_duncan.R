#' Cálculo dos Índices de Duncan
#'
#' @param dataset_selecionado Dataset com colunas da tabela `dados_preprocessados` selecionadas.
#' @param nivel_geografico Nível geográfico para o cálcul do índice. Valores podem ser: `MN` - para município; `RM` para região metropolitana; `UF` para Unidade Federativa; `RG` para Região e `Brasil` para o país.
#' @param referencia Variável que será usada como referência para o cálulo do Índice. Equivale ao `X` da fórmula.
#' @param grupo Variável que contém o grupo a ser usado como cálculo do Índice. Equivale ao `Y` da fórmula.
#'
#' @returns Uma tabela contendo os resultados do índice para o dado nível geográfico
#' @export
#'
#' @examples
calcula_indice_duncan <- function(dataset_selecionado, nivel_geografico, referencia, grupo) {
  
  # Construindo nome das variáveis
  ref_pop   <- paste0("soma_", referencia)
  grp_pop   <- paste0("soma_", grupo)
  ref_prop  <- paste0("prop_", referencia)
  grp_prop  <- paste0("prop_", grupo)
  indice_nome <- paste0("D_indice_", referencia)
  
  # Pegando a Cor ou Raça de referência
  cor_ou_raca_referencia <- grupo |> 
    stringr::str_extract(
      "(?<=_e_)[:alpha:]+"
    ) |> 
    stringr::str_to_title()
  if(cor_ou_raca_referencia == "Naobranco"){
    cor_ou_raca_referencia = "Não Branco"
  }
  
  # Construindo nome do Indice
  indice_racas <- stringr::str_glue("Branca x {cor_ou_raca_referencia}")
  
  indice_nome <- referencia |> 
    stringr::str_remove("pessoas_cor_ou_raca_e_|pessoas_cor_ou_raca_da_|pessoas_sexo") |> 
    stringr::str_remove("cor_ou_raca_e_") |> 
    stringr::str_remove("_sexo_da_pessoa_responsavel_pelo_domicilio") |> 
    stringr::str_remove("e_") |> 
    stringr::str_remove("^_") |> 
    stringr::str_replace_all("_", " ") |> 
    stringr::str_to_title() |> 
    stringr::str_replace("Branca", indice_racas)
  
  
  logger::log_info("Calculando Indice de Duncan {indice_nome} para {nivel_geografico}")
  chaves_geo <- c("cd_regiao", "nm_regiao", "cd_uf", "nm_uf", "nm_rm", "cd_mun", "nm_mun_1")
  
  if (nivel_geografico == "Brasil") {
    tabela_selecionada <- dataset_selecionado %>%
      dplyr::mutate(
        NV_GEO = "Brasil",
        !!!setNames(rep(list(NA), length(chaves_geo)), chaves_geo)
      ) %>%
      dplyr::distinct(NV_GEO, !!!rlang::syms(c(chaves_geo, "cd_setor")), .keep_all = TRUE)
    
    geo <- c("NV_GEO", "cd_regiao", "nm_regiao", "cd_uf", "nm_uf", "cd_mun", "nm_mun_1")
    
  } else {
    niveis <- list(
      MN = chaves_geo,
      RM = setdiff(chaves_geo, c("cd_mun", "nm_mun_1")),
      UF = c("NV_GEO","cd_regiao", "nm_regiao", "cd_uf", "nm_uf"),
      RG = c("NV_GEO","cd_regiao", "nm_regiao")
    )
    geo <- niveis[[nivel_geografico]]
    
    def_geo <- setdiff(chaves_geo, geo)
    
    tabela_estruturada <- dataset_selecionado %>%
      dplyr::mutate(NV_GEO = nivel_geografico) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(def_geo), ~NA))
    
    tabela_selecionada <- tabela_estruturada %>%
      dplyr::distinct(NV_GEO, !!!rlang::syms(c(chaves_geo, "cd_setor")), .keep_all = TRUE) %>%
      { if (nivel_geografico == "RM") dplyr::filter(., !is.na(nm_rm)) else .}
  }
  
  
  
  tabela_selecionada %>%
    # totalizações por grupo
    dplyr::mutate(
      .by = dplyr::all_of(geo),
      !!ref_pop := sum(.data[[referencia]], na.rm = TRUE),
      !!grp_pop := sum(.data[[grupo]], na.rm = TRUE)
    ) %>%
    # proporções
    dplyr::mutate(
      !!ref_prop := .data[[referencia]] / .data[[ref_pop]],
      !!grp_prop := .data[[grupo]] / .data[[grp_pop]]
    ) %>%
    # diferenças absolutas
    dplyr::mutate(
      media_diff_abs = abs(.data[[ref_prop]] - .data[[grp_prop]])
    ) %>%
    # índice final
    dplyr::mutate(
      .by = dplyr::all_of(geo),
      D_indice_value = sum(media_diff_abs, na.rm = TRUE)
    ) %>%
    dplyr::slice_sample(n = 1, by = dplyr::all_of(geo)) |> 
    dplyr::mutate(
      D_indice_tipo = indice_nome
    ) |> 
    dplyr::select(
      NV_GEO, cd_regiao, nm_regiao, cd_uf, nm_uf, nm_rm, cd_mun, nm_mun_1, D_indice_tipo, D_indice_value
    ) |> 
    dplyr::rename_with(
      .fn = ~ stringr::str_to_upper(.x),
      .cols = dplyr::starts_with("cd")
    ) |> 
    dplyr::rename_with(
      .fn = ~ stringr::str_to_upper(.x),
      .cols = dplyr::starts_with("nm")
    ) |> 
    dplyr::mutate(
      D_indice_value = D_indice_value/2,
      D_indice_value = round(D_indice_value, 4),
      D_indice_CAT = dplyr::case_when(
        D_indice_value == 0 ~ "0: total integração",
        D_indice_value > 0 & D_indice_value <= 0.2 ~ "0 - 0.2: segregação baixa",
        D_indice_value > 0.2 & D_indice_value <= 0.4 ~ "0.2 - 0.4: segregação moderada",
        D_indice_value > 0.4 & D_indice_value <= 0.6 ~ "0.4 - 0.6: alta segregação",
        D_indice_value > 0.6 & D_indice_value <= 0.8 ~ "0.6 - 0.8: segregação muito alta",
        D_indice_value > 0.8 & D_indice_value <= 1 ~ "0.8 - 1.0: segregação quase total"
      )
    )
  
}
