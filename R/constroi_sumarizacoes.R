#' Constrói Sumarização das Variáveis
#'
#' @param dataset_selecionado Dataset com colunas da tabela `dados_preprocessados` selecionadas.
#' @param nivel_geografico Nível geográfico para o cálcul do índice. Valores podem ser: `MN` - para município; `RM` para região metropolitana; `UF` para Unidade Federativa; `RG` para Região e `Brasil` para o país.
#'
#' @returns Uma tabela contendo os dados sumarizados.
#' @export
#'
#' @examples
constroi_sumarizacoes <- function(dataset_selecionado, nivel_geografico){
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
      MN = c("NV_GEO",chaves_geo),
      RM = c("NV_GEO", setdiff(chaves_geo, c("cd_mun", "nm_mun_1"))),
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
  
  dados_summarizados <- tabela_selecionada |> 
    dplyr::select(-cd_setor) |> 
    dplyr::summarise(
      .by = dplyr::all_of(geo),
      dplyr::across(
        -dplyr::contains("_renda_estimada"),
        ~ sum(.x)
      ),
      dplyr::across(
        dplyr::contains("_renda_estimada"),
        ~ mean(.x)
      )
    ) |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("cd"),
        ~ as.character(.x)
      ),
      dplyr::across(
        dplyr::starts_with("nm"),
        ~ as.character(.x)
      )
    ) |> 
    dplyr::rename_with(
      .fn = ~ stringr::str_to_upper(.x),
      .cols = dplyr::starts_with("cd")
    ) |> 
    dplyr::rename_with(
      .fn = ~ stringr::str_to_upper(.x),
      .cols = dplyr::starts_with("nm")
    )
}