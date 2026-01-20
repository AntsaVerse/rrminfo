#' Reformater un tableau de frequences verticale en horizontale
#'
#' Cette fonction pivote les données, calcule les statistiques (moyennes, médianes,
#' et effectifs) pour les variables de type marge d'erreur et n, puis retourne un
#' tableau résumé par groupes. Elle se base sur les sorties d'analyse de IMPACT Initiatives
#'
#' @param df Dataframe d'entrée
#' @param admin Variable d'administration (symbole non quoté)
#' @param pop_group Variable du groupe de population (symbole non quoté)
#' @param question Variable contenant la question (symbole non quoté)
#' @param reponse Variable contenant la réponse (symbole non quoté)
#' @param value Variable contenant la valeur principale (symbole non quoté)
#' @param moe_var Variable contenant la marge d'erreur (symbole non quoté)
#' @param n_var Variable contenant le nombre d'observations (symbole non quoté)
#'
#' @return Un dataframe résumé avec les valeurs pivotées et les statistiques moe/n
#' @export
#'
#' @examples
#' # pivotwider_freq_table(df, admin = region, pop_group = group,
#' #                  question = q, reponse = r, value = val,
#' #                  moe_var = moe, n_var = n)

pivotwider_freq_table <- function(df, admin, pop_group, question, reponse, value, moe_var, n_var) {
  cat("\014")
  cat(cli::col_blue("Prends un bon thé avec des herbes, je m'occupe de tout.... \n"))
  Sys.sleep(3)

  # Étape 1 : Pivot des données en format large
  cat(cli::col_red("Étape 1/4 : Pivot des données en format large...\n"))

  moe_sym <- rlang::ensym(moe_var)
  n_sym   <- rlang::ensym(n_var)

  df_wide <- df %>%
    tidyr::pivot_wider(
      names_from = c({{question}}, {{reponse}}),
      values_from = c({{value}}),
      names_sep = "."
    )%>%
    dplyr::select(-!!moe_sym, -!!n_sym)

  # Étape 2 : Calcul des stats moe/n par question
  cat(cli::col_green("Étape 2/4 : Calcul des stats moe/n par question...\n"))

  unique_questions <- unique(df[[rlang::as_name(rlang::ensym(question))]])

  moe_stats_list <- purrr::map(unique_questions, function(q) {
    df_sub <- df %>% filter({{question}} == q)
    df_sub %>%
      group_by({{admin}}, {{pop_group}}, {{question}}) %>%
      summarise(
        moe_mean   = mean(!!moe_sym, na.rm = TRUE),
        moe_median = median(!!moe_sym, na.rm = TRUE),
        n    = max(!!n_sym, na.rm = TRUE),
        .groups    = "drop"
      )
  }, .progress = TRUE)

  moe_stats <- dplyr::bind_rows(moe_stats_list) %>%
    tidyr::pivot_wider(
      names_from  = {{question}},
      values_from = c(moe_mean, moe_median, n),
      names_glue  = "{.name}.{.value}",
      names_sep   = "."
    )

  # Étape 3 : Fusionner les tables
  cat(cli::col_yellow("Étape 3/4 : Fusion des tables...\n"))

  df_final <- df_wide %>%
    dplyr::left_join(
      moe_stats,
      by = c(
        rlang::as_name(rlang::ensym(admin)),
        rlang::as_name(rlang::ensym(pop_group))
      )
    )
 
  # Étape 4 : Calcul des moyennes finales par groupe
  cat(cli::col_magenta("Étape 4/4 : Calcul des moyennes finales par groupe...\n"))

  df_final <- df_final %>%
    dplyr::group_by({{admin}}, {{pop_group}}) %>%
    dplyr::summarise(
      dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ ifelse(is.nan(.x), 0, .x))
    )

  cat(cli::col_cyan("✅ Traitement terminé !...\n"))
  return(df_final)
}
