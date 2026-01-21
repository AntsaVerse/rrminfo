#' Construire une ligne de labels pour une table de fréquences formatée
#'
#' Cette fonction génère une table de labels alignée sur les colonnes
#' d'un tableau de fréquences (output analytique), en combinant les
#' informations issues des labels de questions, des choix et des
#' statistiques (MoE, n).
#'
#' @param freq_h_table_output Dataframe de fréquences finales
#' @param question_labels Dataframe question–choice labels (voir create_question_choice_labels)
#' @param survey Dataframe de la feuille "survey" du XLSForm
#'
#' @return Un dataframe avec une ligne de labels et les mêmes colonnes
#'   que \code{freq_h_table_output}
#'
#' @export
add_frequency_table_labels <- function(
  freq_h_table_output,
  question_labels,
  survey
) {

  # ======================
  # 1. Noms de variables
  # ======================
  add_label <- data.frame(
    variable_name = names(freq_h_table_output),
    stringsAsFactors = FALSE
  )

  # ======================
  # 2. Extraction question / choice
  # ======================
  add_label <- add_label %>%
    dplyr::filter(!variable_name %in% c("admin", "pop_group")) %>%
    dplyr::mutate(
      question_name = sub("\\..*$", "", variable_name),
      question_name = ifelse(
        question_name %in% c("moe_mean", "moe_median", "n"),
        stringr::str_extract(variable_name, "(?<=\\.).*(?=\\.)"),
        question_name
      ),
      choice_name = sub("^[^.]*\\.", "", variable_name),
      choice_name = sub("^[^.]*\\.", "", choice_name)
    )

  # ======================
  # 3. Jointure labels
  # ======================
  add_label <- add_label %>%
    dplyr::left_join(
      dplyr::select(question_labels, -question_label),
      by = c("question_name", "choice_name")
    ) %>%
    dplyr::left_join(
      dplyr::select(survey, name, label),
      by = c("question_name" = "name")
    ) %>%
    dplyr::select(variable_name, label, choice_name, choice_label)

  # ======================
  # 4. Labels des stats
  # ======================
  add_label <- add_label %>%
    dplyr::mutate(
      choice_label = dplyr::case_when(
        choice_name == "moe_mean"   ~ "MoE [Mean]",
        choice_name == "moe_median" ~ "MoE [Median]",
        choice_name == "n"          ~ "n",
        TRUE                        ~ choice_label
      )
    ) %>%
    dplyr::select(variable_name, label, choice_label) %>%
    dplyr::distinct(variable_name, .keep_all = TRUE)

  # ======================
  # 5. Transposition
  # ======================
  add_label_t <- add_label %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)

  colnames(add_label_t) <- add_label_t[1, ]
  add_label_t <- add_label_t[-1, , drop = FALSE]
  rownames(add_label_t) <- seq_len(nrow(add_label_t))

  # ======================
  # 6. Admin / pop_group
  # ======================
  add_label_t <- add_label_t %>%
    dplyr::select(-dplyr::any_of(c("admin", "pop_group"))) %>%
    dplyr::mutate(
      admin     = NA,
      pop_group = NA
    )

  # ======================
  # 7. Ordre final
  # ======================
   out <- add_label_t %>%
    dplyr::select(dplyr::all_of(names(freq_h_table_output)))

 return(out)
}


