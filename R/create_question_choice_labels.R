#' Construire les labels de questions et de choix depuis un XLSForm
#'
#' Cette fonction extrait les questions de type select_one / select_multiple
#' depuis la feuille survey, associe les labels des choix depuis la feuille
#' choices, et retourne une table plate question–choix prête pour le reporting.
#'
#' @param survey Dataframe correspondant à la feuille "survey" du XLSForm
#' @param choices Dataframe correspondant à la feuille "choices" du XLSForm
#'
#' @return Un dataframe avec les colonnes :
#' \itemize{
#'   \item question_name
#'   \item question_label
#'   \item list_name
#'   \item choice_name
#'   \item choice_label
#' }
#'
#' @export
#'
#' @examples
#' labels <- create_question_choice_labels(survey, choices)

create_question_choice_labels <- function(survey, choices) {

  # Nettoyage des choices
  choices_clean <- choices %>%
    dplyr::filter(!is.na(list_name)) %>%
    dplyr::rename(
      choice_name  = name,
      choice_label = label
    ) %>%
    dplyr::select(list_name, choice_name, choice_label)

  # Extraction des questions select_one / select_multiple
  question_labels <- survey %>%
    dplyr::mutate(
      type = ifelse(type == "integer", "select_one l_stat", type)
    ) %>%
    dplyr::filter(stringr::str_detect(type, "select_one|select_multiple")) %>%
    dplyr::mutate(
      list_name = stringr::str_trim(
        stringr::str_remove(type, "^select_(one|multiple)")
      )
    ) %>%
    dplyr::rename(
      question_name  = name,
      question_label = label
    ) %>%
    dplyr::select(question_name, question_label, list_name)

  # Jointure questions ↔ choix
  out <- question_labels %>%
    dplyr::left_join(choices_clean, by = "list_name")
  
# ======================
  # 🔔 Messages de contrôle
  # ======================

  missing_questions <- out %>%
    dplyr::filter(is.na(question_label) | question_label == "") %>%
    dplyr::distinct(question_name) %>%
    dplyr::pull(question_name)

  missing_choices <- out %>%
    dplyr::filter(is.na(choice_label) | choice_label == "") %>%
    dplyr::distinct(choice_name) %>%
    dplyr::pull(choice_name)

  if (length(missing_questions) > 0 || length(missing_choices) > 0) {

    message("⚠️ Labels manquants détectés :")

    if (length(missing_questions) > 0) {
      message(
        "  - Questions sans label : ",
        paste(missing_questions, collapse = ", ")
      )
    }

    if (length(missing_choices) > 0) {
      message(
        "  - Choices sans label : ",
        paste(missing_choices, collapse = ", ")
      )
    }

  } else {
    message("✅ Tous les questions et choices ont un label.")
  }

  return(out)
}



