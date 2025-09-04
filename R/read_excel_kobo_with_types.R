#' Lire et nettoyer un fichier brut Kobo avec typage auto
#'
#' @param file_path Chemin du fichier données brutes (.xlsx).
#' @param path_survey Chemin du fichier survey de kobo (.xlsx).
#' @param sheet Numéro ou nom de la feuille du fichier brut (par défaut 1).
#' @param sheet_survey Numéro ou nom de la feuille du survey (par défaut 2).
#'
#' @return Un tibble nettoyé avec les bons types de colonnes.
#' @export
#'
#' @examples
#' \dontrun{
#' raw <- read_and_clean(
#'   file_path   = "data/04_preclean/msna25_preclean.xlsx",
#'   path_survey = "data/01_docs/kobo/mli_msna_2025.xlsx"
#' )
#' }
read_and_clean <- function(file_path,
                           path_survey,
                           sheet = 1,
                           sheet_survey = 2) {
  
  # Vérifications
  if (!file.exists(file_path)) stop("Le fichier brut n'existe pas : ", file_path)
  if (!file.exists(path_survey)) stop("Le fichier survey n'existe pas : ", path_survey)
  
  # Charger le survey
  survey <- readxl::read_excel(path_survey, sheet = sheet_survey)
  if (!all(c("type", "name") %in% names(survey))) {
    stop("Le survey doit contenir au moins les colonnes 'type' et 'name'.")
  }
  
  # Mapping Kobo -> col_types readxl
  type_map <- c(
    "integer"         = "numeric",
    "decimal"         = "numeric",
    "text"            = "text",
    "select_one"      = "text",
    "select_multiple" = "text"
  )
  
  # Associer types Kobo aux colonnes
  var_types <- dplyr::tibble(
    name = survey$name,
    col_type = dplyr::recode(survey$type, !!!type_map, .default = "guess")
  )
  
  # Lire une fois pour avoir les colonnes
  raw_tmp <- readxl::read_excel(file_path, sheet = sheet, n_max = 0)
  col_types_vec <- rep("guess", ncol(raw_tmp))
  
  # Assigner les bons col_types
  for (i in seq_len(ncol(raw_tmp))) {
    col_name <- names(raw_tmp)[i]
    if (col_name %in% var_types$name) {
      col_types_vec[i] <- var_types$col_type[var_types$name == col_name]
    }
  }
  
  # Lire avec typage correct
  data <- readxl::read_excel(file_path, sheet = sheet, col_types = col_types_vec)
  
  # Nettoyer les noms de colonnes
  names(data) <- gsub("^_", "", names(data))
  names(data) <- gsub("/", ".", names(data))
  
  return(data)
}

