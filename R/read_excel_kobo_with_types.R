#' Lire et nettoyer un fichier brut Kobo avec typage auto
#'
#' Cette fonction lit un fichier brut (souvent issu de KoboToolbox) en appliquant
#' automatiquement les bons types de colonnes (numeric, text) en fonction du
#' fichier `survey` de l’outil Kobo. Les noms de colonnes sont ensuite nettoyés :
#' - suppression du préfixe "_" 
#' - remplacement de "/" par "."
#'
#' @param path_raw_file Chemin du fichier brut (.xlsx).
#' @param path_survey Chemin du fichier Kobo survey (.xlsx). 
#' @param sheet Numéro ou nom de la feuille du fichier brut (par défaut 1).
#' @param sheet_survey Feuille contenant le survey (par défaut 2).
#'
#' @return Un tibble avec les types de colonnes correctement assignés et des noms propres.
#' @export
#'
#' @examples
#' \dontrun{
#' raw <- read_and_clean(
#'   path_raw_file = "data/04_preclean/msna25_preclean.xlsx",
#'   path_survey   = "data/01_docs/kobo/mli_msna_2025.xlsx",
#'   sheet_survey  = 2
#' )
#' }
#'
read_and_clean <- function(path_raw_file,
                           path_survey,
                           sheet = 1,
                           sheet_survey = 2) {
  # Vérifications
  if (!file.exists(path_raw_file)) {
    stop("Le fichier brut n'existe pas : ", path_raw_file)
  }
  if (!file.exists(path_survey)) {
    stop("Le fichier survey n'existe pas : ", path_survey)
  }
  
  # Charger le survey
  survey <- readxl::read_excel(path_survey, sheet = sheet_survey)
  
  if (!"type" %in% names(survey) || !"name" %in% names(survey)) {
    stop("Le fichier survey doit contenir au moins les colonnes 'type' et 'name'.")
  }
  
  # Extraire les variables numériques et texte
  var_numerique <- survey %>%
    dplyr::filter(.data$type %in% c("integer", "decimal")) %>%
    dplyr::pull(.data$name)
  
  var_text <- survey %>%
    dplyr::filter(.data$type %in% c("text", "select_one", "select_multiple")) %>%
    dplyr::pull(.data$name)
  
  # Lire seulement les noms de colonnes du fichier brut
  raw_tmp <- readxl::read_excel(path_raw_file, sheet = sheet, n_max = 0)
  
  # Trouver les positions
  pos_var_numerique <- match(var_numerique, names(raw_tmp)) %>% stats::na.omit()
  pos_var_text <- match(var_text, names(raw_tmp)) %>% stats::na.omit()
  
  # Construire le vecteur col_types
  col_types_vec <- rep("guess", ncol(raw_tmp))
  if (length(pos_var_numerique) > 0) col_types_vec[pos_var_numerique] <- "numeric"
  if (length(pos_var_text) > 0) col_types_vec[pos_var_text] <- "text"
  
  # Relire avec typage correct
  raw <- readxl::read_excel(path_raw_file, sheet = sheet, col_types = col_types_vec)
  
  # Nettoyer les noms de colonnes
  names(raw) <- gsub("^_", "", names(raw))
  names(raw) <- gsub("/", ".", names(raw))
  
  return(raw)
}
