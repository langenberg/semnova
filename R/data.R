#' Example data from reading research.
#'
#' @rdname reading
#'
#' @details
#' \code{reading_raw} is the raw data in long data format.
#'
#' \code{reading_latent} is based on \code{reading_raw} but the variable
#' \code{dv} was log-transformed and standardized. The data set only includes
#' measurements at grade 1, 2 and 4.
#'
#' \code{reading_manifest} is based on \code{reading_latent} and was transposed
#' to wide format introducing the three additional variables: \code{gaze_dur},
#' \code{tv_dur} and \code{ini_fix_dur}. The names for the variables were taken
#' from \code{indicator} and the values were taken from \code{dv}.
#'
#' @format
#' \describe{
#'     \item{id}{Unique identifier.}
#'     \item{dv}{Continuous dependent variable. The variable is on its raw scale
#'     for \code{reading_raw}. The variable is log-transformed and
#'     standardized (grouped by indicator) for \code{reading_latent}.}
#'     \item{indicator}{Includes the names of three indicators measuring reading
#'     skills (also see gaze_dur, tv_dur and ini_fix_dur).}
#'     \item{sentence}{The type of the sentence: sentence vs. landolt.}
#'     \item{grade}{Grade variable: grade0 to grade5.}
#'     \item{gaze_dur}{Mean gaze duration per participant.}
#'     \item{tv_dur}{Mean total viewing duration per participant}
#'     \item{ini_fix_dur}{Mean initial fixation duration per participant.}
#'     \item{age_t0}{Age at grade 0.}
#'     \item{sex}{Sex.}
#'     \item{grade_t0}{Grade at grade 0. Important if participant had to repeat a year.}
#'     \item{iq}{IQ.}
#'     \item{cbcl_ges_tw_t0}{Child Behavior Checklist. Overall t-score at grade 0.}
#'     \item{cbcl_int_verh_tw_t0}{Child Behavior Checklist. Internalizing behavior t-score at grade 0.}
#'     \item{cbcl_ext_verh_tw_t0}{Child Behavior Checklist. Externalizing behavior t-score at grade 0.}
#'     \item{cbcl_ges_rw_t0}{Child Behavior Checklist. Overall raw score at grade 0.}
#'     \item{cbcl_int_verh_rw_t0}{Child Behavior Checklist. Internalizing behavior raw score at grade 0.}
#'     \item{cbcl_ext_verh_rw_t0}{Child Behavior Checklist. Externalizing behavior raw score at grade 0.}
#'     \item{cbcl_ges_tw_t5}{Child Behavior Checklist. Overall t-score at grade 5.}
#'     \item{cbcl_int_verh_tw_t5}{Child Behavior Checklist. Internalizing behavior t-score at grade 5.}
#'     \item{cbcl_ext_verh_tw_t5}{Child Behavior Checklist. Externalizing behavior t-score at grade 5.}
#'     \item{cbcl_ges_rw_t5}{Child Behavior Checklist. Overall raw score at grade 5.}
#'     \item{cbcl_int_verh_rw_t5}{Child Behavior Checklist. Internalizing behavior raw score at grade 5.}
#'     \item{cbcl_ext_verh_rw_t5}{Child Behavior Checklist. Externalizing behavior raw score at grade 5.}
#' }
#'
#' @example
#'
#' if (FALSE) {
#'     data(reading_raw)
#'
#'     reading_latent <- reading_raw %>%
#'         group_by(indicator) %>%
#'         mutate(dv = as.vector(scale(log(dv)))) %>%
#'         ungroup()
#'
#'     reading_manifest <- reading_latent %>%
#'         pivot_wider(names_from = "indicator", values_from = "dv") %>%
#'         select(id, sentence, grade, gaze_dur, tv_dur, ini_fix_dur, 4:20)
#' }
#'
"reading_raw"


#' @rdname reading
#' @format
"reading_latent"



#' @rdname reading
#' @format
"reading_manifest"
