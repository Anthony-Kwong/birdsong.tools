#' get_var_score_dep2
#' 
#' Dependency function for gap_var_score. Not to be used alone. Computes the weighted 
#' population mean coefficient of variation for a transition type. 
#'
#' @param transition: A string indicating the transition type.
#' @param gap_table: a gap table produced from the get_gaps function
#'
#' @return A numeric variable for the mean coefficient of variantion
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @export
#'
#' @examples   unit_table1 = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.95, 1.8, 2.5),
#' end = c(0.45, 0.7, 0.9, 1.4, 2, 3),
#' selec = seq(6),
#' sound.files = c(rep("JS001.wav",6)),
#' note_label = c(rep("A",3), rep("B",3) ))
#' unit_table2 = tibble::tibble(start = c(0.30, 0.5, 0.75, 0.95, 1.8, 2.5),
#'                              end = c(0.45, 0.7, 0.9, 1.4, 2, 3),
#'                              selec = seq(6),
#'                              sound.files = c(rep("JS002.wav",6)),
#'                              note_label = c(rep("A",2), rep("B",4) ))
#' gap_table = get_gaps(rbind(unit_table1,unit_table2))
#' 
#' transition = "B-B"
#' output = get_var_score_dep2(transition, gap_table)

get_var_score_dep2 <- function(transition, gap_table){
  #find each bird which sings the transition
  trunc_table = gap_table %>%
    dplyr::filter(transitions == transition)
  birds = unique(trunc_table$sound.files)
  #compute contribution for each bird
  top = sapply(birds, function(b){
    data = trunc_table %>%
      dplyr::filter(sound.files == b)
    Cjb = get_var_score_dep(data)
    njb = nrow(data)
    num = Cjb*(njb - 1)
    return(num)
  })
  bottom = sapply(birds, function(b){
    data = trunc_table %>%
      dplyr::filter(sound.files == b)
    njb = nrow(data)
    return(njb - 1)
  })
  #print(top)
  #print(bottom)
  #if only one occurrence of a transition, return 0
  if(sum(bottom) == 0){
    return(0)
  } else {
    Cj = sum(top)/sum(bottom)
    return(Cj)
  }
}
