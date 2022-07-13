#' get_var_score function
#' 
#' Takes a gap_table for an individual and computes the overall variability score
#' for an individual bird. 
#' 
#' The function first sorts the gaps by transition type and computes a score for each transition. This 
#' is the variance of the gaps, divided by their mean. The final score is a linear combination of the 
#' scores for each transition. The constant for the score for transition type AB is the total number 
#' AB transitions divided by the total number of transitions - the number of transition types. 
#'  
#' The score is the variance of all the gaps, divided by their mean. 
#'
#' @param gap_table: A tibble as produced using the get_gaps function. It has Bird.ID, gap_dur,
#' selec and transitions. 
#' @param ID: The ID of the bird to compute the gap variance score for. 
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter 
#' @importFrom tibble tibble
#'
#' @return A numeric, scalar score for the bird
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.2, 1.8, 2.5), 
#' end = c(0.45, 0.7, 0.9, 0.4, 2, 3), 
#' selec = c(1,2,3,1,2,3),
#' sound.files = c(rep("JS001.wav",6)), 
#' note_label = rep(c("A","B","C"),2))
#' output = get_gaps(unit_table)
#' get_var_score_dep(output)
gap_var_score <- function(gap_table, ID){
  #get individual table for the selected bird
  bird_data = gap_table %>%
    dplyr::filter(Bird.ID == ID)
  
  #get all available transitions for the bird across all its recordings
  trans = unique(bird_data$transitions)
  #compute scores across all transition types
  Cjb = lapply(trans, function(transition){
    trans_table = dplyr::filter(bird_data, transitions == transition)
    #compute the var score for that transition type
    score = get_var_score_dep(trans_table)
    #record the number of occurrences for that transition
    count = nrow(trans_table)
    res = tibble::tibble(score = score, transition = transition)
    return(res)
  })
  Cjb = do.call(rbind, Cjb)
  
  #compute the weighted population mean coefficient of variation for each transition type
  #input: transition type, population gap table
  Cj = lapply(trans, function(t){
    coef = get_var_score_dep2(t, gap_table)
    tibble::tibble(coef, transition = t)
  })
  Cj = do.call(rbind, Cj)
  
  #compute Zjb, the log ratio Cjb/Cj
  ntrans = nrow(Cjb)
  Z = lapply(seq(ntrans),function(x){
    top = Cjb[x,]
    bot = Cj[x,]
    trans = top$transition
    if(bot$coef == 0){
      output = tibble::tibble(Zjb = 0, transition = trans) 
      return(output)
    } else {
      ratio = log(top$score/bot$coef)
      output = tibble::tibble(Zjb = ratio, transition = trans)
      return(output)
    }
  })
  Z = do.call(rbind, Z)
  
  #compute G
  
  G = sapply(seq(ntrans), function(x){
    d = Z[x,]
    trans = d$transition
    trans_table = dplyr::filter(bird_data, transitions == trans)
    nj = nrow(trans_table)
    (nj-1)*d$Zjb
  })
  
  # test = tibble::tibble(Cjb = Cjb, Cj = Cj, Z = Z, G)
  # print(test)
  
  #total number of transitions/gaps produced by bird
  Nb = nrow(bird_data)
  #total number of transition types produced by bird b
  kb = length(unique(bird_data$transitions))
  #final score for bird b
  Gb = 1/(Nb - kb)*sum(G)
  
  return(Gb)
}