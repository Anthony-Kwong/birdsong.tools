#' overlap_score function
#' 
#' Computes the overlap scores between 2 notes. The scores are defined as follows.
#' \eqn{score1 = (y \cap z) / y},
#' \eqn{score2 = (y \cap z) / (y \cup z)},
#' \eqn{score3 = |(y \cap z)|^2 / |y| \times |z|}
#'
#' @param note1 : A 2-dim numeric vector. Designated as y. 
#' @param note2 : A 2-dim numeric vector. Designated as z. 
#'
#' @return A numeric vector for scores1,2,3. 
#' @export
#'
#' @examples note1 = c(1,5), note2 = c(3,4)
#' overlap_score(note1,note2)
overlap_score <- function(note1,note2){
  
  #check inputs
  if(length(note1) != 2 || is.numeric(note1) != T ){
    stop("note1 must be a 2-dim numeric vector")
  }
  
  if(length(note2) != 2 || is.numeric(note2) != T ){
    stop("note2 must be a 2-dim numeric vector")
  }
  
  #check interval vector is sorted
  if(note1[1] > note1[2] || note2[1] > note2[2]){
    stop("end points are earlier than start points in input intervals")
  }
  
  #function starts here
  x = check_overlap(note1,note2)
  y = note1[2] - note1[1]
  z = note2[2] - note2[1]
  a = max(note1,note2) - min(note1,note2)
  
  res = c( x/y, x/a, (x^2)/(y*z) )
  
  return(res)
}