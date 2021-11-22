#' check_overlap function
#' 
#' Takes 2 intervals and computes how much they overlap. 
#'
#' @param note1 : A 2-dim numeric vector. 
#' @param note2 : A 2-dim numeric vector. 
#'
#' @return A numeric scalar value indicating the amount of overlap. If
#' the intervals do not overlap, the value is 0. 
#' @export
#'
#' @examples note1 = c(1,5), note2 = c(3,4)
#' check_overlap(note1, note2)
check_overlap = function(note1, note2){
  
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
  
  #function starts here ----
  
  #check if one note is entirely contained in another
  
  #case: note2 contained entirely in note1
  if(note2[1]>note1[1] & note1[2]>note2[2]){
    return(note2[2] - note2[1])
  }
  #case: note1 contained entirely in note2
  if(note1[1]>note2[1] & note1[2]<note2[2]){
    return(note1[2] - note1[1])
  }
  
  #Cases with partial overlap
  
  olap = (note1[1] <= note2[2]) & (note1[2] >= note2[1])
  
  if(olap){
    #case: note1 is on the right
    if(note1[1] > note2[1]){
      #print("note1 on right")
      res = note2[2] - note1[1]
    } else {
      #print("note1 on left")
      #case: note2 is on the left
      res = note1[2]- note2[1]
    }
  } else {
    #case: no overlap
    res = 0
  }
  
  return(res)
}

# 
# rng = cbind(pmin(ranges[,1], ranges[,2]), pmax(ranges[,1], ranges[,2]),
#             pmin(ranges[,3], ranges[,4]), pmax(ranges[,3], ranges[,4]))
# 
# olap = (rng[,1] <= rng[,4]) & (rng[,2] >= rng[,3])
# 
# (pmin(ranges[,1], ranges[,2]) <= pmax(ranges[,3], ranges[,4])) &
#   (pmax(ranges[,1], ranges[,2]) >= pmin(ranges[,3], ranges[,4]))