# straight_path
#
# Author: Roger Beecham
###############################################################################


# Takes a df defining an OSGB OD pair and count. 
# Returns a df of coord pairs representing OD.
get_trajectory_line <- function(data) {
  o_x=data$o_x
  o_y=data$o_y
  d_x=data$d_x
  d_y=data$d_y 
  od_pair=data$od_pair  
  count=data$count

  d <- tibble(
    x=c(o_x,d_x),
    y=c(o_y,d_y),
    od_pair=od_pair,
    count=count
  )
  return(d)
}