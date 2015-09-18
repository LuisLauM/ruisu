#' @name centroidAssigner
#' @aliases centroidAssigner
#' @title Returns centroid values from Isoparalitoral-area codes.
#' 
#' @description The function takes a vector of isoparalitoral Areas and returns centroids, i.e. latitude and longitude values.
#' \code{old} parameter is used to indicate the type of shape used: Ancient (elaborated, roughly, 
#' in 1992; it has errors respect coast line) and New (made in 2012 from a shape of coastline more accurate). 
#' The variations between the values -about centroid- ranging from a few meters (for nearshore areas) to almost 
#' half a nautical mile (for those further away from the coast).
#' 
#' @usage centroidAssigner(isoCode, old = TRUE)
#' @param isoCode Vector of isoparalitoral-areas codes.
#' @param old \code{logical}. Do you want to use "Old" shape of Isoparalitoral Areas?
#' 
#' @example areaCodes <- c(1050, 4043, 17073, 27103)
#' centroidAssigner(isoCode)
centroidAssigner <- function(isoCode, old = TRUE)
{
  if(old)
    isoAreas <- data1 else
      isoAreas <- data2
    
  return(data.frame(lon = isoAreas[match(isoCode, isoAreas$code), "lon"],
                    lat = isoAreas[match(isoCode, isoAreas$code), "lat"]))
}