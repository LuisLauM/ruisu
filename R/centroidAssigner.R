centroidAssigner <- function(isoCode, old = TRUE)
{
  if(old)
    isoAreas <- data1 else
      isoAreas <- data2
    
  return(data.frame(lon = isoAreas[match(isoCode, isoAreas$code), "lon"],
                    lat = isoAreas[match(isoCode, isoAreas$code), "lat"]))
}