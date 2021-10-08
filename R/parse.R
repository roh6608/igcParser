#' Parse an igc file
#'
#' @param filepath The filepath to the igc file.
#' @param record A string to select which record is to be parsed, defaults to "B".
#' Can be any record from A-L.
#' @return The specified record from the igc file in a dataframe.
parse = function(filepath, record = "B"){
  file = readLines(filepath)
  # add error handling up here the whole way along, do not want to give them
  # any chance to shut me down, think of just about every edge case and
  # how the program can exit gracefully with a warning that helps
  # write parser for the other parts of the igc file, most should be easy
  # leave the I record stuff till last, can try and get on cran before doing that
  # can also add viggnettes
  # probably can finish by the weekend
  # can use the date from the A record for the strptime
  # will need to write more in the documentation about which records can be used

  if((record == "B") | (record == "b")){
    time = vector(length = length(file),mode ="character")
    latitude = vector(length = length(file),mode ="character")
    longitude = vector(length = length(file),mode ="character")
    fixVal = vector(length = length(file),mode ="character")
    pressAlt = vector(length = length(file),mode ="character")
    gnssAlt = vector(length = length(file),mode ="character")

    for(i in 1:length(file)){
      if(substring(file[i],1,1) == "B"){
        time[i] = substring(file[i],2,7)
        latitude[i] = substring(file[i],8,15)
        longitude[i] = substring(file[i],16,24)
        fixVal[i] = substring(file[i],25,25)
        pressAlt[i] = substring(file[i],26,30)
        gnssAlt[i] = substring(file[i],31,35)
      }
    }

    time = strptime(time,"%H%M%S")
    latitude = latParse(latitude)
    longitude = lonParse(longitude)
    pressAlt = as.numeric(pressAlt)
    gnssAlt = as.numeric(gnssAlt)

    igc = cbind.data.frame(time,latitude,longitude,pressAlt,gnssAlt)
    colnames(igc) = c("Time","Latitude","Longitude","Pressure_Altitude","GNSS_Altitude")

  }
  return(igc)
}

