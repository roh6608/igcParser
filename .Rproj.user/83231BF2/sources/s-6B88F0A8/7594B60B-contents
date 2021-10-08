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
    len = recordLength(file, "B")
    print(len)
    time = vector(length = len,mode ="character")
    latitude = vector(length = len,mode ="character")
    longitude = vector(length = len,mode ="character")
    fixVal = vector(length = len,mode ="character")
    pressAlt = vector(length = len,mode ="character")
    gnssAlt = vector(length = len,mode ="character")
    j = 1

    for(i in 1:length(file)){
      if(substring(file[i],1,1) == "B"){
        time[j] = substring(file[i],2,7)
        latitude[j] = substring(file[i],8,15)
        longitude[j] = substring(file[i],16,24)
        fixVal[j] = substring(file[i],25,25)
        pressAlt[j] = substring(file[i],26,30)
        gnssAlt[j] = substring(file[i],31,35)
        j = j + 1
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

