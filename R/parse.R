#' Parse an igc file
#'
#' @param filepath The filepath to the igc file.
#' @param record A string to select which record is to be parsed, defaults to "B".
#' Can be any record A through L.
#' @return The specified record from the igc file in a dataframe.
parse = function(filepath, record = "B"){
  file = readLines(filepath)

  if((record == "A") | record == "a"){
    len = 1
    manId = vector(length = len, mode = "character")
    unqId = vector(length = len, mode = "character")
    optId = vector(length = len, mode = "character")

    if(substring(file[1],1,1) == "A"){
      manId = substring(file[1],1,3)
      unqId = substring(file[1],4,6)
      optId = substring(file[1],7)

      igc = cbind.data.frame(manId, unqId, optId)
      colnames(igc) = c("Manufacturer_ID","Unique_ID","Optional_Extension")
    } else{
      stop("Malformed IGC format, first line must be the A record.")
    }


  } else if((record == "B") | (record == "b")){
    len = recordLength(file, record)
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

  } else if((record == "C") | record == "c"){
    stop("Not yet implemented.")
  } else if((record == "D") | record == "d"){
    len = recordLength(file,"D")
    if(len == 0){
      stop("This was not logged with a differential GPS, thus does not contain any D records.")
    } else{
      qual = vector(length = len, mode = "character")
      statId = vector(length = len, mode= "character")

      j = 1
      for(i in 1:length(file)){
        if(substring(file[i]) == "D"){
          qual[j] = substring(file[i],2,2)
          statId[j] = substring(file[i],3,6)
          j = j + 1
        }
      }

      igc = cbind.data.frame(qual,statId)
      colnames(igc) = c("GPS_qualifier","DGPS_Station_ID")
    }
  } else if((record == "E") | (record == "e")){
    stop("Not yet implemented")
  } else if((record == "F") | (record == "f")){
    len = recordLength(file,"F")
    # need to add checks for the required entries and stop if their is none
    time = vector(length = len, mode = "character")
    satId = vector(length = len, mode = "character")

    j = 1
    for(i in 1:length(file)){
      if(substring(file[i],1,1) == "F"){
        time[j] = substring(file[i],2,7)
        satId[j] = substring(file[i],8)
      }
    }
    time = strptime(time,"%H%M%S")

    igc = cbind.data.frame(time,satId)
    colnames(igc) = c("Time","Satellite_IDs")
  } else if((record == "G") | (record == "g")){
    len = recordLength(file,"G")

    if(len == 0){
      stop("Malformed IGC format, must contain G record.")
    } else{
      secCode = vector(length = len, mode = "character")

      for(i in 1:length(file)){
        if(i > 1){
          stop("Malformed IGC format, must only contain one G record.")
        } else{
          secCode = substring(file[i],2)
        }
      }

    }
  } else if((record == "H") | (record == "h")){
    len = recordLength(file,'H')
  }
  return(igc)
}

# consider re-writing this so that it parses each and return a list object
# this would remove the need for a lot of flow control, could also then
# isolate the different records into different functions, simplifying testing
# and making the code more maintainable, only issue is that I would need to make
# it a one pass function for speed, will have to run the recordLength function
# though for each record, should be quick enough, and just makes alot more sense
# overall
