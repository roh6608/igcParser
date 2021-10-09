#' Parse the latitude string in the B record
#'
#' @param string The latitude string or vector of strings from the B record in DDMMmmmN/S format
#' @return The latitude in decimal degrees.
#' @examples
#' latParse("1024361N")
latParse = function(string){
  if(is.character(string) == T){
    deg = vector(length=length(string))
    min = vector(length=length(string))
    minDec = vector(length=length(string))
    hem = vector(length=length(string))

    for(i in 1:length(string)){
      if(nchar(string[i]) == 0){
        warning("Empty latitude string.")
        next
      } else{
          deg[i] = substring(string[i],1,2)
          min[i] = substring(string[i],3,4)
          minDec[i] = paste0("0.",substring(string[i],5,7))
          hem[i] = substring(string[i],8,8)

          deg[i] = as.numeric(deg[i]) + (as.numeric(min[i])+as.numeric(minDec[i]))/60

          if(hem[i] == "N"){
            deg[i] = deg[i]
          } else if(hem[i] == "S"){
            deg[i] = -deg[i]
          } else{
            stop("Malformed latitude string, should end with N or S.")
          }
      }
    }
  } else{
      stop("Incorrect type entered, please pass variable of type character.")
    }

  return(deg)
}

#' Parse the latitude string in the B record
#'
#' @param string The longitude string or vector of strings from the B record in DDDMMmmmE/W format
#' @return The longitude in decimal degrees.
#' @examples
#' lonParse("10243461E")
lonParse = function(string){
  if(is.character(string) == T){
    deg = vector(length=length(string))
    min = vector(length=length(string))
    minDec = vector(length=length(string))
    hem = vector(length=length(string))

    for(i in 1:length(string)){
      if(nchar(string[i]) == 0){
        warning("Empty longitude string.")
        next
      } else{
        deg[i] = substring(string[i],1,3)
        min[i] = substring(string[i],4,5)
        minDec[i] = paste0("0.",substring(string[i],6,8))
        hem[i] = substring(string[i],9,9)

        deg[i] = as.numeric(deg[i]) + (as.numeric(min[i])+as.numeric(minDec[i]))/60

        if(hem[i] == "E"){
          deg[i] = deg[i]
        } else if(hem[i] == "W"){
          deg[i] = -deg[i]
        } else{
          stop("Malformed longitude string, should end with E or W.")
        }
      }
    }
  } else{
    stop("Incorrect type entered, please pass variable of type character.")
  }

  return(deg)
}

#' Give the amount of entries for a certain record
#' @param charVec The vector of characters to operate on.
#' @param record The record to find the amount of entries for.
#' @return The amount of entries for the given record.
#' @examples
#' recordLength("test.igc","B")
recordLength = function(charVec, record){
  records = LETTERS[seq(1,12)]
  recordsLow = letters[seq(1,12)]
  len = 0

  if(is.character(charVec)){
    for(i in 1:length(records)){
      if((records[i] == record) | (recordsLow[i] == record)){
        for(j in 1:length(charVec)){
          if((records[i] == substring(charVec[j],1,1))){
            len = len + 1
          }
        }
      }
    }
  } else{
    stop("Incorrect type, please pass variable of type character.")

}
  return(len)
}
