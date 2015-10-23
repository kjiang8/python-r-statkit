works <- function(){
  M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
  dimnames(M) <- list(gender = c("F", "M"),
                      party = c("Democrat","Independent", "Republican"))
  Xsq <- chisq.test(M)
  print(Xsq)
}


readinteger <- function(n)
{ 

  if(!grepl("^[0-9]+$",n))
  {
    return("FALSE")
  }
  return(as.integer(n))
}

opencsv <- function()
{
  filename <- readline(prompt = "Enter .csv file name: ")
  #global var
  rawData <<- read.csv(file=paste0(filename,".csv"), head=TRUE, sep=",") #paste0 removes space
  print(rawData)
  rXsq <- chisq.test(rawData)
  rXsq
}

x2_gof <- function()
{

  filename <- readline(prompt = "Enter .csv file name: ")
  
  #reads titles
  coltitles <<- read.csv(file=paste0(filename,".csv"), nrows=1, head=FALSE) #paste0 removes space
  print(coltitles)
  
  f_obs <- readline(prompt = "Select observed frequency column by name or number: ")
  
  #test if letter or number
  if (is.numeric(suppressWarnings(as.integer(f_obs))) & !is.na(suppressWarnings(as.integer(f_obs)))){
    f_obs <- as.integer(f_obs)
  }
  
  f_exp <- readline(prompt = "Select expected frequency column by name or number. If evenly distributed, leave blank: ")
  print(f_exp)
  
  rawData <<- read.csv(file=paste0(filename,".csv"), head=TRUE, sep=",")
  
  if (f_exp==""){
  
    #evenly distributed data
    rXsq <- chisq.test(rawData[f_obs])
    print(rXsq)
    
  } else {
    
    #test if letter or number
    if (is.numeric(suppressWarnings(as.integer(f_exp))) & !is.na(suppressWarnings(as.integer(f_exp)))){
      f_exp <- as.integer(f_exp)
    }

    sum_exp <- unlist(sum(rawData[f_exp]))
    norm_exp <- unlist(rawData[f_exp]/sum_exp)
    
    rXsq <- chisq.test(rawData[f_obs], p=norm_exp)
    print(rXsq)
    
  }

}

x2_ind <- function()
{
  filename <- readline(prompt = "Enter .csv file name: ")
  
  #reads titles
  coltitles <<- read.csv(file=paste0(filename,".csv"), nrows=1, head=FALSE) #paste0 removes space
  print(coltitles)
  
  colns <- readline(prompt = "Select column(s) by name or number: ")
  
  colns <- unlist(strsplit(colns,","))
  
  #test if letter or number
  if (is.numeric(suppressWarnings(as.integer(colns[1]))) & !is.na(suppressWarnings(as.integer(colns[1])))){
    colns <- as.integer(colns)
  }
  
  rawData <<- read.csv(file=paste0(filename,".csv"), head=TRUE, sep=",")

  #select = c(1:4)
  data <- subset(rawData, select=c(colns))

  print(data)
  rXsq <- chisq.test(data)
  print(rXsq)
  
}

