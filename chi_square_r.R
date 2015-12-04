x2_gof <- function(test=FALSE) #chi-square goodness of fit to see if a sample of data matches a population with a specific distribution
  #test is an optional parameter that defaults to FALSE, can be turned on to run in testing mode, which prints additional information
{
  parse(test) #parses raw csv file
  
  f_obs <- readline(prompt = "Select observed frequency column by name or number: ")
  
  #test if letter or number
  if (is.numeric(suppressWarnings(as.integer(f_obs))) & !is.na(suppressWarnings(as.integer(f_obs)))){
    f_obs <- as.integer(f_obs)
  }
  
  if (test==TRUE){
    print("[Section 3]")
    print(paste("observed frequency name: ",f_obs))
  }
  
  f_exp <- readline(prompt = "Select expected frequency column by name or number. If evenly distributed, leave blank: ")
  
  if (f_exp==""){
    #evenly distributed data
    if (test==TRUE){
      print("[Section 4]")
      print("Evenly distributed data")
    }
    
    print(chisq.test(rawData[f_obs]))
    
  } else {
    
    #test if letter or number
    if (is.numeric(suppressWarnings(as.integer(f_exp))) & !is.na(suppressWarnings(as.integer(f_exp)))){
      f_exp <- as.integer(f_exp)
    }
    
    norm_exp <- unlist(rawData[f_exp])
   
    if (test==TRUE){
      print("[Section 4]")
      print(paste("expected frequency name: ", f_exp))
      print("expected frequencies: ")
      print(norm_exp)
    }
    
    tryCatch(print(chisq.test(rawData[f_obs], p=norm_exp)),
             error = function(e) {print(paste("Error: expected frequency percents do not add to 1"));
            })
    
  }
  
}

x2_ind <- function(test=FALSE) #chi-square test for independence, used to determine if there is a significant association between two variables
{
  parse(test)
  
  colns <- readline(prompt = "Select column(s) by name or number: ")
  colns <- gsub(" ","",colns, fixed=TRUE)
  colns <- unlist(strsplit(colns,","))


#parses to see if any thing with colons need to be expanded  
  for (i in 1:length(colns)){ #loops through every element in colns
    if (grepl(":",colns[i])==TRUE){
      x <- unlist(strsplit(colns[i],":")) #x is temporary storage of split :
      lenmult <- as.integer(x[2]) - as.integer(x[1])
      
      colns <- colns[-i] #removes element with ":"
      
      #adds values between upper and lower limits
      for (j in 0:lenmult){
        colns <- c(colns,as.integer(x[1])+j)
      }
    }
  }
  
  #test if letter or number
  if (is.numeric(suppressWarnings(as.integer(colns[1]))) & !is.na(suppressWarnings(as.integer(colns[1])))){
    colns <- as.integer(colns)
  }
  
  if (test==TRUE){
    print("[Section 3]")
    print("columns selected: ")
    print(colns)
  }

  data <- subset(rawData, select=c(colns))
  rXsq <- chisq.test(data)
  print(rXsq)
  
}

parse <- function(test) #file input handler. opens .csv file, reads column names, and copies rawdata.
{
  
  if (test==TRUE){
    print("[Section 1]")
  }
  
  print("Here are .csv files in current directory: ")
  print(list.files(pattern = "\\.csv$")) #only opens .csv files
  
  filename <- readline(prompt = "Enter .csv file name without extension (ex test instead of test.csv): ")
  
  while (file.exists(paste0(filename,".csv"))==FALSE){ #checks to see if file exists before opening
    print("File does not exist. ")
    filename <- readline(prompt = "Enter .csv file name: ")
  }
  
  #reads first row to get headers
  coltitles <- read.csv(file=paste0(filename,".csv"), nrows=1, head=FALSE)
  newcol <<- t(coltitles)
  colnames(newcol) <<- c('ColnNames')

  if (test==TRUE){
    print("[Section 2]")
  }
    
  #display header in table
  print(newcol)
  rawData <<- read.csv(file=paste0(filename,".csv"), head=TRUE, sep=",")
  
}

changewd <- function() #changes working directory
{
  print(paste("Current working directory: ",getwd()))
  path <- readline(prompt = "Enter filepath to .csv data file: ")
  setwd(path)
  print(paste("New working directory: ",getwd()))
  
}

#changewd() #calls function to change wd upon loading file



errorchecking <- function(){
  for(input in inputs) {
   tryCatch(print(paste("log of", input, "=", log(input))),
      warning = function(w) {
        print(paste("negative argument", input)); 
        log(-input)},
      error = function(e) {print(paste("non-numeric argument", input));
      NaN})
  }
  
}