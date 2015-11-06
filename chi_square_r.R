x2_gof <- function(test=FALSE) #chi-square goodness of fit
{
  parse()
  
  f_obs <- readline(prompt = "Select observed frequency column by name or number: ")
  
  #test if letter or number
  if (is.numeric(suppressWarnings(as.integer(f_obs))) & !is.na(suppressWarnings(as.integer(f_obs)))){
    f_obs <- as.integer(f_obs)
  }
  
  if (test==TRUE){
    print(paste("observed frequency name: ",f_obs))
  }
  
  f_exp <- readline(prompt = "Select expected frequency column by name or number. If evenly distributed, leave blank: ")
  
  if (f_exp==""){
    #evenly distributed data
    if (test==TRUE){
      print("Evenly distributed data")
    }
    rXsq <- chisq.test(rawData[f_obs])
    print(rXsq)
    
  } else {
    
    #test if letter or number
    if (is.numeric(suppressWarnings(as.integer(f_exp))) & !is.na(suppressWarnings(as.integer(f_exp)))){
      f_exp <- as.integer(f_exp)
    }
    
    sum_exp <- unlist(sum(rawData[f_exp]))
    norm_exp <- unlist(rawData[f_exp]/sum_exp)
   
    if (test==TRUE){
      print(paste("expected frequency name: ", f_exp))
      print("expected frequency normalized: ")
      print(norm_exp)
    }
    
    rXsq <- chisq.test(rawData[f_obs], p=norm_exp)
    print(rXsq)
    
  }
  
}

x2_ind <- function(test=FALSE) #chi-square test for independence
{
  parse()
  
  colns <- readline(prompt = "Select column(s) by name or number: ")
  colns <- unlist(strsplit(colns,","))
  
  #grepl(valu, chars)
  
  #test if letter or number
  if (is.numeric(suppressWarnings(as.integer(colns[1]))) & !is.na(suppressWarnings(as.integer(colns[1])))){
    colns <- as.integer(colns)
  }
  
  if (test==TRUE){
    print("columns selected: ")
    print(colns)
  }

  #select = c(1:4)
  data <- subset(rawData, select=c(colns))
  rXsq <- chisq.test(data)
  print(rXsq)
  
}

parse <- function()
{
  print("Here are .csv files in current directory: ")
  print(list.files(pattern = "\\.csv$"))
  
  filename <- readline(prompt = "Enter .csv file name without extension (ex test instead of test.csv): ")
  
  while (file.exists(paste0(filename,".csv"))==FALSE){
    print("File does not exist. ")
    filename <- readline(promp = "Enter .csv file name: ")
  }
  
  #reads titles
  coltitles <- read.csv(file=paste0(filename,".csv"), nrows=1, head=FALSE)
  newcol <<- t(coltitles)
  colnames(newcol) <<- c('ColnNames')
  
  #display header in table
  print(newcol)
  rawData <<- read.csv(file=paste0(filename,".csv"), head=TRUE, sep=",")
  
}

