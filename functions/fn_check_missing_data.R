# check for missing data
check_missing_data <- function(df){ 
  # define a function to check for missing data
  check_data <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x)))
  # run the previous functin over dataset passed as parameter
  data_missing <- check_data(df)
  # iterate over dataset, printing missing values
  for(i in names(data_missing)){
    print (paste('Column:',i,unname(unlist(data_missing[i][1]))[1],'missing values'))
  }
}
