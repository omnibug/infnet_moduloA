# -------------------------
# Change Work Dir
# -------------------------
change_work_dir <- function(home_dir, work_dir) {
  if (str_sub(home_dir,nchar(work_dir) * -1, -1) != work_dir){
    new_home = paste(home_dir,work_dir, sep = "")
    setwd(new_home)
    print(paste('Changed to:',getwd()))
  }
  else{
    print(paste('Work Dir already current:',getwd()))
  }
  
}
