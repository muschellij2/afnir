##########
# Parse arguments for align_epi_anat
##########
rm(list=ls())
options(afni.path="/Users/johnmuschelli/afni")
library(stringr)
library(plyr)
library(zoo)
x = readLines("txts/align_epi_anat.txt")
x = str_trim(x)
x = gsub("\\s+", " ", x)
x = x[x != ""]
df = data.frame(x=x, arg = grepl("^-", x), stringsAsFactors = FALSE)
df$id = cumsum(df$arg)
df$use = grepl("^use:", df$x)
df$allowed = grepl("^allowed:", df$x)
df$default = grepl("^default:", df$x)
cols = c("arg", "use", "allowed", "default")
df$type = apply(df[, cols], 1, function(x) {
  ind = which(x)
  if (length(ind) == 0){
    return(NA)
  }
  cols[ind]
})
df$type = na.locf(df$type)

x =df[ df$id == 1,]
df = ddply(df, .(id), function(x){
  arg = gsub("^-", "", x$x[ x$type == "arg"])
  default = x$x[ x$type == "default"]
  use = x$x[ x$type == "use"]
  allowed = x$x[ x$type == "allowed"]
  default = paste(default, collapse = " ")
  use = paste(use, collapse = " ")
  allowed = paste(allowed, collapse = " ")
  use = str_trim(gsub("use:", "", use))
  allowed = str_trim(gsub("allowed:", "", allowed))
  default = str_trim(gsub("default:", "", allowed))
  c(arg=arg, use=use, default=default, allowed=allowed)
})
df$id = NULL
