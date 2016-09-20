rm(list=ls())
library(XML)

url.stub = "http://afni.nimh.nih.gov"
help.url = file.path(url.stub, 
	"pub/dist/doc/program_help/index.html")

doc <- htmlTreeParse(help.url, asText=FALSE, useInternal=TRUE)

cols = xpathSApply(doc, "//table//tr", xmlGetAttr, "bgcolor")

urls = xpathSApply(doc, "//table//td//a", xmlGetAttr, "href")

readme = min(which(grepl("README", urls)))
urls = urls[1:(readme-1)]

urls = file.path(url.stub, urls)

url = urls[1]

urldoc <- htmlTreeParse(url, asText=FALSE, useInternal=TRUE)
