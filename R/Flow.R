Flow <- setRefClass("Flow",fields = list(steps="list",._parms="list"))


#steps=list("readFile"=readFile(),"cleanup"=cleanup())

init <- function(steps_list,...){
  i <- 1
  step_names=names(steps_list)
  for (st in steps_list) {
    steps[[step_names[i]]] <<- list(func=st,output=NULL,log=NULL,params=formals(st))
    i <- i + 1

  }
}

Flow$methods(initialize=init)
fl=Flow$new(steps=list("linMod"=lm,"Gen"=glm))

paramToList=function(plist,stepname){
  nm = names(plist)
  #for (n in nm){
  #  if (as.character(plist[[n]])=="" || is.null(plist[[n]]))
  #    plist[[n]]=""

  #}
  if (!is.null(plist[["data"]]))
      plist[["data"]]=NULL
  params = as.character(plist)
  param_df=data.frame(Step=stepname,Param=names(plist),values=params,stringAsFactors=F)
  param_df

}

saveParms=function(filename){

  write.csv(p,filename,row.names = F)
}


eval(parse(text="list(a=1)"))
