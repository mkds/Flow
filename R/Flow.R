

#workflow1 <- Flow$new(steps=list("getdata"=get_data,"cleanup"=clean_up,
#                                 "getFeatures"=get_feature,"model"=lm,
#                                 "modelAnalysis"=analyze_model,"predict"=predict,
#                                 "modelPerformance"=evaluate_prediction))
Flow <- setRefClass("Flow",fields = list(steps="list"))


init <- function(steps){
  i <- 1
  step_names=names(steps)
  for (st in steps) {
    steps[[step_names[i]]] <<- list(funcs=st,output=NULL,stats=NULL,params=formals(st@run))
    i <- i + 1

  }
}

Flow$methods(initialize=init)



prn=function(){
  cat("Start")
  for (st in names(steps))
    cat(" ----> ",st)
  cat(" ----> ","End")
}

Flow$methods(show=prn)

#fl=Flow$new(steps=list(lr=lr,glr=glr))



paramToList=function(plist,stepname){
  nm = names(plist)
  if (!is.null(plist[["data"]]))
    plist[["data"]]=NULL
  params = as.character(plist)
  param_df=data.frame(Step=stepname,Param=names(plist),values=params,stringsAsFactors=F)
  param_df

}

all_params <- function(){
  all_steps <- names(steps)
  params=NULL
  for(st in all_steps){
    params <- rbind(params,paramToList(steps[[st]]$params,st))
  }
  return(params)
}

Flow$methods(all_params=all_params)



save_params=function(filename){

  write.csv(.self$all_params(),filename,row.names = F)
}

Flow$methods(save_params=save_params)


#eval(parse(text="list(a=1)"))
