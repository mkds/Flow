
Flow <- setRefClass("Flow",fields = list(steps="list"))


Flow$methods(initialize = function(steps){
  for (st in steps) {
    steps[[st$step_name]] <<- st
  }
})



Flow$methods(show=function(){
  cat("Start")
  for (st in names(steps))
    cat(" ----> ",st)
  cat(" ----> ","End")
})



#fl=Flow$new(steps=list(lr=lr,glr=glr))

Flow$methods(get_all_params=function(){
  all_param_df=NULL
  for (st in steps){
    params_list <- st$params
    if (!is.null(params_list[["data"]]))
      params_list[["data"]]=NULL
    if (length(params_list > 0)){
      params_chars = as.character(params_list)
      params_df=data.frame(Step=st$step_name, Parameter=names(params_list),
                           values=params_chars, stringsAsFactors=F)
      all_param_df=rbind(all_param_df,params_df)
    }
  }
  return(all_param_df)
})


Flow$methods(save_all_params = function(file_name){
  all_param_df=.self$get_all_params()
  write.csv(all_params_df,file_name,row.names = F)
})


#eval(parse(text="list(a=1)"))
Flow$methods(run_all=function(){
  #Run all steps
  for (i in (1:length(steps))){
    if (i > 1){
      steps[[names(steps)[i]]]$params$data <<- steps[[names(steps)[i-1]]]$output$result
    }
    steps[[names(steps)[i]]]$run()
  }

})

