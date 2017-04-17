Step <- setRefClass("Step",fields = list(step_name="character",
                                         main="function",
                                         stats_fn="function",
                                         doc_str_fn="function",
                                         prn_stats_fn="function",
                                         #input="list"
                                         output="list",
                                         params="list"))


NA_func=function() {NA}

Step$methods(initialize = function(step_name,main,stats_fn=NA_func, doc_str_fn=NA_func,
                                   prn_stats_fn=NA_func, #input = list(),
                                   ...){

    step_name <<- step_name
    main <<- main
    params <<- as.list(formals(main))
    n_params = length(params)
    if (n_params > 0)
      if (names(params[n_params]) == "...")
        params <<- params[-n_params]
    #input <<- input
    output <<- list()
    stats_fn  <<- stats_fn
    doc_str_fn <<- doc_str_fn
    prn_stats_fn <<- prn_stats_fn
    chkDots(...)

})

Step$methods(run=function(){
  output$result <<- do.call(main,args = params)
})

Step$methods(get_stats=function(){
  if (!is.null(output$result)){
    output$stats <<- do.call(stats_fn,args = list(input=input,output=output$result))
  } else {
    stop("Step must be run in order to get status")
  }
})

Step$methods(print_stats=function(){
  if (!is.null(output$result)){
    if (is.null(output$stats)) .self$get_stats()
    do.call(prn_stats_fn,args = list(input=input, output=output$results,
                              stats=output$stats))
  } else {
    stop("Step must be run in order to get status")
  }
})

Step$methods(print_documentation=function(){
  do.call(doc_str_fn,list(params=params))
})


Step$methods(save_params = function(file_name){
  params_list <- params
  if (!is.null(params_list[["data"]]))
    params_list[["data"]]=NULL
  if (length(parms_list) > 0){
    params_chars = as.character(params_list)
    params_df=data.frame(Step=step_name, Parameter=names(params_list),
                         values=params_chars, stringsAsFactors=F)
    write.csv(params_df,file_name,row.names = F)
  }

})

ModelStep <- setRefClass("ModelStep",contains=c("Step"))

