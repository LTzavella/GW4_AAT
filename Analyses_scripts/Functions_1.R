######################################################FUNCTIONS FOR DATA PRE-PROCESSING AND AVERAGING######################################################

#Function 1: linear interpolation of mouse coordinates and reaction times
interp <- function(i) {
  min_t = min(i$rt)
  max_t = max(i$rt)
  i$t = i$rt - min_t
  
  res1 <- approx(i$t, i$mousey, n=100, method="linear")
  res2 <- approx(i$t, i$mousex, n=100, method="linear")
  return(data.frame(rt=res1$x, y=res1$y, x=res2$y)) 
}

#Function 2: replicate rows for new dataframe
repr<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

#Function 3: AAT reaction time medians for different conditions

fun_AAT_RTs <- function (x) {
  c(Pre.Pull.Filler.RT= median(x$total_rt[x$condition== "pre_pull_filler"]), 
    Pre.Push.Filler.RT= median(x$total_rt[x$condition== "pre_push_filler"]),
    Pre.Pull.Go.RT= median(x$total_rt[x$condition== "pre_pull_go"]),
    Pre.Push.Go.RT= median(x$total_rt[x$condition== "pre_push_go"]),
    Pre.Pull.NoGo.RT= median(x$total_rt[x$condition== "pre_pull_nogo"]),
    Pre.Push.NoGo.RT= median(x$total_rt[x$condition== "pre_push_nogo"]),
    
    Post.Pull.Filler.RT= median(x$total_rt[x$condition== "post_pull_filler"]), 
    Post.Push.Filler.RT= median(x$total_rt[x$condition== "post_push_filler"]),
    Post.Pull.Go.RT= median(x$total_rt[x$condition== "post_pull_go"]),
    Post.Push.Go.RT= median(x$total_rt[x$condition== "post_push_go"]),
    Post.Pull.NoGo.RT= median(x$total_rt[x$condition== "post_pull_nogo"]),
    Post.Push.NoGo.RT= median(x$total_rt[x$condition== "post_push_nogo"]),
    
    Pre.Pull.RT= median(x$total_rt[x$condition== "pre_pull_filler"|x$condition== "pre_pull_go"|x$condition== "pre_pull_nogo"]),
    Pre.Push.RT= median(x$total_rt[x$condition== "pre_push_filler"|x$condition== "pre_push_go"|x$condition== "pre_push_nogo"]),
    Post.Pull.RT=  median(x$total_rt[x$condition== "post_pull_filler"|x$condition== "post_pull_go"|x$condition== "post_pull_nogo"]),
    Post.Push.RT= median(x$total_rt[x$condition== "post_push_filler"|x$condition== "post_push_go"|x$condition== "post_push_nogo"]))
}

#Function 4: AAT mean error rates for different conditions 

fun_AAT_ERs <- function (x) {
  c(Pre.Pull.Filler.ER= mean(x$accuracy[x$condition== "pre_pull_filler"]), 
    Pre.Push.Filler.ER= mean(x$accuracy[x$condition== "pre_push_filler"]),
    Pre.Pull.Go.ER= mean(x$accuracy[x$condition== "pre_pull_go"]),
    Pre.Push.Go.ER= mean(x$accuracy[x$condition== "pre_push_go"]),
    Pre.Pull.NoGo.ER= mean(x$accuracy[x$condition== "pre_pull_nogo"]),
    Pre.Push.NoGo.ER= mean(x$accuracy[x$condition== "pre_push_nogo"]),
    
    Post.Pull.Filler.ER= mean(x$accuracy[x$condition== "post_pull_filler"]), 
    Post.Push.Filler.ER= mean(x$accuracy[x$condition== "post_push_filler"]),
    Post.Pull.Go.ER= mean(x$accuracy[x$condition== "post_pull_go"]),
    Post.Push.Go.ER= mean(x$accuracy[x$condition== "post_push_go"]),
    Post.Pull.NoGo.ER= mean(x$accuracy[x$condition== "post_pull_nogo"]),
    Post.Push.NoGo.ER= mean(x$accuracy[x$condition== "post_push_nogo"]),
    
    Pre.Pull.ER= mean(x$accuracy[x$condition== "pre_pull_filler"|x$condition== "pre_pull_go"|x$condition== "pre_pull_nogo"]),
    Pre.Push.ER= mean(x$accuracy[x$condition== "pre_push_filler"|x$condition== "pre_push_go"|x$condition== "pre_push_nogo"]),
    Post.Pull.ER= mean(x$accuracy[x$condition== "post_pull_filler"|x$condition== "post_pull_go"|x$condition== "post_pull_nogo"]),
    Post.Push.ER= mean(x$accuracy[x$condition== "post_push_filler"|x$condition== "post_push_go"|x$condition== "post_push_nogo"]),
    Pre.ER = mean(x$accuracy[x$condition=="pre_pull_filler"|x$condition=="pre_push_filler"|x$condition=="pre_pull_go"|x$condition=="pre_push_go"|
                             x$condition=="pre_pull_nogo"|x$condition=="pre_push_nogo"]),
    Post.ER = mean(x$accuracy[x$condition=="post_pull_filler"|x$condition=="post_push_filler"|x$condition=="post_pull_go"|x$condition=="post_push_go"|
                              x$condition=="post_pull_nogo"|x$condition=="post_push_nogo"]))
}


#Function 5: Mean liking ratings pre- and post-training
fun_ratings <- function(x) {
  c(Pre.Liking.Go = mean(x$values.rating[x$blockcode=="pre_ratings" & (x$food_category=="go_sweet"|x$food_category=="go_savoury")]),
    Pre.Liking.Nogo = mean(x$values.rating[x$blockcode=="pre_ratings" & (x$food_category=="nogo_sweet"|x$food_category=="nogo_savoury")]),
    Pre.Liking.Filler = mean(x$values.rating[x$blockcode=="pre_ratings" & (x$food_category=="filler_sweet"|x$food_category=="filler_savoury")]),
    Post.Liking.Go = mean(x$values.rating[x$blockcode=="post_ratings" & (x$food_category=="go_sweet"|x$food_category=="go_savoury")]),
    Post.Liking.Nogo = mean(x$values.rating[x$blockcode=="post_ratings" & (x$food_category=="nogo_sweet"|x$food_category=="nogo_savoury")]),
    Post.Liking.Filler = mean(x$values.rating[x$blockcode=="post_ratings" & (x$food_category=="filler_sweet"|x$food_category=="filler_savoury")])
  )
}

# Functions for reaction time (RT) and error rate (PE) outcomes

fun_gng_RTs <- function (x) {
  c(FillerGo.RT= mean(x$values.gng_rt[(x$values.gng_trial_type=="filler_go_left" | x$values.gng_trial_type=="filler_go_right")]), 
    Go.RT= mean(x$values.gng_rt[(x$values.gng_trial_type=="go_left" | x$values.gng_trial_type=="go_right")]))
}

fun_gng_PEs <- function (x) {
  c(FillerGo.PE= mean(x$accuracy[(x$values.gng_trial_type=="filler_go_left" | x$values.gng_trial_type=="filler_go_right")]), 
    FillerNoGo.PE= mean(x$accuracy[(x$values.gng_trial_type=="filler_nogo_left" | x$values.gng_trial_type=="filler_nogo_right")]),
    Go.PE= mean(x$accuracy[(x$values.gng_trial_type=="go_left" | x$values.gng_trial_type=="go_right")]),
    NoGo.PE= mean(x$accuracy[(x$values.gng_trial_type=="nogo_left" | x$values.gng_trial_type=="nogo_right")]))
}


