# Median liking ratings pre- and post-training
fun_ratings2 <- function(x) {
  c(Pre.Liking.Go = median(x$values.rating[x$blockcode=="pre_ratings" & (x$food_category=="go_sweet"|x$food_category=="go_savoury")]),
    Pre.Liking.Nogo = median(x$values.rating[x$blockcode=="pre_ratings" & (x$food_category=="nogo_sweet"|x$food_category=="nogo_savoury")]),
    Pre.Liking.Filler = median(x$values.rating[x$blockcode=="pre_ratings" & (x$food_category=="filler_sweet"|x$food_category=="filler_savoury")]),
    Post.Liking.Go = median(x$values.rating[x$blockcode=="post_ratings" & (x$food_category=="go_sweet"|x$food_category=="go_savoury")]),
    Post.Liking.Nogo = median(x$values.rating[x$blockcode=="post_ratings" & (x$food_category=="nogo_sweet"|x$food_category=="nogo_savoury")]),
    Post.Liking.Filler = median(x$values.rating[x$blockcode=="post_ratings" & (x$food_category=="filler_sweet"|x$food_category=="filler_savoury")])
  )
}


item_ratings <- function(x) {
c(Go_1a = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="go_sweet" & x$exemplar=="1"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="go_sweet" & x$exemplar=="1"],
  Go_1b = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="go_sweet" & x$exemplar=="2"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="go_sweet" & x$exemplar=="2"],
  Go_1c = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="go_sweet" & x$exemplar=="3"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="go_sweet" & x$exemplar=="3"],
  Go_2a = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="go_savoury" & x$exemplar=="1"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="go_savoury" & x$exemplar=="1"],
  Go_2b = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="go_savoury" & x$exemplar=="2"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="go_savoury" & x$exemplar=="2"],
  Go_2c = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="go_savoury" & x$exemplar=="3"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="go_savoury" & x$exemplar=="3"],
  
  NoGo_1a = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="nogo_sweet" & x$exemplar=="1"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="nogo_sweet" & x$exemplar=="1"],
  NoGo_1b = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="nogo_sweet" & x$exemplar=="2"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="nogo_sweet" & x$exemplar=="2"],
  NoGo_1c = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="nogo_sweet" & x$exemplar=="3"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="nogo_sweet" & x$exemplar=="3"],
  NoGo_2a = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="nogo_savoury" & x$exemplar=="1"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="nogo_savoury" & x$exemplar=="1"],
  NoGo_2b = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="nogo_savoury" & x$exemplar=="2"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="nogo_savoury" & x$exemplar=="2"],
  NoGo_2c = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="nogo_savoury" & x$exemplar=="3"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="nogo_savoury" & x$exemplar=="3"],
  
  Ctrl_1a = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="filler_sweet" & x$exemplar=="1"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="filler_sweet" & x$exemplar=="1"],
  Ctrl_1b = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="filler_sweet" & x$exemplar=="2"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="filler_sweet" & x$exemplar=="2"],
  Ctrl_1c = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="filler_sweet" & x$exemplar=="3"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="filler_sweet" & x$exemplar=="3"],
  Ctrl_2a = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="filler_savoury" & x$exemplar=="1"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="filler_savoury" & x$exemplar=="1"],
  Ctrl_2b = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="filler_savoury" & x$exemplar=="2"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="filler_savoury" & x$exemplar=="2"],
  Ctrl_2c = x$values.rating[x$blockcode=="post_ratings" & x$food_category=="filler_savoury" & x$exemplar=="3"] - x$values.rating[x$blockcode=="pre_ratings" & x$food_category=="filler_savoury" & x$exemplar=="3"])
}

# Trimmed mean liking ratings pre- and post-training
fun_ratings_trimmed <- function(x) {
  c(Pre.Liking.Go = mean(x$values.rating[x$blockcode=="pre_ratings" & (x$food_category=="go_sweet"|x$food_category=="go_savoury")], trim=0.2),
    Pre.Liking.Nogo = mean(x$values.rating[x$blockcode=="pre_ratings" & (x$food_category=="nogo_sweet"|x$food_category=="nogo_savoury")], trim=0.2),
    Pre.Liking.Filler = mean(x$values.rating[x$blockcode=="pre_ratings" & (x$food_category=="filler_sweet"|x$food_category=="filler_savoury")], trim=0.2),
    Post.Liking.Go = mean(x$values.rating[x$blockcode=="post_ratings" & (x$food_category=="go_sweet"|x$food_category=="go_savoury")], trim=0.2),
    Post.Liking.Nogo = mean(x$values.rating[x$blockcode=="post_ratings" & (x$food_category=="nogo_sweet"|x$food_category=="nogo_savoury")], trim=0.2),
    Post.Liking.Filler = mean(x$values.rating[x$blockcode=="post_ratings" & (x$food_category=="filler_sweet"|x$food_category=="filler_savoury")], trim=0.2)
  )
}

# Functions for reaction time (RT) and error rate (PE) outcomes per block

fun_gng_RTs_blocks <- function (x) {
  c(FillerGo.RT.B1= mean(x$values.gng_rt[(x$values.gng_trial_type=="filler_go_left" | x$values.gng_trial_type=="filler_go_right") & x$block=="1"]), 
    FillerGo.RT.B2= mean(x$values.gng_rt[(x$values.gng_trial_type=="filler_go_left" | x$values.gng_trial_type=="filler_go_right") & x$block=="2"]), 
    FillerGo.RT.B3= mean(x$values.gng_rt[(x$values.gng_trial_type=="filler_go_left" | x$values.gng_trial_type=="filler_go_right") & x$block=="3"]), 
    FillerGo.RT.B4= mean(x$values.gng_rt[(x$values.gng_trial_type=="filler_go_left" | x$values.gng_trial_type=="filler_go_right") & x$block=="4"]), 
    FillerGo.RT.B5= mean(x$values.gng_rt[(x$values.gng_trial_type=="filler_go_left" | x$values.gng_trial_type=="filler_go_right") & x$block=="5"]), 
    FillerGo.RT.B6= mean(x$values.gng_rt[(x$values.gng_trial_type=="filler_go_left" | x$values.gng_trial_type=="filler_go_right") & x$block=="6"]), 
    
    Go.RT.B1= mean(x$values.gng_rt[(x$values.gng_trial_type=="go_left" | x$values.gng_trial_type=="go_right") & x$block=="1"]),
    Go.RT.B2= mean(x$values.gng_rt[(x$values.gng_trial_type=="go_left" | x$values.gng_trial_type=="go_right") & x$block=="2"]),
    Go.RT.B3= mean(x$values.gng_rt[(x$values.gng_trial_type=="go_left" | x$values.gng_trial_type=="go_right") & x$block=="3"]),
    Go.RT.B4= mean(x$values.gng_rt[(x$values.gng_trial_type=="go_left" | x$values.gng_trial_type=="go_right") & x$block=="4"]),
    Go.RT.B5= mean(x$values.gng_rt[(x$values.gng_trial_type=="go_left" | x$values.gng_trial_type=="go_right") & x$block=="5"]),
    Go.RT.B6= mean(x$values.gng_rt[(x$values.gng_trial_type=="go_left" | x$values.gng_trial_type=="go_right") & x$block=="6"])
    )
}

fun_gng_PEs_blocks <- function (x) {
  c( 
    FillerNoGo.PE.H1= mean(x$accuracy[(x$values.gng_trial_type=="filler_nogo_left" | x$values.gng_trial_type=="filler_nogo_right") & (x$block==1|x$block==2|x$block==3)]),
    NoGo.PE.H1= mean(x$accuracy[(x$values.gng_trial_type=="nogo_left" | x$values.gng_trial_type=="nogo_right") & (x$block==1|x$block==2|x$block==3)]),
    FillerNoGo.PE.H2= mean(x$accuracy[(x$values.gng_trial_type=="filler_nogo_left" | x$values.gng_trial_type=="filler_nogo_right") & (x$block==4|x$block==5|x$block==6)]),
    NoGo.PE.H2= mean(x$accuracy[(x$values.gng_trial_type=="nogo_left" | x$values.gng_trial_type=="nogo_right") & (x$block==4|x$block==5|x$block==6)]))
}

#Function 5: AAT initiation time medians for different conditions

fun_AAT_init <- function (x) {
  c(Pre.Pull.Filler.in= median(x$init_time[x$condition== "pre_pull_filler"]), 
    Pre.Push.Filler.in= median(x$init_time[x$condition== "pre_push_filler"]),
    Pre.Pull.Go.in= median(x$init_time[x$condition== "pre_pull_go"]),
    Pre.Push.Go.in= median(x$init_time[x$condition== "pre_push_go"]),
    Pre.Pull.NoGo.in= median(x$init_time[x$condition== "pre_pull_nogo"]),
    Pre.Push.NoGo.in= median(x$init_time[x$condition== "pre_push_nogo"]),
    
    Post.Pull.Filler.in= median(x$init_time[x$condition== "post_pull_filler"]), 
    Post.Push.Filler.in= median(x$init_time[x$condition== "post_push_filler"]),
    Post.Pull.Go.in= median(x$init_time[x$condition== "post_pull_go"]),
    Post.Push.Go.in= median(x$init_time[x$condition== "post_push_go"]),
    Post.Pull.NoGo.in= median(x$init_time[x$condition== "post_pull_nogo"]),
    Post.Push.NoGo.in= median(x$init_time[x$condition== "post_push_nogo"]),
    
    Pre.Pull.in= median(x$init_time[x$condition== "pre_pull_filler"|x$condition== "pre_pull_go"|x$condition== "pre_pull_nogo"]),
    Pre.Push.in= median(x$init_time[x$condition== "pre_push_filler"|x$condition== "pre_push_go"|x$condition== "pre_push_nogo"]),
    Post.Pull.in=  median(x$init_time[x$condition== "post_pull_filler"|x$condition== "post_pull_go"|x$condition== "post_pull_nogo"]),
    Post.Push.in= median(x$init_time[x$condition== "post_push_filler"|x$condition== "post_push_go"|x$condition== "post_push_nogo"]))
}
