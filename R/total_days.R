total_days = function(x) {
  return(max(pcm_file[which(pcm_file$profile_key==x),"SURVEY_DATE"]) - min(pcm_file[which(pcm_file$profile_key==x),"SURVEY_DATE"])+1)
}

pcm_days = sapply(profile_key,total_days)
mean(pcm_days)/365
