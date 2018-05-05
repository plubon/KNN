source('knn.R')
files <- list.files('data', include.dirs = FALSE)
metrics <- c(1,2,Inf)
for(file in files)
{
  tryCatch({
  dir_name <- tools::file_path_sans_ext(file)
  dir.create(file.path('data', dir_name))
  print(file.path('data', file))
  M_df <- as.data.frame(read.csv(file.path('data', file)))
  M <- as.matrix(M_df)
  M_scaled <- M
  M_scaled[,2:ncol(M)] <- scale(M_scaled[,2:ncol(M)]) 
  for(metric in metrics)
  {
    knn_result <- five_fold_knn(M, 19, metric)
    write.csv(knn_result, file=file.path('data', dir_name,paste(c('knn_',metric,'.csv'),collapse='')))
    knn_result_scaled <- five_fold_knn(M_scaled, 19, metric)
    write.csv(knn_result_scaled, file=file.path('data', dir_name, paste(c('knn_scaled_',metric,'.csv'),collapse='')))
  }
  #polr_result <- five_fold_formula(logistic_regression, M_df)
  #write.csv(polr_result, file=file.path('data', dir_name, paste(c('polr.csv'),collapse='')))
  rf_result <- five_fold(random_forest, M)
  write.csv(rf_result, file=file.path('data', dir_name, paste(c('rf.csv'),collapse='')))
  nb_result <- five_fold_formula(naive_bayes, M_df)
  write.csv(nb_result, file=file.path('data', dir_name, paste(c('nb.csv'),collapse='')))
  })
}