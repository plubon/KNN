source('knn.R')

get_clean_file_name <- function(file)
{
  return(tools::file_path_sans_ext(gsub('_', ' ', file)))
}

get_comparison_dt  <- function(dir)
{
  setwd('/home/piotr/Uczelnia/PdPYR/PD3/')
  files <- list.files(file.path('results',dir))
  files <- files[files!='labels.csv']
  labels <- read.csv(file.path('results', dir, 'labels.csv'))
  labels <- labels[,2:ncol(labels), drop=FALSE]
  ret <- data.frame(matrix(ncol=3, nrow=0))
  colnames(ret) <- c('ERR', 'MAD', 'MSE')
  for (file in files) {
    data <- read.csv(file.path('results',dir,file))
    data <- data[,2:ncol(data), drop=FALSE]
    if(ncol(data)> 1)
    {
      ks <- c(3, 5, 7, 9, 15, 19)
      for(k_i in sample(length(ks),2))
      {
        fun_names <- c('moda', 'srednia_a', 'mediana', 'minkara1.5', 'minkara3.0', 'srednia_wazona')
        funs <- list(moda, srednia_a, mediana, minkara1.5, minkara3.0, srednia_wazona)
        idx <- 1
        for(fun_i in sample(length(funs),2))
        {
          k <- ks[k_i]
          fun <- funs[fun_i]
          res <- data[,1:k, drop=FALSE]
          res <- funs[[fun_i]](res)
          err <- err(as.vector(res), as.vector(labels))
          mad <- blad_bezwzgeldny(as.vector(res), as.vector(labels))
          mse <- mse(as.vector(res), as.vector(labels))
          row_name <- paste(get_clean_file_name(file), k, fun_names[fun_i])
          to_add <- data.frame(err, mad, mse, row.names = c(row_name))
          colnames(to_add) <- colnames(ret)
          ret <- rbind(ret, to_add)    
          idx <- idx + 1
        }
      }
    }
    else
    {
      err <- err(as.vector(data), as.vector(labels))
      mad <- blad_bezwzgeldny(as.vector(data), as.vector(labels))
      mse <- mse(as.vector(data), as.vector(labels))
      to_add <- data.frame(err, mad, mse, row.names = c(get_clean_file_name(file)))
      colnames(to_add) <- colnames(ret)
      ret <- rbind(ret, to_add)
    }
  }
  return(t(ret))
}