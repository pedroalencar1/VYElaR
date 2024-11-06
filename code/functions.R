"
Re-creating Vyelas

This script reproduces with good level of accuracy, the outputs from vyelas
The input structure is the same as of the original program


Pedro Alencar
30.10.23
"


# 0. import libraries -----------------------------------------------------
library(dplyr)
library(tidyr)
library(progress)


# 1. define Vyelas functions ----------------------------------------------

#' Auxiliar function to disply progress bar
#'
#' @param total_it number of iteracions, provided in the input file
#' @param width_bar width in pt. of the displayed
easy_progress_bar <- function(total_it, width_bar = 120){
  
  pb <- progress::progress_bar$new(format = "(:spin) [:bar] :current/:total [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                   total = total_it,
                                   complete = "=",   # Completion bar character
                                   incomplete = "-", # Incomplete bar character
                                   current = ">",    # Current bar character
                                   clear = FALSE,    # If TRUE, clears the bar when finish
                                   width = width_bar)
  
  return(pb)
}

#' read input file and creates list of inputs
#'
#' @param path_to_file path to input file
read_data <- function(path_to_file){
  # path_to_file = "docs/Vyelas3/cb2000.txt"
  
  data <- read.delim(path_to_file, header = FALSE) |>
    dplyr::select(V1)
  
  name <- data[1,] #get name
  data <- data[-1,] #get only inputs
  
  inputs <- data.frame(vars = c("Qam", "CV", "alpha", "Evap", "Vmax", 
                                  "Vmin","V00", "nQr", "Qrmin", "Qrmax", "n0"),
                         val = as.numeric(data)) |>
    pivot_wider(names_from = "vars", values_from = "val") |>
    as.list()
  
  inputs[["name"]] <- name
  
  return(inputs)
}

#' function to get gamma distribution as df
#' deprecated
#'
#' @param Qam  Inflow mean
#' @param CV Inflow coef. of variance
#' @param n_steps number of subdivisions of assessed distribution
gamma_dist <- function(Qam, CV, n_steps = 1000){
  
  # n_steps = 1000
  
  shape_ <- (1/CV)^2
  scale_ <- Qam/shape_
  
  dX = Qam*(1+4*CV)/n_steps
  
  X <- seq(0, dX*(nsteps - 1), dX)
  
  df <- data.frame(x = X, px = pgamma(X, shape = shape_, scale = scale_))
  
  return(df)
}

#' function to calculate random annual inflow
#'
#' @param Qam  Inflow mean
#' @param CV Inflow coef. of variance
calc_qa <- function(Qam, CV){
  
  shape_ <- (1/CV)^2
  scale_ <- Qam/shape_
  
  rand_seed <- runif(1, min = 0.00001, max = 0.99999)
  
  qa <- qgamma(rand_seed, shape = shape_, scale = scale_)
  
  return(qa)
  # plot(x = rand_seed, y = qgamma(rand_seed, shape = shape_, scale = scale_))
  
}

#' function to calculate new volume 
#'
#' @param V2 volume before evaporation
#' @param hi depth before evaporation
#' @param Qr outflow mean
#' @param inputs vector output of `read_data`
calc_new_vol <- function(V2, hi, Qr, inputs){

  B = (hi^3) - 1.5*inputs$Evap*(hi^2) - Qr*1000000/inputs$alpha
  B = max(0.00001,B)
  h_new = hi-Qr/(6*inputs$alpha*(hi^2))
  
  j = 0
  err = 1000
  tol = 0.001
  while(err > tol){
    j = j+1
    h_new_i <- h_new
    
    # h_new <- h_new_i - (h_new_i^3 + 1.5*h_new_i^2 - B)/(1.5*h_new_i^2 + 3*inputs$Evap*h_new_i)
    h_new <- (B/(h_new_i+1.5*inputs$Evap))^0.5
    err <- abs(h_new - h_new_i)/h_new
    
    if (j > 2000){
      cat(paste0('No convergence after 2000 trials (err = ',err,')'))
      err = tol/2
    }
  }
  V4 <- (inputs$alpha*h_new^3)/1000000
  return(V4)
}


#' function to calculate evaporation loss as discharge
#'
#' @param V2 Volume before evaporation
#' @param Qr0 simulated outflow mean
#' @param inputs vector output of `read_data`
calc_qev <- function(V2, Qr0, inputs){
  
  hi <- (V2*1000000/inputs$alpha)^(1/3) # water depth before evaporation
  doubt <- TRUE # check if criteria satisfies
  
  if (V2 <= inputs$Vmin){ #initial volume below critical (no success)
    hi <- hi - inputs$Evap
    
    if (hi <= 0){ #check empying
      Qr <- 0
      Qe <- V2
      V4 <- 0
    } else {
      V3 <- (inputs$alpha*hi^3)/1000000
      Qe <- V2 - V3
      Qr <- min(Qr0/2, V3)
      V4 <- V2-Qe-Qr
    }
    doubt <- FALSE
  }
  
  Vaux <- inputs$Vmin + 3*inputs$Evap*inputs$alpha*hi/1000000 + Qr0
  if (V2 >= Vaux){ #above critical volume (success) 
    Qr <- Qr0
    V4 <- calc_new_vol(V2, hi, Qr, inputs)
    Qe <- V2-Qr-V4
    doubt <- FALSE
  }
  
  if(doubt) {#initial volume in "gray-zone" (doubt)
    Qr <- Qr0
    V4 <- calc_new_vol(V2, hi, Qr, inputs)
    Qe <- V2-Qr-V4
    
    if (V4 >= inputs$Vmin){
      doubt <- FALSE
    } else {
      Qr <- Qr0/2
      V4 <- calc_new_vol(V2, hi, Qr, inputs)
      Qe <- V2-Qr-V4
      
      if (V4 >= 0){
        doubt <- FALSE
      } else {
        Qr <- Qr/2
        V4 <- calc_new_vol(V2, hi, Qr, inputs)
        Qe <- V2-Qr-V4
        
        if (V4 < 0){
          V4 <- 0
          Qr <- V2/2
          Qe <- V2/2
        }
      }
    }
  }
  
  output <- c(V4, Qe, Qr)
  return(output)
}

#' function to calculate guaranty
#'
#' @param Qr0 simulated outflow mean
#' @param inputs vector output of `read_data`
calc_garant <- function(Qr0, inputs){
  
  V4 <- inputs$V00 # initialize loop
  Qsm = 0
  Qem = 0
  Qrm = 0
  
  count <- 0
  for (i in 1:inputs$n0){
    # i = 138
    V0 <- V4
    Qa <- calc_qa(inputs$Qam, inputs$CV) # call function to calc mean inflow
    
    V1 <- V0 + Qa
    Qs <- max(0, V1-inputs$Vmax) #spill discharge
    Qsm <- Qsm + Qs #accumulate spill
    
    V2 <- V1 - Qs # volume after spill
    
    evap_out <- calc_qev(V2, Qr0, inputs)
    V4 <- evap_out[1] #volume after evap
    Qem <- Qem + evap_out[2] 
    Qrm <- Qrm + evap_out[3] 
    
    if (evap_out[3] >= Qr0){
      count <- count + 1
    } else {
      count <- count
    }
    # cat(paste0(i, "-"))
  }
  
  garanty <- 100*count/inputs$n0
  Qsm <- Qsm/inputs$n0
  Qem <- Qem/inputs$n0
  Qrm <- Qrm/inputs$n0
  
  output <- c(Qr0,garanty, Qrm, Qem, Qsm)
}


#' Main function
#'
#' @param path_to_file path to input file
run_vyelas <- function(path_to_file){
  # path_to_file = "docs/Vyelas3/cb1944.txt"
  inputs <- read_data(path_to_file)
  
  dQr <- (inputs$Qrmax - inputs$Qrmin)/inputs$nQr
  
  df <- data.frame(matrix(ncol = 5, nrow = 0))

  pb <- easy_progress_bar(inputs$nQr+1)
  for (i in 0:inputs$nQr){
    pb$tick()
    # i = 3
    Qr0 <- inputs$Qrmin + i*dQr
    result_garanty <- calc_garant(Qr0, inputs)
    
    df <- rbind(df, result_garanty)
    
    # cat(paste0(i, "-"))
  }
  
  colnames(df) <- c("Qr", "G", "Qrm", "Qem", "Qsm")
  
  return(df)
  
}


#2.  run example -------------------------------------------------------------


df <- run_vyelas("docs/Vyelas3/cb1944.txt")
View(df)

