#' @title Apply a filter to a data.frame
#'
#' @param df data.frame
#'
#' @export

ggDensity= function(df= mtcars, filter.var= 'cyl', filter.val= 6){

  Filter= paste0(filter.var, "==", filter.val)
  print(Filter)
  df2=
  df %>%
    filter(Filter)

  ggplot(df2, aes(x= raquate))
}
densityPlot <- function(x,y){
  attach(x)
  ggplot(x)+
    geom_density(aes(y))
}


densityPlot(wine, points)

SamplingDist <- function(samples = 50, sample_size = 100, mean =0, sd =1){
  if(samples <= 0){
    stop("The number of samples needs to be greater than zero")
  }
  if (sd <0){
    stop("You cannot have a negative standard deviation ")
  }
  sampleMeans <- rep(NA, samples)
  for (i in 1:samples){
    x<- rnorm(sample_size, mean = mean, sd= sd)
    sampleMeans[i] = mean(x)
  }
  return(sampleMeans)
}



