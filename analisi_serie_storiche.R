calculate_stl <- function(dataframe){
  seq_date <- seq(as.Date("2001/01/01"), as.Date("2022/12/01"), by="month")
  time_serie <- ts(dataframe, start=c(2001,1), end=c(2022,12), frequency = 12)
  stl_data <- stl(time_serie, s.window = "periodic")
  
  df <- as.data.frame(stl_data$time.series[,1:3])
  df$date <- seq_date
  return(df)
}

calculate_iqr <- function(dataframe_stl, iqr_dato){
  df_iqr <- data.frame(
    iqr = c(IQR(dataframe_stl$trend)*100/iqr_dato,
            IQR(dataframe_stl$seasonal)*100/iqr_dato,
            IQR(dataframe_stl$remainder)*100/iqr_dato),
    name = c("Trend", "Stagionalità", "Residuo")
  )
  return(df_iqr)
}

plot_stl <- function(stl_dataframe, name, to_save=FALSE, filename){
  plot_trend <- ggplot(stl_dataframe, aes(x=date, y=trend)) +
    geom_line() +
    labs(x="Data", y=name)
  print(plot_trend)
  
  plot_seasonal <- ggplot(stl_dataframe, aes(x=date, y=seasonal)) +
    geom_line() +
    labs(x="Data", y=name)
  print(plot_seasonal)
  
  plot_remainder <- ggplot(stl_dataframe, aes(x=date, y=remainder)) +
    geom_line() +
    labs(x="Data", y=name)
  print(plot_remainder)
  if(to_save){
    save_plot(plot_trend, paste(filename, "trend", sep = "_"))
    save_plot(plot_seasonal, paste(filename, "seasonal", sep = "_"))
    save_plot(plot_remainder, paste(filename, "remainder", sep = "_"))
  }
}