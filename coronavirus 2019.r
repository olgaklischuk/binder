#install data from github repo /RamiKrispin/coronavirus
install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")
#install coronavirus library data bulk package
library(coronavirus)
update_dataset()

#data
covid19_df <- refresh_coronavirus_jhu()
head(covid19_df)

#data.frame
covid19_df<-data.frame(covid19_df)

#plotting packages
library(ggplot2)
library(dplyr)
library(GGally)

#plots
advanced_countries<-unique(covid19_df[,"contries"])
covid19_advanced_co_df<-covid19_df%>%
filter(country=advanced_countries))
plot<-ggplot(data=covid19_df, aes(date,cases))
