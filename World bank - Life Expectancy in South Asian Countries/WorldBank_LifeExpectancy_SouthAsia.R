# Import Datasets
dfLE <- read.csv("Life_Expectancy.csv")
dfHC <- read.csv("health_care_per_capita.csv")
library(data.table)

#Source - https://data.worldbank.org/indicator/SH.XPD.CHEX.PC.CD
#Source - https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=CN-IN-PK-AF-BD-BT-NP


#lines 10 - 12 subset dfLE to obtain the Life Expectancy for the given country
#lines 13 - 15 subset the dfHC to obtain the Health Expenditure / capita for the given country
#lines 17 - 18 merge the BGD.LE and BGD.HC dataset into one based on the Year and filter out unnecessary columns
#line 19 removes BGD.LE and BGD.HC subset dataframes as they are unnecessary
#this is repeated for all 4 countries: Bangladesh, India, Sri Lanka, Pakistan

#Bangladesh datacleansing and creating dataframe
BGD.LE <- as.numeric(transpose(dfLE[dfLE$Country.Code == "BGD",])[5:62,])
BGD.LE <- data.frame("Country" = "Bangladesh", "Year" = c(1960:2017), "LE" = BGD.LE)
BGD.LE <- BGD.LE[BGD.LE$Year >= 2000 & BGD.LE$Year <= 2016,]
BGD.HC <- as.numeric(transpose(dfHC[dfHC$Country.Code == "BGD",])[45:61,])
BGD.HC <- AFG.HC <- data.frame("Country" = "Bangladesh", "Year" = c(2000:2016),
                               "HCpC" = BGD.HC)
BGD <- merge.data.frame(BGD.LE, BGD.HC, by.x = "Year", by.y = "Year")
BGD <- BGD[,c(1,2,3,5)]
rm(BGD.LE,BGD.HC)

#India datacleansing and creating dataframe
IND.LE <- as.numeric(transpose(dfLE[dfLE$Country.Code == "IND",])[5:62,])
IND.LE <- data.frame("Country" = "India", "Year" = c(1960:2017),"LE" = IND.LE)
IND.LE <- IND.LE[IND.LE$Year >= 2000 & IND.LE$Year <= 2016,]
IND.HC <- as.numeric(transpose(dfHC[dfHC$Country.Code == "IND",])[45:61,])
IND.HC <- AFG.HC <- data.frame("Country" = "India", "Year" = c(2000:2016),
                               "HCpC" = IND.HC)
IND <- merge.data.frame(IND.LE, IND.HC, by.x = "Year", by.y = "Year")
IND <- IND[,c(1,2,3,5)]
rm(IND.LE,IND.HC)

#Sri Lanka datacleansing and creating dataframe
LKA.LE <- as.numeric(transpose(dfLE[dfLE$Country.Code == "LKA",])[5:62,])
LKA.LE <- data.frame("Country" = "Sri Lanka", "Year" = c(1960:2017),"LE" = LKA.LE)
LKA.LE <- LKA.LE[LKA.LE$Year >= 2000 & LKA.LE$Year <= 2016,]
LKA.HC <- as.numeric(transpose(dfHC[dfHC$Country.Code == "LKA",])[45:61,])
LKA.HC <- AFG.HC <- data.frame("Country" = "Sri Lanka", "Year" = c(2000:2016),
                               "HCpC" = LKA.HC)
LKA <- merge.data.frame(LKA.LE, LKA.HC, by.x = "Year", by.y = "Year")
LKA <- LKA[,c(1,2,3,5)]
rm(LKA.LE,LKA.HC)

#Pakistan datacleansing and creating dataframe
PAK.LE <- as.numeric(transpose(dfLE[dfLE$Country.Code == "PAK",])[5:62,])
PAK.LE <- data.frame("Country" = "Pakistan","Year" = c(1960:2017), "LE" = PAK.LE)
PAK.LE <- PAK.LE[PAK.LE$Year >= 2000 & PAK.LE$Year <= 2016,]
PAK.HC <- as.numeric(transpose(dfHC[dfHC$Country.Code == "PAK",])[45:61,])
PAK.HC <- AFG.HC <- data.frame("Country" = "Pakistan", "Year" = c(2000:2016),
                               "HCpC" = PAK.HC)
PAK <- merge.data.frame(PAK.LE, PAK.HC, by.x = "Year", by.y = "Year")
PAK <- PAK[,c(1,2,3,5)]
rm(PAK.LE,PAK.HC)

#combine different country datasets
df.final <- rbind(BGD,IND,LKA,PAK)
df.final

#Call ggplot library and create the aesthetics for the graph
library(dplyr)
library(plotly)
library(lubridate)

#Create function needed to accumulate the plotly chart by Year
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

#define the accumulated variable for plotly
df.final <- df.final %>%
  accumulate_by(~Year)

df.final

#Create the plotly chart
p <- plot_ly(data = df.final, 
             x = ~LE, 
             y = ~HCpC,
             color = ~Country.x,
             frame = ~frame,
             type = "scatter", 
             mode = "lines") %>% animation_opts(
               frame = 165,
               transition = 50,
               redraw = FALSE
             ) %>% animation_slider(
               currentvalue = list(prefix = "Year ")
             ) %>% layout(
               title = "Life Expectancy vs. Health Care Expenditure / Capita",
               xaxis = list(title = "Life Expectancy"),
               yaxis = list(title = "Health Care Expenditure / Capita ($)")
             )
p
