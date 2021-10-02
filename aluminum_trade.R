#Aluminum tariffs and import/export fluctuations
#Feb. 2021
library(ggplot2)
library(zoo)
library(dplyr)
library(readxl)
library(scales)

#imported data from U.S. International Trade Commission DataWeb
#HTS codes included: 7601, 7608, 7607, 7606, 7605, 76169951, 7609, 7604
customs_val15 <- read_excel("import_customs_value.xlsx", 
                                   sheet = "2015, Customs Value")
customs_val16 <- read_excel("import_customs_value.xlsx", 
                                   sheet = "2016, Customs Value")
customs_val17 <- read_excel("import_customs_value.xlsx", 
                                   sheet = "2017, Customs Value")
customs_val18 <- read_excel("import_customs_value.xlsx", 
                                   sheet = "2018, Customs Value")
customs_val19 <- read_excel("import_customs_value.xlsx", 
                                   sheet = "2019, Customs Value")
customs_val20 <- read_excel("import_customs_value.xlsx", 
                                   sheet = "2020, Customs Value")

# kg15 <- read_excel("Downloads/import_kg.xlsx", 
#                         sheet = "2015, First Unit of Quantity")
# kg16 <- read_excel("Downloads/import_kg.xlsx", 
#                         sheet = "2016, First Unit of Quantity")
# kg17 <- read_excel("Downloads/import_kg.xlsx", 
#                         sheet = "2017, First Unit of Quantity")
# kg18 <- read_excel("Downloads/import_kg.xlsx", 
#                         sheet = "2018, First Unit of Quantity")
# kg19 <- read_excel("Downloads/import_kg.xlsx", 
#                         sheet = "2019, First Unit of Quantity")
# kg20 <- read_excel("Downloads/import_kg.xlsx", 
#                         sheet = "2020, First Unit of Quantity")

exp15 <- read_excel("exports.xlsx", 
                   sheet = "2015, FAS Value")
exp16 <- read_excel("exports.xlsx", 
                   sheet = "2016, FAS Value")
exp17 <- read_excel("exports.xlsx", 
                   sheet = "2017, FAS Value")
exp18 <- read_excel("exports.xlsx", 
                   sheet = "2018, FAS Value")
exp19 <- read_excel("exports.xlsx", 
                   sheet = "2019, FAS Value")
exp20 <- read_excel("exports.xlsx", 
                   sheet = "2020, FAS Value")

#remove units column for quantity data
# kg15 <- kg15[,-3]
# kg16 <- kg16[,-3]
# kg17 <- kg17[,-3]
# kg18 <- kg18[,-3]
# kg19 <- kg19[,-3]
# kg20 <- kg20[,-3]

make_dates <- function(year){
  months <- vector(mode = "list", length = 12)
  i=1
  for (m in month.abb){
    months[i] <- paste(m, year)
    i = i+1
  }
  return(unlist(months))
}

process_data <- function(excel_df, year){
  import <- as.data.frame(t(as.matrix(excel_df)))
  import <- subset(import, select = -c(V1) )
  import <- import[-c(1),]
  colnames(import) <- as.character(unlist(import[1,]))
  import <- import[-c(1),]
  colnames(import)[length(import)] <- "total"
  dates <- make_dates(year)
  dates <- as.yearmon(dates)
  import$date <- dates
  tot <- factor(import$total)
  import$total <- as.numeric(as.character(tot))
  return(import)
}

#process excel files
import2015val <- process_data(customs_val15, 2015)
import2016val <- process_data(customs_val16, 2016)
import2017val <- process_data(customs_val17, 2017)
import2018val <- process_data(customs_val18, 2018)
import2019val <- process_data(customs_val19, 2019)
import2020val <- process_data(customs_val20, 2020)

# import2015kg <- process_data(kg15, 2015)
# import2016kg <- process_data(kg16, 2016)
# import2017kg <- process_data(kg17, 2017)
# import2018kg <- process_data(kg18, 2018)
# import2019kg <- process_data(kg19, 2019)
# import2020kg <- process_data(kg20, 2020)

exportfas2015 <- process_data(exp15,2015)
exportfas2016 <- process_data(exp16,2016)
exportfas2017 <- process_data(exp17,2017)
exportfas2018 <- process_data(exp18,2018)
exportfas2019 <- process_data(exp19,2019)
exportfas2020 <- process_data(exp20,2020)


#combine data from all years
combine_yrs <- function(df1,df2,df3,df4,df5,df6){
  all_yrs <- bind_rows(df1, df2)
  all_yrs <- bind_rows(all_yrs, df3)
  all_yrs <- bind_rows(all_yrs, df4)
  all_yrs <- bind_rows(all_yrs, df5)
  all_yrs <- bind_rows(all_yrs, df6)
  return(all_yrs)
}

all_customs <- combine_yrs(import2015val,import2016val,import2017val,import2018val,import2019val,import2020val)
#all_kg <- combine_yrs(import2015kg,import2016kg,import2017kg,import2018kg,import2019kg,import2020kg)
all_exp_fas <- combine_yrs(exportfas2015,exportfas2016,exportfas2017,exportfas2018,exportfas2019,exportfas2020)
# canada <- combine_yrs(import2015kg[c('Canada','date')],import2016kg[c('Canada','date')],
#                       import2017kg[c('Canada','date')],import2018kg[c('Canada','date')],
#                       import2019kg[c('Canada','date')],import2020kg[c('Canada','date')])

#shorter date range (2016-2020)
all_cust_smaller <- all_customs[13:72,]
all_exp_smaller <- all_exp_fas[13:72,]

#PLOT VISUALIZATIONS 
#all years, all US imports 2015-2020
ggplot(data=all_customs, aes(x=factor(date), y=total, group=1)) +
  geom_line() + geom_point()  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_vline(xintercept = "Mar 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_text(aes(x="Jun 2018", label="Mar. 8: Tariff Announced, Mar. 23: Tariff implemented", y=650000000), colour="blue") +
  xlab("Date") + ylab("Total Customs Value ($)") + ggtitle("US Aluminum Imports from World") 


# ggplot(data=all_kg, aes(x=factor(date), y=total, group=1)) +
#   geom_line() + geom_point()  +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   geom_vline(xintercept = 'Mar 2018', linetype="dashed", 
#              color = "black", size=.5) +
#   geom_text(aes(x="Jun 2018", label="Mar. 8: Tariff Announced, Mar. 23: Tariff implemented", y=650000000), colour="blue") +
#   xlab("Date") + ylab("Total Quantity (kg)") + ggtitle("US Aluminum Imports from World")

#US exports to top importers 2016-2020
all_exp_fas['South_Korea'] <- all_exp_fas['South Korea']
axis_labels_exp <- c("Jan 2016","","","Apr 2016","","","Jul 2016","","","Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Oct 2017","","","Jan 2018","","","Apr 2018","","","Jul 2018","","","Oct 2018","","","Jan 2019","","","Apr 2019","","","Jul 2019","","","Oct 2019","","","Jan 2020","","","Apr 2020","","","Jul 2020","","","Oct 2020","","")
ggplot()+
  geom_line(data=all_exp_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(China)))/1000000, color="blue"), group=1) + 
  geom_line(data=all_exp_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(Canada)))/1000000,color="red"),group=2) +
  geom_line(data=all_exp_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(Japan)))/1000000,color="purple"),group=3) +
  geom_line(data=all_exp_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(Mexico)))/1000000,color="orange"),group=4) +
  geom_line(data=all_exp_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(South_Korea)))/1000000,color="green"),group=5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_vline(xintercept = "Mar 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_vline(xintercept = "Apr 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_vline(xintercept = "Jun 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_vline(xintercept = "Jan 2020", linetype="dashed", 
             color = "black", size=.5) +
  xlab("Date") + ylab("FAS Value (million USD)") + ggtitle("U.S. Aluminum Exports", subtitle ="Dashed lines represent changes in trading policy") +
  labs(caption = "Data Source: U.S. International Trade Commission") +
  scale_color_discrete(name = "Importing Country",labels=c("China","South Korea","Mexico","Japan","Canada")) +
  scale_x_discrete(labels = axis_labels_exp) + scale_y_continuous(labels = comma)


#trade with top 5 countries exporting to U.S.2015-2020
all_customs['UAE'] <- all_customs['United Arab Em']
axis_labels <- c("Jan 2015","","","Apr 2015","","","Jul 2015","","","Oct 2015","","","Jan 2016","","","Apr 2016","","","Jul 2016","","","Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Oct 2017","","","Jan 2018","","","Apr 2018","","","Jul 2018","","","Oct 2018","","","Jan 2019","","","Apr 2019","","","Jul 2019","","","Oct 2019","","","Jan 2020","","","Apr 2020","","","Jul 2020","","","Oct 2020","","")
ggplot()+
  geom_line(data=all_customs, aes(x=factor(date), y=as.numeric(as.character(factor(China)))/1000000, color="blue"), group=1) + 
  geom_line(data=all_customs, aes(x=factor(date), y=as.numeric(as.character(factor(Canada)))/1000000,color="red"),group=2) +
  geom_line(data=all_customs, aes(x=factor(date), y=as.numeric(as.character(factor(UAE)))/1000000,color="purple"),group=3) +
  geom_line(data=all_customs, aes(x=factor(date), y=as.numeric(as.character(factor(Mexico)))/1000000,color="green"),group=4) +
  geom_line(data=all_customs, aes(x=factor(date), y=as.numeric(as.character(factor(Germany)))/1000000,color="orange"),group=5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_vline(xintercept = "Mar 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_vline(xintercept = "Apr 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_vline(xintercept = "Jun 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_vline(xintercept = "Jan 2020", linetype="dashed", 
             color = "black", size=.5) +
  xlab("Date") + ylab("Total Customs Value (million USD)") + 
  ggtitle(label="U.S. Aluminum Imports from Top Aluminum Trade Partners", subtitle ="Dashed lines represent changes in trading policy") + 
  labs(caption = "Data Source: U.S. International Trade Commission") +
  scale_color_discrete(name = "Exporting Country",labels=c("China","Mexico","Germany","UAE","Canada")) +
  scale_x_discrete(labels = axis_labels) + scale_y_continuous(labels = comma)

#trade with top 5 countries exporting to U.S. 2016-2020
axis_labels_small <- c("Jan 2016","","","Apr 2016","","","Jul 2016","","","Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Oct 2017","","","Jan 2018","","","Apr 2018","","","Jul 2018","","","Oct 2018","","","Jan 2019","","","Apr 2019","","","Jul 2019","","","Oct 2019","","","Jan 2020","","","Apr 2020","","","Jul 2020","","","Oct 2020","","")
all_cust_smaller['UAE'] <- all_cust_smaller['United Arab Em']
ggplot()+
  geom_line(data=all_cust_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(China)))/1000000, color="blue"), group=1) + 
  geom_line(data=all_cust_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(Canada)))/1000000,color="red"),group=2) +
  geom_line(data=all_cust_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(UAE)))/1000000,color="purple"),group=3) +
  geom_line(data=all_cust_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(Mexico)))/1000000,color="green"),group=4) +
  geom_line(data=all_cust_smaller, aes(x=factor(date), y=as.numeric(as.character(factor(Germany)))/1000000,color="orange"),group=5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_vline(xintercept = "Mar 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_vline(xintercept = "Apr 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_vline(xintercept = "Jun 2018", linetype="dashed", 
             color = "black", size=.5) +
  geom_vline(xintercept = "Jan 2020", linetype="dashed", 
             color = "black", size=.5) +
  xlab("Date") + ylab("Total Customs Value (million USD)") + ggtitle("U.S. Aluminum Imports from Top Aluminum Trade Partners") +
  scale_color_discrete(name = "Exporting Country",labels=c("China","Mexico","Germany","UAE","Canada")) +
  scale_x_discrete(labels = axis_labels_small) + scale_y_continuous(labels = comma)


# ggplot(data=canada, aes(x=factor(date), y=as.numeric(as.character(factor(Canada))), group=1)) +
#   geom_line() + geom_point()   +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   geom_vline(xintercept = "Mar 2018", linetype="dashed", 
#              color = "black", size=.5) +
#   geom_text(aes(x="Jun 2018", label="Mar. 8: Tariff Announced, Mar. 23: Tariff implemented", y=650000000), colour="blue") +
#   xlab("Date") + ylab("Total Quantity (kg)") + ggtitle("US Aluminum Imports from Canada")



