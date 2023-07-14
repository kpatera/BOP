#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list = ls(all.names = TRUE))
library(shiny)
library(readxl)
library(shinyWidgets)
require(DT)
require(PriorGen)

library(readxl)
mult=c(0.0833333333333333, 0.166666666666667, 0.25, 0.333333333333333, 
       0.416666666666667, 0.5, 0.583333333333333, 0.666666666666667, 
       0.75, 0.833333333333333, 0.916666666666667, 1, 1, 1, 1, 1, 1, 
       1, 1, 1, 1, 1)
VDEV_latest <- data.frame(read_excel("VaccineData_EudraVigilance_latest.xlsx"))
# 
# Table_init<<-
#   structure(list(Date = c("N20", "D20", "J21", "F21", "M21", "A21",
#                           "Ma21", "J21", "Ju21", "A21", "S21", "O21"),
#                  Anaphylaxis = c(0, 0, 1, 3, 10, 19, 34, 55, 80, 103, 132, 158),
#                  Cerebral.Thrombosis = c(0, 0, 0, 2, 6, 11, 20, 33, 48, 61, 79, 94),
#                  GBS = c(0, 0, 1, 7, 22, 40, 74, 119, 173, 222, 285, 341),
#                  Thrombocytocis = c(0, 0, 0, 0, 1, 2, 3, 5, 7, 9, 11, 13),
#                  Total_full = c(19, 81, 144, 1623, 1212042, 7860612, 17894594, 25282794, 30485146, 33459987, 37224028, 39093203),
#                  Total_doses = c(19, 155, 204024, 1334709, 4848062.5, 11601500, 22259930, 34642535, 46126145, 54454946, 60351020.5, 62872853),
#                  Total_doses_Total = c(19, 155, 204024, 1334709, 4848062, 11601500, 22259930, 34642535, 46126145, 54454946, 60351021, 62872853)),
#             class = "data.frame", row.names = c(NA, -12L))
# 
# Table_init$Date2<-factor(x = 1:dim(Table_init)[1],
#                          labels = c("Dec 2020","Jan 2021","Feb 2021","Mar 2021","Apr 2021",
#                                     "May 2021","Jun 2021","Jul 2021","Aug 2021","Sep 2021",
#                                     "Oct 2021","Nov 2021"),ordered = TRUE)
# Example_Anaphylactic<-data.frame(Table_init[,c(2,7,9)])
# Example_Cerebral<-data.frame(Table_init[,c(3,7,9)])
# Example_GBS<-data.frame(Table_init[,c(4,7,9)])
# Example_Thrombosis<-data.frame(Table_init[,c(5,7,9)])
# rm(Table_init)
# temp = ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
# 

VDEV_latest[is.na(VDEV_latest)]<-0

grp=(1:nrow(VDEV_latest) - 1)%/%4
ID_new<-aggregate(VDEV_latest$YearWeek_VaccineTracker, list(grp),function(x) paste(range(x), collapse="--"))$x
ID_new<-paste("4W",1:length(ID_new),sep="")
VDEV_latest<-VDEV_latest[,-c(1,3)]



VDEV_latest<-data.frame(
  YearWeeks = ID_new,
  aggregate(VDEV_latest[,c(2,3:6)], list(grp), sum)  ,
  stringsAsFactors = FALSE)


VDEV_latest$cum_EM<-cumsum(VDEV_latest$EM)
VDEV_latest$cum_MIS<-cumsum(VDEV_latest$MIS)
VDEV_latest$cum_MC<-cumsum(VDEV_latest$MC)
VDEV_latest$cum_Doses<-cumsum(VDEV_latest$Doses_admin)

Table_init<<-
  structure(list(Date2 = VDEV_latest$YearWeeks, 
                 ErythemaM = VDEV_latest$cum_EM, 
                 MIS = VDEV_latest$cum_MIS, 
                 Myocarditis = VDEV_latest$cum_MC, 
                 Total_doses = VDEV_latest$cum_Doses), 
            class = "data.frame", row.names = c(NA, -22L))


N=dim(Table_init)[1]
Example_ErythemaM<-data.frame(Table_init[,c(2,5,1)])
Example_MIS<-data.frame(Table_init[,c(3,5,1)])
Example_Myocarditis<-data.frame(Table_init[,c(4,5,1)])

multip<<-c(1:12,rep(12,dim(VDEV_latest[-c(1:12),])[1]))/12

rm(Table_init);rm(VDEV_latest)
temp = ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']

source("Functions/Table2out.R",local = TRUE)
source("Functions/overlap.R",local = TRUE)
source("Functions/betalap.R",local = TRUE)
source("Functions/int_f2.R",local = TRUE)
source("Functions/findbeta2.R",local = TRUE)

