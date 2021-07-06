#import packages to be used from library 

library(dplyr)
library(DT)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(shinydashboard)
library(googleVis)
library(leaflet)
library(maps)
library(mapdeck)
library(mapview)
library(ggmap)
library(sp)
library(highcharter)
library(stringr)
library(withr)
library(treemap)
library(timevis)
library(WDI)
library(geosphere)
library(magrittr)
library(scales)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinycustomloader)
library(googleVis)
library(fresh)
library(dashboardthemes)
library(plotly)
library(treemapify)

#import/load data sets for employees
design <- read_csv("./data/s_designation.csv")
employee <- read_csv("./data/s_employee.csv")
skill <- read_csv("./data/s_skill_level.csv")

#import/load data sets for projects
project <- read_csv("./data/s_project.csv")
plocation <- read_csv("./data/s_project_location.csv")
client <- read_csv("./data/s_client.csv")

#Create Resource Master
RMaster <- employee %>% 
  left_join(design, 'Eid') %>%
  left_join(skill,'Eid') %>% 
  rename(.,Ename="Ename.x")
RMaster$Ename.y <- NULL

#sub-resource master for review purpose
RMasters <- RMaster %>% 
  select(., c(1:11))

PF <- RMasters %>%
  select(.,c(1,2,3,4,5,6,7,8,9,10,11))

#Dollar format
USD <- dollar_format(prefix = "$")

#Create Project Master
PMaster <- project %>%
  left_join(plocation, 'Location_Index') %>%
  left_join(client, 'Client_ID') %>% 
  transform(Project_Budget = as.numeric(Project_Budget))%>%
  mutate(Project_Budget2 = USD(Project_Budget))%>%
  unite(label,c('Project_Name','Project_Budget2'),sep = ' - ',remove = FALSE)

TP <- PMaster %>%
      select(.,c(3,5,9))
TPP <- summarise(group_by(TP,`Domain_Skill`,`ISO`),Budget=sum(Project_Budget))

#Set colors
label_col = "#cc4c02"
a_col = "#cc4c02"
b_col = "#662506"
c_col = "#045a8d"
d_col = "#4d004b"
e_col = "#016c59"

#Subset of tables for review purpose

#1

List_of_Consultant = subset(RMaster, select=c('Eid','Ename','Designation','Rating','Total_projects'))%>%
                     rename(.,'Consultant Name'='Ename','Project Experience' ='Total_projects')


#2

Area_of_Interest = subset(RMaster, select=c('Eid','Ename','Area_of_Interest_1','Area_of_Interest_2','Area_of_Interest_3'))%>%
                   rename(.,'Consultant Name'='Ename','Highly Interested' ='Area_of_Interest_1','Moderately Interested'='Area_of_Interest_2',
                          'Slightly Interested'='Area_of_Interest_3')

#3

Language = subset(RMaster, select=c('Eid','Ename','Language1','Language2','Language3'))%>%
           rename(.,'Consultant Name' ='Ename','First Language'='Language1','Second Language'='Language2',
                  'Third Language'='Language3')

#4

Project_Experience = subset(RMaster, select=c('Eid','Ename','Total_projects','AI_project_count','ML_project_count',
                                              'JS_project_count','Java_project_count','DotNet_project_count',
                                              'Mobile_project_count')) %>%
                     rename(.,'Project Experience' ='Total_projects', AI='AI_project_count',ML='ML_project_count',
                            JS='JS_project_count',Java='Java_project_count',
                            DotNet='DotNet_project_count',Mobile='Mobile_project_count')

#5

Skill_Level = subset(RMaster, select=c('Eid','Ename','Python','Machine Learning','Deep Learning','Data Analysis','Asp.Net',
                                       'Ado.Net','VB.Net','C#','Java','Spring Boot','Hibernate','NLP','CV','JS','React',
                                       'Node','Angular','Dart','Flutter','Vb.Net')) %>%
                     rename(.,'Consultant Name' ='Ename')

#6

Client_Project = subset(PMaster, select=c('Client_ID','Client_Name','Project_Name','Project_Budget','Domain_Skill','City_Name','Admin_Name','Country'))%>%
                 rename(.,'Client Code' ='Client_ID','Client'='Client_Name','Project Name'='Project_Name','Project Scale'='Project_Budget',
                        'Demanding Domain Skill'='Domain_Skill','City'='City_Name','Province/State'='Admin_Name')
#7

Client_Project2 = subset(PMaster, select=c('Client_ID','Client_Name','Project_Name','Project_Budget','Domain_Skill','City_Name','Admin_Name','Country'))%>%
  rename(.,'Client Code' ='Client_ID','Client'='Client_Name','Project Name'='Project_Name','Project Scale'='Project_Budget',
         'Demanding Domain Skill'='Domain_Skill','City'='City_Name','Province/State'='Admin_Name')
as.factor(Client_Project2$City)
as.factor(Client_Project2$'Project Name')

#8

Profile = subset(RMaster, select=c('Ename','Designation','Experience','Total_projects','Rating','AI_project_count','ML_project_count',
                                              'JS_project_count','Java_project_count','DotNet_project_count',
                                              'Mobile_project_count','Area_of_Interest_1','Area_of_Interest_2','Area_of_Interest_3',
                                   'Language1','Language2','Language3')) %>%
  rename(.,`Consultant Name` ='Ename',Position = 'Designation', 'Project Experience' ='Total_projects', 
         AI='AI_project_count',ML='ML_project_count',
         JS='JS_project_count',Java='Java_project_count',
         DotNet='DotNet_project_count',Mobile='Mobile_project_count',
         `First Language`='Language1',`Second Language`='Language2',
         `Third Language`='Language3') 

#Choice list
choice1 <- colnames(Client_Project)[-1]
choice2 <- colnames(PMaster)[-1]

#write.csv(RMaster,"RMaster.csv", row.names = FALSE)
#write.csv(PMaster,"PMaster.csv", row.names = FALSE)
