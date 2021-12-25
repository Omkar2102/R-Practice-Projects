#connecting to the SQL server
library(odbc)
con <- dbConnect()
#selecting the dataset required
library(RODBC)
Service_Request_Escalations <- dbGetQuery(con,"select top 1000 * from Service_Request_Escalations")
#Removing the unwanted columns
library(dplyr)
library(readr)
library(ggplot2)
Service_Request_Escalations <- Service_Request_Escalations %>%
  select(-USER_EMAIL,-USER_MOBILE_NUMBER)
Service_Request_Escalations <- Service_Request_Escalations %>%
  select(-SERVICE_REQUEST_STATUS)
Service_Request_Escalations <- Service_Request_Escalations %>%
  select(-CREATED_BY)
#Finding unique values in all variables
unique(Service_Request_Escalations$ESCALATION_LEVEL)
unique(Service_Request_Escalations$MASTER_CTST_LOANS_ID)
unique(Service_Request_Escalations$CREATED_BY)
unique(Service_Request_Escalations$CREATED)
unique(Service_Request_Escalations$SERVICE_REQUEST_ID)
unique(Service_Request_Escalations$USER_ID)

#replacing unique values
Service_Request_Escalations$USER_ID[Service_Request_Escalations$USER_ID == "kol_gprs"] <- "KOL_GPRS"
unique(Service_Request_Escalations$USER_ID)

#Grouping by escalation levels and their counts
Group_by_escalation_level <- Service_Request_Escalations %>%
  group_by(ESCALATION_LEVEL)%>%
  summarise(Quant=n())
#grouping by created
Group_by_created <- Service_Request_Escalations %>%
  group_by(CREATED) %>%
  summarise (Quant=n())
#group by master city loans id
Group_by_mcli <- Service_Request_Escalations %>%
  group_by(MASTER_CTST_LOANS_ID) %>%
summarise(Quant=n())
#group by service request id
Group_by_sri <- Service_Request_Escalations %>%
  group_by(SERVICE_REQUEST_ID) %>%
  summarise(Quant=n())
#group by user id
Group_by_User_id <- Service_Request_Escalations %>%
  group_by(USER_ID)%>%
  summarise(Quant=n())
#grouping by  eascalation level and user id

Group_by_el_ui <- Service_Request_Escalations %>%
  group_by(ESCALATION_LEVEL,USER_ID) %>%
  summarise(Quant=n())
library(plotly)
# Plotting Group by el and ui  USER_ID
library(ggplot2)
ggplot(data = Group_by_el_ui) + aes(x= Group_by_el_ui$ESCALATION_LEVEL,y = Group_by_el_ui$USER_ID)+geom_point()

#grouping by 

