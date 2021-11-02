#Matthew Douglas Green
#November 9, 2021
#Project: Spring Health Data Scientist  Assessment

#Part 2: Data Visualization

source("Code/Data_prep.R")

#Are people who book appointment near day/time more likely to attend?
################################################################################################################################################
#A: Member Data

#Geographic Variation
data_apt_hashed%>%
  group_by(member_user_state) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(member_user_state, -count),y=count, fill=member_user_state)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "State", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")


data_apt_hashed%>%
  group_by(appointment_attended) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(appointment_attended, -count),y=count, fill=appointment_attended)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "appointment attended", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")  

data_apt_hashed%>%
  group_by(appointment_kind) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(appointment_kind, -count),y=count, fill=appointment_kind)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "appointment kind", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")  


data_apt_hashed%>%
  group_by(appointment_medium) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(appointment_medium, -count),y=count, fill=appointment_medium)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "appointment medium", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 


data_apt_hashed%>%
  filter(basic_visits_covered<300)%>%
  ggplot(aes(x =basic_visits_covered,y=specialist_visits_covered)) + 
  geom_point()+
  geom_smooth(method="lm")+
  labs(x = "appointment medium", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

data_apt_hashed%>%
  group_by(provider_id_hashed)%>%
  summarise(mean_visit_covered=mean(basic_visits_covered))%>%
  ggplot(aes(x =reorder(provider_id_hashed,-mean_visit_covered),y=mean_visit_covered, fill=provider_id_hashed)) + 
  geom_bar(stat = "identity")+
  scale_fill_viridis(discrete = T)+
  labs(x = "Providers", y = "Basic Visits Covered") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

data_apt_hashed%>%
  group_by(provider_id_hashed)%>%
  summarise(mean_visit_covered=mean(specialist_visits_covered))%>%
  ggplot(aes(x =reorder(provider_id_hashed,-mean_visit_covered),y=mean_visit_covered, fill=provider_id_hashed)) + 
  geom_bar(stat = "identity")+
  scale_fill_viridis(discrete = T)+
  labs(x = "Providers", y = "Specialist Visits Covered") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

#Booking appointment
data_apt_hashed%>%
  mutate(day=wday(appointment_created,label = TRUE, abbr = FALSE))%>%
  group_by(day) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(day, -count),y=count, fill=day)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "day apt created", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

#Apportionment Start Time
data_apt_hashed%>%
  mutate(day=wday(appointment_start_time,label = TRUE, abbr = FALSE))%>%
  group_by(day) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(day, -count),y=count, fill=day)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "Day Appointment Started", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

data_apt_hashed%>%
  separate("appointment_start_time", sep="T" ,into=c("appointment_start_date", "appointment_start_time"))%>%
  separate("appointment_start_time", sep="Z" ,into=c( "appointment_start_time"))%>%
  group_by(appointment_start_time) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(appointment_start_time, -count),y=count, fill=appointment_start_time)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "Day Appointment Started", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

data_time_apt<-data_apt_hashed%>%
  separate("appointment_start_time", sep="T" ,into=c("appointment_start_date", "appointment_start_time"))%>%
  separate("appointment_start_time", sep="Z" ,into=c( "appointment_start_time"))%>%
  separate("appointment_created", sep="T" ,into=c("appointment_created_date", "appointment_created_time"))%>%
  separate("appointment_created_time", sep="Z" ,into=c( "appointment_created_time"))%>%
  mutate(diff_dat_create_start=difftime(appointment_start_date,appointment_created_date, unit="days"))%>%
  separate("diff_dat_create_start", sep="days" ,into=c( "diff_dat_create_start"))%>%
  mutate(diff_dat_create_start=as.numeric(diff_dat_create_start))
  
data_time_apt%>% 
  filter(diff_dat_create_start>-1)%>%
  ggplot(aes(x = appointment_medium,y=diff_dat_create_start, fill=appointment_medium)) + 
  geom_boxplot()+
  scale_fill_viridis(discrete = T)+
  labs(x = "Appointment Medium", y = "diff_days_create_start") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

data_time_apt%>% 
  filter(diff_dat_create_start>-1)%>%
  filter(appointment_attended !="NA")%>%
  ggplot(aes(x = appointment_attended,y=diff_dat_create_start, fill=appointment_attended)) + 
  geom_boxplot()+
  scale_fill_viridis(discrete = T)+
  labs(x = "Appointment Attended", y = "diff_days_create_start") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")
################################################################################################################################################
#B: Provider Data

data_providers_hased%>%
  group_by(roles) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(roles, -count),y=count, fill=roles)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "appointment medium", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

data_providers_hased%>%
  group_by(states) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(states, -count),y=count, fill=states)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "appointment medium", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

data_providers_hased%>%
  group_by(licenses) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(licenses, -count),y=count, fill=licenses)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "appointment medium", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

data_providers_hased%>%
  group_by(status) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(status, -count),y=count, fill=status)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "status", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 

data_providers_hased%>%
  group_by(ther_vs_mm) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(ther_vs_mm, -count),y=count, fill=ther_vs_mm)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T)+
  labs(x = "appointment medium", y = "Count") +  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") 
