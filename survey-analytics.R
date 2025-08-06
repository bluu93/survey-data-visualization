library(tidyverse)
library(paletteer)
library(skimr)

# Definitions
#-------------------------------------------------------------------------------

likertWellness <- c("Very bad","Bad","Neither good nor bad","Good","Very good",NA)
collegeList <- c("Arts","Business","Liberal Arts","Education","Engineering",
                 "Health and Human Services","Natural Sciences and Mathematics","University Programs",NA)

fallStart <- as.Date("2024-09-16")
fallEnd <- as.Date("2024-11-08")

springStart <- as.Date("2025-02-03")
springEnd <- as.Date("2025-04-18")

# Function to creating datasets
#-------------------------------------------------------------------------------

createData <- function(startDate,endDate,Term){
  
  seqDates <- seq.Date(startDate,endDate,by = "day")
  
  minutesAlpha <- 1.5
  minutesBeta <- 1
  
  if(Term == "Fall"){
    hoursAlpha <- 3.5
    hoursBeta <- 2
    datesAlpha <- 1.5
    datesBeta <- 4
    
    set.seed(37724)
    participationPercent <- runif(1,0.60,0.65)
  }
  else if(Term == "Spring"){
    hoursAlpha <- 4
    hoursBeta <- 1.5
    datesAlpha <- 2
    datesBeta <- 1.5
    
    set.seed(37725)
    participationPercent <- runif(1,0.45,0.50)
  }
  
  surveyInvites <- 1000
  
  # Hours
  timeHours <- 0:23
  x.norm <- (timeHours - min(timeHours)) / (max(timeHours) - min(timeHours))
  density_vals <- dbeta(x.norm, shape1 = hoursAlpha, shape2 = hoursBeta)
  prob_vals <- density_vals/sum(density_vals)
  surveyHours <- sample(as.character(timeHours), 
                        size = surveyInvites, 
                        replace = TRUE, 
                        prob = prob_vals) %>% 
    str_pad(width = 2,side = "left",pad = "0")
  
  # Minutes
  timeMinutes <- 0:59
  x.norm <- (timeMinutes - min(timeMinutes)) / (max(timeMinutes) - min(timeMinutes))
  density_vals <- dbeta(x.norm, shape1 = minutesAlpha, shape2 = minutesBeta)
  prob_vals <- density_vals/sum(density_vals)
  surveyMinutes <- sample(as.character(timeMinutes), 
                          size = surveyInvites, 
                          replace = TRUE, 
                          prob = prob_vals) %>% 
    str_pad(width = 2,side = "left",pad = "0")
  
  surveyTime <- paste(surveyHours,surveyMinutes,sep = ":")
  
  # Dates
  date_indices <- as.numeric(seqDates - startDate)
  scaled_dates <- date_indices/max(date_indices)
  density_vals <- dbeta(scaled_dates,shape1 = datesAlpha,shape2 = datesBeta)
  prob_vals <- density_vals/sum(density_vals)
  
  sampled_dates <- sample(seqDates, size = 1000, replace = TRUE, prob = prob_vals)
  
  tempUnif <- runif(n = 6,min = 0,max = 100)
  tempProbs <- tempUnif/sum(tempUnif)
  mental_health = sample(likertWellness,size = surveyInvites,replace = TRUE,
                         prob = tempProbs)
  
  tempUnif <- runif(n = 6,min = 0,max = 100)
  tempProbs <- tempUnif/sum(tempUnif)
  physical_health = sample(likertWellness,size = surveyInvites,replace = TRUE,
                           prob = tempProbs)
  
  tempUnif <- runif(n = 6,min = 0,max = 100)
  tempProbs <- tempUnif/sum(tempUnif)
  social_health = sample(likertWellness,size = surveyInvites,replace = TRUE,
                         prob = tempProbs)
  
  # Defining the data frame
  df <- data.frame(
    user_id = seq_along(1:surveyInvites),
    college = sample(collegeList,size = surveyInvites,replace = TRUE),
    
    start_date = paste(sampled_dates,surveyTime,sep = " "),
    
    mental_health = mental_health,
    physical_health = physical_health,
    social_health = social_health
  )
  
  df <- df[sample(1:nrow(df),
                  size = participationPercent*nrow(df),
                  replace = FALSE),]
  
  return(df)
}


# Create datasets
#-------------------------------------------------------------------------------

df24 <- createData(fallStart,fallEnd,"Fall")
df25 <- createData(springStart,springEnd,"Spring")


# Fall 2024
#-------------------------------------------------------------------------------
df24 %>% head()
skim(df24)

# Spring 2025
#-------------------------------------------------------------------------------
df25 %>% head()
skim(df25)


################################################################################
################################################################################


# Begin Visualization
#-------------------------------------------------------------------------------

likert.order.5 <- c("Very bad","Bad","Neither good nor bad","Good","Very good")


# Some Pre-processing
#-------------------------------------------------------------------------------

names(df24) <- names(df24) %>% 
  str_replace(pattern = "_",replacement = " ") %>% 
  str_to_title()

names(df25) <- names(df25) %>% 
  str_replace(pattern = "_",replace = " ") %>% 
  str_to_title()


# Heatmap of Survey Dates
#-------------------------------------------------------------------------------

ref_dates <- read_csv("csulb_2425_calendar.csv",show_col_types = FALSE)
ref_dates <- ref_dates %>% 
  mutate(Date = dmy(Date))

df_startDates <- df24 %>% 
  bind_rows(df25)

dates_heatmap <- df_startDates %>% 
  select(`Start Date`) %>% 
  separate_wider_delim(cols = `Start Date`,delim = " ",names = c("Date","Time")) %>% 
  select(Date) %>% 
  table() %>% 
  as.data.frame() %>% 
  mutate(Date = ymd(Date)) %>% 
  complete(Date = seq(as.Date("2024-07-28"),as.Date("2025-05-31"),by = "day")) %>% 
  mutate(month = month(Date,label = TRUE),
         wday = wday(Date,label = TRUE),
         day = day(Date),
         week = as.integer(
           ((Date - min(Date)) / 7) + 1
         ),
         month = factor(month,levels = c("Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May"))
  ) %>% 
  filter(!is.na(month))

dates_heatmap <- dates_heatmap %>% 
  left_join(ref_dates,by = join_by(Date)) %>% 
  mutate(Event = factor(Event,levels = c("Classes Start","Holiday/Break",
                                         "Classes End","Finals")),
         "Term" = case_when(
           month %in% c("Aug","Sep","Oct","Nov","Dec") ~ "Fall",
           month %in% c("Jan","Feb","Mar","Apr","May") ~ "Spring"
         ))

ggplot(dates_heatmap,aes(x = wday,y = week)) + 
  geom_tile(aes(fill = Freq,color = Event,width = 0.9,height = 0.9),linewidth = 1) + # border around days specified by event; set width and height to draw complete borders
  geom_text(aes(label = day)) + 
  theme_minimal() + 
  facet_wrap(~month,scales = "free_y",ncol = 2,dir = "v") + 
  scale_fill_gradient(low = "#E2E6BD",high = "#D33F6A",na.value = "white") + 
  scale_color_manual(values = c("Holiday/Break" = "purple","Classes Start" = "green",
                                "Finals" = "dodgerblue","Classes End" = "red"),
                     na.value = "white", # border around NA values is white
                     na.translate = FALSE) + # removes NA from legend
  scale_x_discrete(position = "top") + 
  scale_y_continuous(transform = "reverse") + 
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face="bold")) + 
  labs(title = "Participation in Campus Pulse surveys", x = "", y = "",
       fill = "Number of Students",color = "") + 
  guides(color = guide_legend(override.aes = list(fill="white"))) # changes fill of color legend to white (otherwise black)



# Heatmap of Survey Start Times
#-------------------------------------------------------------------------------

df_startDates %>% 
  select(`Start Date`) %>% 
  separate_wider_delim(cols = `Start Date`,delim = " ",names = c("Date","Time")) %>% 
  mutate(Date = ymd(Date),
         Time = as.numeric(gsub(":","",Time)),
         dummyTime = cut(Time,breaks = c(0,300,600,900,
                                         1200,1500,1800,2100,2400))) %>% 
  count(dummyTime,Date) %>% 
  ggplot(aes(x = dummyTime,y = Date, fill = n)) + 
  geom_tile() + 
  theme_minimal() + 
  scale_fill_viridis_c() + 
  labs(x = "Time of Day","Date",fill = "Number of participants") + 
  theme(legend.position = "bottom") + 
  scale_y_date(date_breaks = "1 month") + 
  scale_x_discrete(labels = c("03:00","06:00","09:00","12:00",
                              "15:00","18:00","21:00","00:00"))


# A look at participants of both surveys
#-------------------------------------------------------------------------------

dfBoth <- df25 %>% 
  inner_join(df24,by=join_by(`User Id`)) %>% 
  rename_with(~ gsub(pattern = ".x",replacement = " S25",.x,fixed = TRUE)) %>% # reference vignette for dplyr::rename
  rename_with(~ gsub(pattern = ".y",replacement = " F24",.x,fixed = TRUE))

library(eulerr)

vd.counts<-c("Fall" = df24 %>% anti_join(dfBoth,by = join_by(`User Id`)) %>% nrow(),
             "Spring" = df25 %>% anti_join(dfBoth,by = join_by(`User Id`)) %>% nrow(),
             "Fall&Spring" = dfBoth %>% nrow())
plot(
  euler(vd.counts),
  quantities = TRUE,
  fills = c("#ED7D31","#70AD47","#FFC000")
)

# Participants by college
#-------------------------------------------------------------------------------

bind_rows(
  df24 %>% 
    anti_join(dfBoth,by = join_by(`User Id`)) %>% 
    count(College) %>% 
    mutate("Term" = "Fall Only"),
  df25 %>% 
    anti_join(dfBoth,by = join_by(`User Id`)) %>% 
    count(College) %>% 
    mutate("Term" = "Spring Only"),
  dfBoth %>% 
    count(`College S25`) %>% 
    rename("College" = `College S25`) %>% 
    mutate("Term" = "Both")
) %>% 
  mutate(
    College = case_when(
      is.na(College) ~ "NA",
      TRUE ~ College
    ),
    Term = factor(Term,levels = c("Fall Only","Spring Only","Both"))) %>% 
  group_by(Term) %>% 
  mutate("Proportion" = round(n*100/sum(n)),
         "TermAndLabel" = paste(
           Term,
           paste0("(n=",sum(n),")"),
           sep = "\n")) %>% 
  ggplot(aes(x = Proportion,y = reorder(College,Proportion),fill = College)) + 
  geom_col(color = "black") + 
  facet_wrap(~TermAndLabel,nrow = 1) + 
  geom_text(aes(label = paste0(Proportion,"%")),
            position = position_stack(vjust = 0.5)) + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(face = "bold",hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        strip.text = element_text(face = "bold")) +  
  labs(x = "Percentage of Participants",y = "")


# Comparing Fall and Spring Responses
#-------------------------------------------------------------------------------

bind_rows(
  df24 %>% 
    select(contains("Health")) %>% 
    pivot_longer(cols = everything(),
                 names_to = "Question",values_to = "Response") %>% 
    mutate("Term" = "Fall") %>% 
    na.omit() %>% 
    count(Question,Response,Term) %>% 
    group_by(Question) %>% 
    mutate("Proportion" = round(n*100/sum(n))),
  df25 %>% 
    select(contains("Health")) %>% 
    pivot_longer(cols = everything(),
                 names_to = "Question",values_to = "Response") %>% 
    mutate("Term" = "Spring") %>% 
    na.omit() %>% 
    count(Question,Response,Term) %>% 
    group_by(Question) %>% 
    mutate("Proportion" = round(n*100/sum(n)))
) %>% 
  group_by(Question,Term) %>% 
  mutate("TermAndCount" = paste(Term,
                                paste0("(n=",sum(n),")"),sep = "\n"),
         Question = factor(Question,levels = c("Mental Health","Physical Health","Social Health")),
         Response = factor(Response,levels = likert.order.5)) %>% 
  ggplot(aes(x = Response,y = Proportion, fill = Response)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = paste0(Proportion,"%"),vjust = -0.5)) + 
  theme_minimal() + 
  facet_wrap(Question~TermAndCount,scales = "free",dir = "v",nrow = 2) + 
  scale_y_continuous(limits = c(0,50)) + 
  scale_fill_manual(values = rev(paletteer_c("grDevices::Geyser",length(likert.order.5)))) + 
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom") + 
  labs(x = "",
       y = "Percentage of Respondents",
       title = "How did respondents rate their wellness in Fall and Spring?",
       fill = "")


# Tracking Changes
#-------------------------------------------------------------------------------

dfBoth <- dfBoth %>% 
  mutate(
    "mental_coded_char_F24" = case_when(
      `Mental Health F24` == "Very good" ~ "Positive",
      `Mental Health F24` == "Good" ~ "Positive",
      `Mental Health F24` == "Neither good nor bad" ~ "Neutral",
      `Mental Health F24` == "Bad" ~ "Negative",
      `Mental Health F24` == "Very bad" ~ "Negative",
      TRUE ~ NA
    ),
    "physical_coded_char_F24" = case_when(
      `Physical Health F24` == "Very good" ~ "Positive",
      `Physical Health F24` == "Good" ~ "Positive",
      `Physical Health F24` == "Neither good nor bad" ~ "Neutral",
      `Physical Health F24` == "Bad" ~ "Negative",
      `Physical Health F24` == "Very bad" ~ "Negative",
      TRUE ~ NA
    ),
    "social_coded_char_F24" = case_when(
      `Social Health F24` == "Very good" ~ "Positive",
      `Social Health F24` == "Good" ~ "Positive",
      `Social Health F24` == "Neither good nor bad" ~ "Neutral",
      `Social Health F24` == "Bad" ~ "Negative",
      `Social Health F24` == "Very bad" ~ "Negative",
      TRUE ~ NA
    ),
    "mental_coded_char_S25" = case_when(
      `Mental Health S25` == "Very good" ~ "Positive",
      `Mental Health S25` == "Good" ~ "Positive",
      `Mental Health S25` == "Neither good nor bad" ~ "Neutral",
      `Mental Health S25` == "Bad" ~ "Negative",
      `Mental Health S25` == "Very bad" ~ "Negative",
      TRUE ~ NA
    ),
    "physical_coded_char_S25" = case_when(
      `Physical Health S25` == "Very good" ~ "Positive",
      `Physical Health S25` == "Good" ~ "Positive",
      `Physical Health S25` == "Neither good nor bad" ~ "Neutral",
      `Physical Health S25` == "Bad" ~ "Negative",
      `Physical Health S25` == "Very bad" ~ "Negative",
      TRUE ~ NA
    ),
    "social_coded_char_S25" = case_when(
      `Social Health S25` == "Very good" ~ "Positive",
      `Social Health S25` == "Good" ~ "Positive",
      `Social Health S25` == "Neither good nor bad" ~ "Neutral",
      `Social Health S25` == "Bad" ~ "Negative",
      `Social Health S25` == "Very bad" ~ "Negative",
      TRUE ~ NA
    )
  )

library(ggalluvial)

likert.order.3.color<-c("#D37D4AFF","#FBF2C4FF","#4EA093FF")

bind_rows(
  dfBoth %>% 
    filter(!is.na(mental_coded_char_F24) & !is.na(mental_coded_char_S25)) %>% 
    select(`User Id`,mental_coded_char_F24,mental_coded_char_S25) %>% 
    rename("Fall" = mental_coded_char_F24,"Spring" = mental_coded_char_S25) %>% 
    pivot_longer(cols = !`User Id`,names_to = "Term",values_to = "Rating") %>% 
    mutate(Question = "Mental Health"),
  dfBoth %>% 
    filter(!is.na(physical_coded_char_F24) & !is.na(physical_coded_char_S25)) %>% 
    select(`User Id`,physical_coded_char_F24,physical_coded_char_S25) %>% 
    rename("Fall" = physical_coded_char_F24,"Spring" = physical_coded_char_S25) %>% 
    pivot_longer(cols = !`User Id`,names_to = "Term",values_to = "Rating") %>% 
    mutate(Question = "Physical Health"),
  dfBoth %>% 
    filter(!is.na(social_coded_char_F24) & !is.na(social_coded_char_S25)) %>% 
    select(`User Id`,social_coded_char_F24,social_coded_char_S25) %>% 
    rename("Fall" = social_coded_char_F24,"Spring" = social_coded_char_S25) %>% 
    pivot_longer(cols = !`User Id`,names_to = "Term",values_to = "Rating") %>% 
    mutate(Question = "Social Health")
) %>% 
  mutate(Question = factor(Question,levels = c("Mental Health","Physical Health","Social Health"))) %>% 
  ggplot(aes(x = Term,alluvium = `User Id`,stratum = Rating)) + 
  geom_flow(aes(fill = Rating)) + 
  geom_stratum(aes(fill = Rating)) + 
  theme_minimal() + 
  facet_wrap(~Question,nrow = 1) + 
  scale_fill_manual(values = likert.order.3.color) + 
  geom_text(stat = "flow",aes(label = ifelse(after_stat(x == 2),
                                             after_stat(count),
                                             NA))) + 
  geom_text(stat = "stratum",aes(label = ifelse(after_stat(x == 1),
                                                after_stat(count),
                                                NA))) + 
  labs(x = "",y = "Number of Respondents") + 
  theme(legend.position = "right",
        plot.title = element_text(face = "bold",hjust = 0.5),
        axis.text.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

