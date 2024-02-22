#### FUNCTIONS ####

add.n <- function(factor){
  factor <- as.factor(factor)
  if(length(grep("n = ", levels(factor))) >1){}else{
    for(i in 1:length(levels(factor))){
      levels(factor)[i] <- paste0(levels(factor)[i], " (n = ", table(factor)[i], ")")
    }
  }
  return(factor)
}

add.n.cleanpoints <- function(factor){
  #Substituting point in names
  colnames(factor) <- gsub("\\.+"," ",colnames(factor))
  #Adding total n
  for(i in 2:length(colnames(factor))){
    colnames(factor)[i] <- paste0(colnames(factor)[i]," (n = ",sum(factor[,i]),")")
  }
  return(factor)
}

#Generates a table with Chi Square test between levels of a given factor for each of the categories of another factor
chi.table<- function(Xfactor, Yfactor){
  #Xfactor:
  #The independent variable to compare in the Chi.square (ie. cases$Multinationals or cases$State)
  #Yfactor:
  #The dependent variable to compare (ie. cases$Project.Status)
  Xfactor <- as.factor(Xfactor)
  if(is.list(Yfactor)){
    Yfactor$compile <- rep(NA,length.out = nrow(Yfactor)) 
    for(i in 1:nrow(Yfactor)){
      compilation <- NULL
      for(j in 2:(which(colnames(Yfactor) %in% colnames(Yfactor)[ncol(Yfactor)-1])-1)){
        if(Yfactor[i,j] == 1|Yfactor[i,j] == "V"){
          compilation <- c(compilation,colnames(Yfactor)[j]) 
        }else{}
      }
      Yfactor$compile[i] <- paste(compilation, collapse = "\n")
    }
    Ytrans <- Yfactor[which(Xfactor == levels(Xfactor)[1]),]
    Ytrans <- as.data.frame(table(unlist(strsplit(Ytrans$compile,"\n"))))
    heading <- c("factor",levels(Xfactor)[1])
    for(i in 2:nlevels(Xfactor)){
      Ytrans1 <- Yfactor$compile[which(Xfactor == levels(Xfactor)[i])]
      Ytrans1 <- as.data.frame(table(unlist(strsplit(Ytrans1,"\n"))))
      for(j in 1:nrow(Ytrans)){
        Ytrans$Freq2[j] <- if(Ytrans$Var1[j] %in% Ytrans1$Var1)
          {Ytrans1$Freq[which(Ytrans1$Var1 %in% Ytrans$Var1[j])]}else
          {0}
      }
      heading <- c(heading, levels(Xfactor)[i])
    }
    #Add total n for each outcome
    levels(Ytrans$Var1) <- paste0(levels(Ytrans$Var1)," (n = ",table(unlist(strsplit(Yfactor$compile,"\n"))),")")
    colnames(Ytrans) <- heading
    chitable <- Ytrans
    
  }else{
    if(length(grep("\n",Yfactor)) > 0){
      Ytrans <- Yfactor[which(Xfactor == levels(Xfactor)[1])]
      Ytrans <- as.data.frame(table(unlist(strsplit(Ytrans,"\n"))))
      Ytrans$XFactor <- levels(Xfactor)[1] 
      heading <- c("factor",paste(levels(Xfactor)[1]))
      for(i in 2:nlevels(Xfactor)){
        Ytrans1 <- Yfactor[which(Xfactor == levels(Xfactor)[i])]
        Ytrans1 <- as.data.frame(table(unlist(strsplit(Ytrans1,"\n"))))
        Ytrans1$XFactor <- levels(Xfactor)[i]
        Ytrans <- rbind(Ytrans,Ytrans1)
        heading <- c(heading,paste(levels(Xfactor)[i]))
      }
      Xfac <- unique(Ytrans$Var1)
      data <- NULL
      XFactor <- NULL
      for(i in 1:length(Xfac)){
        num<-NULL
        for(j in 1:nlevels(Xfactor)){
          if(length(which((Ytrans$XFactor %in% levels(Xfactor)[j]) & (Ytrans$Var1 %in% levels(Xfac)[i])))>0){
            num1 <- Ytrans$Freq[which((Ytrans$XFactor %in% levels(Xfactor)[j]) & (Ytrans$Var1 %in% levels(Xfac)[i]))]
          } else {num1 <- 0}
          num <- c(num,num1)
          XFactor <- c(XFactor, levels(Xfactor)[j])
        }
        data <- rbind(data,num)
      }
      YFactor <- Xfac
      Ytrans <- cbind.data.frame(YFactor,data)
      #Add total n for each outcome
      for(i in 1:nlevels(Ytrans$YFactor)){
        levels(Ytrans$YFactor)[i] <- paste0(levels(Ytrans$YFactor)[i]," (n = ",sum(Ytrans[i,2:ncol(Ytrans)]),")")
      }
      colnames(Ytrans) <- heading
      chitable <- Ytrans
    }else{Yfactor <- as.factor(Yfactor)
    Yfactor <- add.n(Yfactor)
    chitable <- as.data.frame(levels(Yfactor))
    heading <- "factor"
    #deparse(substitute(Xfactor)) 
    for(i in 1:nlevels(Xfactor)){
      chitable <- cbind.data.frame(chitable,as.integer(table(Yfactor[which(Xfactor %in% levels(Xfactor)[i])])))
      heading <- c(heading, levels(Xfactor)[i])
    }
    colnames(chitable) <- heading}}
  
  chitable$Ratio <- as.numeric(rep(NA,length.out = nrow(chitable)))
  
  for(i in 1:nrow(chitable)){
    chitable$Ratio[i] <- chitable[i,2]/sum(chitable[i,3:ncol(chitable)],na.rm =T)
  }
  
  chisq <- chisq.test(chitable[,c(-1,-ncol(chitable))])
  chitable <- cbind(chitable,chisq$residuals)
  
  residuals <- rep(NA,length.out = nrow(chitable))
  for(i in 1:nlevels(Xfactor)){
    res<-NULL
    for(j in 1:nrow(chitable)){
      res <- c(res,as.numeric(table(Xfactor)[i]) - chitable[j,i+1])
    }
    residuals <- cbind.data.frame(residuals,res)
  }
  residuals <- residuals[,-1]
  
  chitable$pvalue <- rep(NA,length.out = nrow(chitable))
  
  for(i in 1:nrow(chitable)){
    Pretest <- NULL
    for(j in 1:nlevels(Xfactor)){
      Sample <- as.numeric(c(chitable[i,j+1],residuals[i,j]))
      Pretest <- rbind.data.frame(Pretest,Sample)
    }
    Test <- chisq.test(Pretest)
    chitable$pvalue[i] <- Test$p.value
  }
  expected <- chisq$expected
  colnames(expected) <- gsub("\\b$"," \\(expected\\)",colnames(expected))
  chitable <- cbind(chitable,expected)
  relative_abundance <- chitable[,1:nlevels(Xfactor)+1]/chitable[,(ncol(chitable)-nlevels(Xfactor)+1):ncol(chitable)]
  colnames(relative_abundance) <- gsub("\\b$"," \\(relative_abundance\\)",colnames(relative_abundance))
  chitable <- cbind(chitable,relative_abundance)
  chitable <- chitable[order(chitable$Ratio, decreasing = TRUE), ]
  return(chitable)
}

#Plots the result of the chi.table function
chi.plot <- function(chitable,Xfactors, style = "proportion"){
  Xfactors <- as.factor(Xfactors)
  total <- length(Xfactors) 
  #Xfactors <- Xfactors[-which(Xfactors %in% levels(Xfactors)[nlevels(Xfactors)])]
  chitable$Yfactor <- factor(chitable$factor, levels = c(as.character(chitable$factor)))
  data <- NULL
  Xfactor <- NULL
  yintercept <- NULL
  a <- 0
  #n <- (ncol(chitable)-4)/2
  n <- nlevels(Xfactors)
  for(i in 1:n){
    data <- c(data,chitable[,i+1])
    Xfactor <- c(Xfactor,rep(colnames(chitable)[i+1],nlevels(chitable$Yfactor)))
    a <- a+table(Xfactors)[i]
    yintercept <- c(yintercept,a/total)
  }
    yintercept <- 1-yintercept
    Yfactor <- rep(chitable$Yfactor,n)
    chitable_data <- cbind.data.frame(Yfactor,data)
    chitable_data$Xfactor <- Xfactor
    significant <- NULL
    for(i in 1:nrow(chitable)){
      significant <- c(significant,if(chitable$pvalue[i] > 0.05) {"no"} else {"yes"})
    }
    chitable_data$significant <- significant
    
    chiplot<-ggplot(chitable_data, aes(x = Yfactor, y = data, fill=Xfactor)) +
      geom_col(position="fill",) +
      labs(x = NULL, y = NULL)  +
      theme(legend.position = "bottom") +
      scale_fill_manual(name = paste(Xfactors),values=c("#1e818b","#e2c537","#F6EB13","#7A752F","#734C20","#111111","#9B989B","#BE85BA","#3BB449","#ED2224","#AF3456"))+
      coord_flip()+
      gghighlight(chitable_data$significant %in% "yes",
                  unhighlighted_params =  list(fill = NULL, alpha = 0.5))+
      geom_hline(yintercept = yintercept, linewidth=0.5)
  
  return(chiplot)
}

EU.plot <- function(Actors){
  Actors$Freq <- Actors$Yes/length(cases$Case[which(cases$France %in% "Yes")]) 
  Actors$FreqEU <- Actors$No/length(cases$Case[which(cases$France %in% "No")]) 
  preplot <- cbind.data.frame(rep(as.character(Actors$factor), times = 2),
                              as.factor(c(rep("No",times = nrow(Actors)),rep("Yes",times = nrow(Actors)))))
  colnames(preplot)[1] <- "factor"
  colnames(preplot)[2] <- "France"
  preplot$Freq <- c(Actors$FreqEU,Actors$Freq)
  preplot$factor <- as.factor(preplot$factor)
  Actors_plot <- ggplot(preplot,aes(x = preplot$factor, y = preplot$Freq, fill = preplot$France))+
    geom_bar(position="dodge", stat = "identity")+
    coord_flip()
  return(Actors_plot)
}

save.plot <- function(chiplot){
  ggsave(chiplot, filename = paste0(deparse(substitute(chiplot)),".svg"), width = 7, height = (1+nlevels(chiplot$data$Yfactor)/5), dpi = 300, units = "in")
}

#### LIBRARIES ####
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(gghighlight)
#library(data.table)
#library(remotes)
library(svglite)

#### 1. Import and prepare data ####

setwd("C:/Users/Usuario/Desktop/Marcel/EJAtlas/Writings/Local Leadership France/Data")
cases<-read.csv("cases.csv", sep = ",")
companys<-read.csv("companys.csv", sep = ",")
company_conflicts<-read.csv("company_conflicts.csv", sep = ",")
conflict_companys<-read.csv("conflict_companys.csv", sep = ",")
mobilizinggroups <- read.csv("mobilizinggroups.csv", sep = ",")
mobilizingforms <- read.csv("mobilizingforms.csv", sep = ",")
envimpacts <- read.csv("envimpacts.csv", sep = ",")
hltimpacts <- read.csv("hltimpacts.csv", sep = ",")
secimpacts <- read.csv("secimpacts.csv", sep = ",")
Gabriel <- read.csv("GabrielData.csv", sep = ",")
Summary <- read.csv("Summary.csv", sep = ",")

#### 2. Select French and EU cases ####

cases <- cases[which(cases$Country %in% c(
  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic",
  "Denmark","Estonia","Finland","France","Germany","Greece", "Hungary",
  "Ireland","Italy","Latvia","Lithuania","Malta","Netherlands","Poland",
  "Portugal","Romania","Slovak Republic","Slovenia","Spain","Sweden")),]
cases$France <- rep("No",length.out= nrow(cases))
cases$France[which(cases$Country %in% "France")] <- "Yes"
table(cases$France)
write.csv(cases,"casesEU+France.csv")

envimpacts <- envimpacts[which(envimpacts$id %in% cases$Conflict.Id),]
hltimpacts <- hltimpacts[which(hltimpacts$id %in% cases$Conflict.Id),]
secimpacts <- secimpacts[which(secimpacts$id %in% cases$Conflict.Id),]
mobilizingforms <- mobilizingforms[which(mobilizingforms$id %in% cases$Conflict.Id),]
mobilizinggroups <- mobilizinggroups[which(mobilizinggroups$id %in% cases$Conflict.Id),]
company_conflicts <- company_conflicts[which(company_conflicts$conflict_id %in% cases$Conflict.Id),]

mobilizinggroups$France <- cases$France
mobilizingforms$France <- cases$France
envimpacts$France <- cases$France
hltimpacts$France <- cases$France
secimpacts$France <- cases$France

#### 3. Categorise depending on the participation of local actors ####

cases$LocalActors <- rep("No",length.out= nrow(cases))
for(i in 1:nrow(cases)){
  #if(mobilizinggroups$Local.ejos[i] == 1){cases$LocalActors[i] <- "Yes"} 
  #if(mobilizinggroups$Local.government.political.parties[i] == 1){cases$LocalActors[i] <- "Yes"} 
  if(mobilizinggroups$Neighbours.citizens.communities[i] == 1){cases$LocalActors[i] <- "Yes"} 
}
table(cases$LocalActors)

mobilizinggroups$LocalActors <- cases$LocalActors
mobilizingforms$LocalActors <- cases$LocalActors
envimpacts$LocalActors <- cases$LocalActors
hltimpacts$LocalActors <- cases$LocalActors
secimpacts$LocalActors <- cases$LocalActors

mobilizinggroups$LocalActors <- as.factor(mobilizinggroups$LocalActors)
Actors<-chi.table(mobilizinggroups$LocalActors,mobilizinggroups)
Actors_plot <- chi.plot(Actors,mobilizinggroups$LocalActors)

France <- cases[cases$France %in% "Yes",]
table(France$LocalActors)

France <- cbind.data.frame(France,mobilizingforms[which(mobilizingforms$id %in% France$Conflict.Id),])
France <- cbind.data.frame(France,mobilizinggroups[which(mobilizinggroups$id %in% France$Conflict.Id),])

France$First.level.category <- as.factor(France$First.level.category)
France$Project.Status <- as.factor(France$Project.Status)
glm <- glm(Project.Status ~ Local.government.political.parties + Local.ejos + Neighbours.citizens.communities, France,  family = binomial(link = "logit"))

summary(glm)

Status <- chi.table(France$Local.government.political.parties,France$Project.Status)
ChiTab <- chi.table(secimpacts$Local.government.political.parties,
                    secimpacts)

secimpacts$Local.government.political.parties <- France$Local.government.political.parties

#
#### 4. Descriptive France vs EU ####

mobilizinggroups$France <- as.factor(mobilizinggroups$France)
mobilizingforms$France <- as.factor(mobilizingforms$France)

Actors_EU <- chi.table(mobilizinggroups$France,mobilizinggroups)
Actors_EU_plot <- EU.plot(Actors_EU)

ggsave("Actors_EU_plot.svg",Actors_EU_plot,device="svg")

Category_EU <- chi.table(cases$France,cases$First.level.category)
Category_EU$factor <- gsub("(Forests, Agriculture, Fisheries and Livestock Management)","",Category_EU$factor)
Category_EU_plot <- EU.plot(Category_EU)

ggsave("Category_EU_plot.svg",Category_EU_plot,device="svg")

Type_EU <- chi.table(cases$France,cases$Second.level.type)
Type_EU_plot <- EU.plot(Type_EU)

Forms_EU <- chi.table(mobilizingforms$France,mobilizingforms)
Forms_EU_plot <- EU.plot(Forms_EU)

ggsave("Forms_EU_plot.svg",Forms_EU_plot, device = "svg")

Rural_EU <- chi.table(cases$France,cases$Type.of.Population)
Rural_EU_plot <- EU.plot(Rural_EU)

Loc_EU <- chi.table(cases$France, cases$Accuracy.of.location)
Loc_EU_plot <- EU.plot(Loc_EU)

ggsave("Loc_EU_plot.svg",Loc_EU_plot, device = "svg")

Intensity_EU <- chi.table(cases$France,cases$Intensity.of.Conflict)
Intensity_EU_plot <- EU.plot(Intensity_EU)

Prev_EU <- chi.table(cases$France,cases$When.did.the.mobilization.begin)
Prev_EU_plot <- EU.plot(Prev_EU) 

ggsave("Prev_EU_plot.svg",Prev_EU_plot, device = "svg")

Status_EU <- chi.table(cases$France,cases$Project.Status)
Status_EU_plot <- EU.plot(Status_EU)

ggsave("Status_EU_plot.svg",Status_EU_plot, device = "svg")

Outcomes_EU <- chi.table(cases$France,cases$Outcomes)
Outcomes_EU_plot <- EU.plot(Outcomes_EU)

EJ_EU <- chi.table(cases$France,cases$EJ.served.or.not)
EJ_EU_plot <- EU.plot(EJ_EU)

#### 5. Comparison local leadership ####
Actors_plot

cases <- cbind.data.frame(cases,mobilizinggroups,mobilizingforms)

for(i in 1:nrow(cases)){
  cases$success[i] <- if(cases$EJ.served.or.not[i] %in% "Success"){"Yes"}else{"No"}
}
table(cases$success)
cases$success <- as.factor(cases$success)

cases$Local.ejos <- as.factor(cases$Local.ejos)
cases$Local.government.political.parties <- as.factor(cases$Local.government.political.parties) 
EJ_EJOS <- chi.table(cases$Local.ejos,cases$EJ.served.or.not)

#cases <- cases[which(cases$France %in% "Yes"),]
Outcome_EJOS <- chi.table(cases$Local.ejos,cases$Outcomes) 
Outcome_parties <-chi.table(cases$Local.government.political.parties,cases$Outcomes)
Outcome_citi <- chi.table(cases$Neighbours.citizens.communities,cases$Outcomes)

Actors_EJOS <- chi.table(cases$Local.ejos,mobilizinggroups) 
Actors_parties <-chi.table(cases$Local.government.political.parties,mobilizinggroups)
Actors_citi <- chi.table(cases$Neighbours.citizens.communities,mobilizinggroups)
Actors_EJOS_plot <- chi.plot(Actors_EJOS,cases$Local.ejos)
Actors_parties_plot <- chi.plot(Actors_parties,cases$Local.government.political.parties)

Status_EJOS <- chi.table(cases$Local.ejos,cases$Project.Status) 
Status_parties <-chi.table(cases$Local.government.political.parties,cases$Project.Status)
Status_citi <- chi.table(cases$Neighbours.citizens.communities,cases$Project.Status)
Status_EJOS_plot <- chi.plot(Status_EJOS,cases$Local.ejos)
Status_parties_plot <- chi.plot(Status_parties,cases$Local.government.political.parties)

#### 32. Test whether project status and outcome has correlation with category ####

Status_Category<-chi.table(France$Project.Status,France$First.level.category)
Status_Category<-chi.table(France$First.level.category,France$Outcomes)

#### 14. Assign category to each company ####

#The most frequent conflict category is assigned as the company category
Summary$Category <- as.numeric(rep(NA,nrow(Summary)))
for(i in 1:nrow(Summary)){
  Summary$Category[i] <-names(sort(
    table(cases$First.level.category[which(cases$Conflict.Id %in% c(unlist(strsplit(Summary$conflictIDs[i], " "))))])
    ,decreasing = T))[1]
}

#### 15. Assign level reported to each company ####

Summary$Reported <- as.numeric(rep(NA,nrow(Summary)))
for(i in 1:nrow(Summary)){
  Summary$Reported[i] <- if(Summary$Num_cases[i] >30){
    "More than 30 cases"
  } else if(Summary$Num_cases[i] >7){
    "8-30 cases"
  } else if(Summary$Num_cases[i] >4){
    "5-7 cases"
  } else if(Summary$Num_cases[i] > 1){
    "2-4 cases"
  } else if(Summary$Num_cases[i] > 0){
    "One case"
  } 
}

Summary$Reported <- as.factor(Summary$Reported)
levels(Summary$Reported)
Summary$Reported <- factor(Summary$Reported, levels = c("One case", "2-4 cases", "5-7 cases", "8-30 cases","More than 30 cases"))
sort(table(Summary$Reported), decreasing =T)

#### 16. Assign country and region of origin to each company ####

#Counts the number of conflicts for each original company entry
conflict_companys$Num_Cases <- as.numeric(rep(NA,nrow(conflict_companys)))
for(i in 1:nrow(conflict_companys)){
  conflict_companys$Num_Cases[i] <- ncol(conflict_companys[i,which(!is.na(conflict_companys[i,]))])-1
}

#Assign the country of origin most frequently reported in all company entries related cases 
#Watch out! Summary$country (in lower-case) is the original country register of the entry!!
Summary$Country <- as.numeric(rep(NA,nrow(Summary)))
for(i in 1:nrow(Summary)){
  Countries <- companys$country[which(companys$id %in% c(unlist(strsplit(Summary$companyIDs[i], " "))))]
  Num_Cases <- conflict_companys$Num_Cases[which(conflict_companys$company_id %in% c(unlist(strsplit(Summary$companyIDs[i], " "))))]
  a<-cbind.data.frame(Countries,Num_Cases)
  a<- a[which(!(a$Countries %in% "")),]
  b<-a %>% group_by(Countries) %>% summarize(sum(Num_Cases))
  Summary$Country[i] <- if(nrow(a) == 0) {"ND"} else {b$Countries[which(b$`sum(Num_Cases)` %in% max(b$`sum(Num_Cases)`))]}
}
rm(a,b,Countries,Num_Cases)
Summary$Country<-gsub("NA","ND",Summary$Country)

#### 17. Identify multinational companies ####

#If a company does not have a country of origin assigned "ND", it cannot be established whether it is a multinational
Summary$Multinational <- as.numeric(rep(NA,nrow(Summary)))
for(i in 1:nrow(Summary)){
  Summary$Multinational[i] <- if(Summary$Country[i] %in% "ND") {"ND"} else {
    if("FALSE" %in% as.character(cases$Country[which(cases$Conflict.Id %in% c(unlist(strsplit(Summary$conflictIDs[i], " "))))] %in% Summary$Country[i])) 
    {"yes"} else {"no"}
  }
}

#### 6. Company ranking ####

for(i in 1:nrow(Summary)){
  a <- unlist(strsplit(Summary$conflictIDs[i], " "))
  a <- a[which(a %in% cases$Conflict.Id[which(cases$France %in% "Yes")])]
  Summary$cases_france[i] <- paste(a, collapse = " ")
  Summary$num_cases_france[i] <- length(unlist(strsplit(Summary$cases_france[i], " ")))
}

table(Summary$num_cases_france)
France_companies <- Summary[-which(Summary$num_cases_france == 0),]

write.csv(France_companies,"France_companies.csv")
table(France_companies$Multinational)
France_companies<-read.csv("France_companies.csv")
#### 34. French cases stopped or cancelled ####
table(France$Project.Status)
France$cancelled <- as.character(rep("No",nrow(France)))
France$cancelled[grep("cancelled",France$Outcomes)] <- "Yes"
table(France$cancelled)
write.csv(France[grep("Stopped",France$Project.Status),],"France_stopped.csv")
France_stopped<-read.csv("France_stopped.csv")
