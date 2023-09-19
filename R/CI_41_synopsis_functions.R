




Cond.Class<-function(df){
  

  
  #High-Level classification of Reef Habitat condition
  cond.crit=data.frame(Class=c("Insuficient data",
                               "Critical","Critical",
                               "Warning II","Warning II","Warning II",
                               "Warning I","Warning I","Warning I",
                               "Watch","Watch","Watch",
                               "Good"),
                       criteria=c(
                         ##No all indicators are calculated
                         'all.na>0',
                         #Critical
                         'Coral.cover=="No" & Recovery.performance=="No" & Processes=="All.No"',
                         'Coral.cover=="No" & Recovery.performance=="No" & Processes=="AtLeastOne.No"',
                         
                         #Warning II
                         'Coral.cover=="No" & Recovery.performance=="No" & Processes=="All.Yes"',
                         'Coral.cover=="No" & Recovery.performance=="Yes" & Processes=="All.No"',
                         'Coral.cover=="Yes" & Recovery.performance=="No" & Processes=="All.No"',
                         
                         #Warning I
                         'Coral.cover=="No" & Recovery.performance=="Yes" & Processes=="AtLeastOne.No"',
                         'Coral.cover=="Yes" & Recovery.performance=="No" & Processes=="AtLeastOne.No"',
                         'Coral.cover=="Yes" & Recovery.performance=="Yes" & Processes=="All.No"',
                         
                         #Watch
                         'Coral.cover=="No" & Recovery.performance=="Yes" & Processes=="All.Yes"',
                         'Coral.cover=="Yes" & Recovery.performance=="Yes" & Processes=="AtLeastOne.No"',
                         'Coral.cover=="Yes" & Recovery.performance=="No" & Processes=="All.Yes"',
                         
                         #Good
                         'Coral.cover=="Yes" & Recovery.performance=="Yes" & Processes=="All.Yes"'
                       )
  )
  
  
  require(tidyverse)
  if (dim(df)[1]==0){
    c.df=NA
  }else{
    ##Agregate indicators for high-level criteria per Reef
    c.df<-df%>%
      group_by(Name,Depth,Year,Indicator)%>%
      # rename(Score=Median)%>%
      mutate(
        crit=case_when(
          ((Indicator %in% c("Community.composition","Macroalgae","Juvenile.density", "Coral.cover")) &
          Upper<=0.5) ~T, ##at or Below threshold for most of the indicators
          ((Indicator == "Recovery.performance") &
             Upper<0.5) ~ T,##[TODO:REview this] Below threshold for Recovery
          is.na(Median) ~NA,
          .default=F
        ),
        Ind.g=case_when(
          Indicator %in% c("Community.composition","Macroalgae","Juvenile.density") ~ "Processes",
          .default=Indicator
        )
      )%>%
      group_by(Name, Depth,Year, Ind.g)%>%
      summarise(crit.no=sum(crit))%>%
      mutate(
      # crit.yes=case_when(
      #   Surveyed==FALSE ~NA,
      #   .default=crit.yes
      # ),
      crit=case_when(
        ((Ind.g =="Coral.cover") & crit.no==1) ~ "No",
        ((Ind.g =="Coral.cover")  &  crit.no==0) ~ "Yes",
        ((Ind.g =="Recovery.performance")  &  crit.no==1) ~ "No",
        ((Ind.g =="Recovery.performance")  &  crit.no==0) ~ "Yes",
        ((Ind.g %in% c("Processes")  &  crit.no==3)) ~ "All.No",
        ((Ind.g %in% c("Processes")  &  crit.no==0)) ~ "All.Yes",
        ((Ind.g %in% c("Processes")  & crit.no %in% (c(1,2)))) ~ "AtLeastOne.No",
        .default = NA)
      )%>%
      select(-crit.no)%>%
      spread(key=Ind.g, val=crit)%>%
      mutate(all.na=sum(c(is.na(Coral.cover),is.na(Recovery.performance),is.na(Processes))))
    
    c.df$Class=apply(
      do.call(rbind, 
              Map(function(x, y) with(c.df, ifelse(eval(parse(text = x)), y, NA)), 
                  cond.crit$criteria, cond.crit$Class)), 2, function(x) toString(x[!is.na(x)]))
    

  }
  
  return(c.df)
}
 