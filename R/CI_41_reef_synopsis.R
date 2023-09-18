#auto text for regional summaries of Indicators

# summaries assume multiple depths at reefs are independent surveys

# general concept is to create sentences for each indicator that consider the primarily instances where distance from baseline scores deviate below 0.5
# within regions deviations below critical thresholds are also considered
# When combine these sentences provide a brief summary that expands on the overall classification of coral community condition.

##for development 
# setwd("C:\\Users/mgonzale/OneDrive - Australian Institute of Marine Science/projects/RIMReP/Habitat_Indicators/")
# input=list(region_select="GBRMPA.MA",
# report_year=2017,
# value_select="Cairns/Cooktown Management Area")
# load("outputs/scores.RData")
# i.df<-scores%>%
#   filter(Name==input$value_select)%>%
#   droplevels()

# Angus testing


#' Format character vector into HTML bulleted list
#' 
#' @param char a character vector. Each element will be a bullet
#' @param ordered logical (T/F). If `TRUE`, return numbered list.
#' 
#' @keywords internal
format_html_list <- function(char, ordered = FALSE){
  
  seps <- c("<li>", "</li>")
  html_wrapper <-  if(ordered) c("<ol>", "</ol>") else c("<ul>", "</ul>")
  
  bullets <- paste0(seps[1], char, seps[2], collapse = "")
  
  html_list <- paste0(html_wrapper[1], bullets, html_wrapper[2])
  
  return(html_list)
}


sum.reef.tx<-function(i.df, y){
  require(tidyverse)
  source("scripts/Misc/HighLevel_Classification.R")

  
  a.txt<-read_csv(file = "scripts/Misc/Synoptic_text_reef.csv")
  taxaLookup<-read.csv(file="scripts/Misc/taxaLookup.csv")
  
  #************************************
  # Overall classification of condition
  #************************************
  
  Cl<-i.df%>%filter(Year==y, Reference=="Baseline")%>%Cond.Class()%>%pull(Class)
  
  Autotext.class="In <b>%s</b>, the overal condition reef habitats was classified as <b>%s</b>." 
  # sent.class=
  #   sprintf(Autotext.class,
  #           y,
  #           Cl)
  sent.class=
    sprintf(paste(
      a.txt%>%filter(Variable=="Autotext.class")%>%pull(Description), ##overall statement
      a.txt%>%filter(Variable==Cl)%>%pull(Description)),
      y,
      Cl)
  
  # #**************************************
  # ## within function testing data input
  # #**************************************
  # a.txt<-read_csv(file = "Synoptic_text_reef.csv")
  # indices<-read.csv(file="Indices.csv")
  # 
  # i.df<-indices%>%
  #   filter(Name=="Double Cone" & Depth=="shallow slope")
  # 
  # taxaLookup<-read.csv(file="taxaLookup.csv")
  # 
  # c.df<-read.csv(file="Composition_change.csv") %>%
  #   filter(REEF=="Double Cone" & DEPTH.f=="shallow slope") %>%
  #   rename(Name=REEF,Depth=DEPTH.f, Year=REPORT_YEAR) %>%
  #   group_by(Name,Depth,Year,k) %>%
  #   arrange(desc(abs(meanDiff))) %>%
  #   slice(1:3) %>%
  #   ungroup %>%
  #   mutate(Change=ifelse(meanDiff>0,"Increase","Decrease")) %>%
  #   left_join(taxaLookup)
  # 
  # y=2022
  
  #***********************
  # Hard coral cover #####
  #***********************
  
  data<-i.df %>% 
    filter(Indicator=="Coral.cover" & Reference=="Baseline") %>% 
    droplevels() %>%
    arrange(Year) %>%
    mutate(Low=ifelse(Upper<0.5,1,0),
           runID.l=data.table::rleid(Low))
  
  data.hc.c<-i.df %>% 
    filter(Indicator=="Coral.cover" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  start.run<-as.character(data %>% 
                            filter(runID.l==max(runID.l)) %>%
                            summarise(runStart=first(Year)))
  
  low.years<-nrow(data %>% filter(runID.l==max(runID.l) & Low=='1'))
  if(low.years>0){
    first.low<-data %>%
    filter(Year<y,Low==1)%>%
    summarise(first.low=min(Year))%>%
    pull(first.low)
  }
  
  data.hc<-data %>%
    filter(Year==y)
  
  Low.hc<-data.hc$Upper<0.5
  Low.hc.c<-data.hc.c$Upper<0.5
  
  State.hc<-data.hc %>%
    mutate(St=ifelse(Upper<0.5, "below",
                     ifelse(Lower>0.5, "above", "similar to"))) %>%
    pull(St)
  
  State.hc.c<-data.hc.c %>%
    mutate(St=ifelse(Upper<0.5, "unlikely", "likely")) %>%
    pull(St)
  
  
  ## Cover of hard corals  declined to below historical reference levels
  # Autotext1="The cover of hard corals has declined to below historical reference levels. At current levels hard corals are %s to be supporting positive reef accretion."
  ## Cover of hard corals below historical reference levels for a while
  # Autotext2="The cover of hard corals has been below historical reference levels since %s. At current levels hard corals are %s to be supporting positive reef accretion."
  ## Cover of hard corals at or above historical reference levels for a while
  # Autotext3="The cover of hard corals is %s historical reference levels. At current levels hard corals are %s to be supporting positive reef accretion."
  
  
  sent.hc= ifelse(low.years==1,
                  sprintf(a.txt%>%filter(Variable=="Autotext1")%>% pull(Description),
                  State.hc.c),
           ifelse(low.years>1,
                  sprintf(a.txt%>%filter(Variable=="Autotext2")%>%pull(Description),
                          first.low,
                          State.hc.c),
                  sprintf(a.txt%>%filter(Variable=="Autotext3")%>%pull(Description),
                                 State.hc,
                                 State.hc.c)
                  )
           )
 
  if(is.na(data.hc$Upper)){sent.hc=NULL}
  #*********************************#
  # Recovery.performance Indicator  ##########
  #*********************************#
  
  data.p<-i.df %>% 
    filter(Indicator=="Recovery.performance" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.p.c<-i.df %>% 
    filter(Indicator=="Recovery.performance" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.pe<-data.p$Upper<0.5
  Low.pe.c<-data.p.c$Upper<0.5
  
  Concern<-ifelse(isFALSE(Low.hc), "concern","additional concern")
  
  # Indi.desc.pe<-as.character(Indi %>% 
  #                              filter(Indicator=="Recovery.performance") %>% 
  #                              pull(Ind.desc))
  ##  Separate sentences for Regional and reef level summary to allow combination as appropriate
  
  ## Reef Recovery.performance baseline low
  # Autotext4="Of %s is that during the most recent recovery period, the rate of increase in hard coral cover was lower than expected for a reef in this bioregion."
  # 
  ## Reef Recovery.performance baseline and critical both low, 
  # Autotext5="Of %s is that during the most recent recovery period, the rate of increase in hard coral cover was lower than expected for a reef in this bioregion and declining."
  # 
  # # Reef Recovery.performance at expectations, HC at or above 
  # Autotext6="In addition to hard coral cover being %s historical reference levels, during the most recent recovery period coral cover increased at a rate expected for a reef in this bioregion."
  # 
  # # Reef Recovery.performance at expectations, but slowing, HC at or above 
  # Autotext7="In addition to hard coral cover being %s historical reference levels, during the most recent recovery period coral cover increased at a rate expected for a reef in this bioregion, but has shown a recent decline."
  # 
  # # Reef Recovery.performance at expectations, HC low
  # Autotext8="Encouragingly, during the most recent recovery period coral cover increased at a rate expected for a reef in this bioregion."
  # 
  # Autotext9="An assessment of the rate of recovery of hard coral cover for this reef could not be made."
  
  
  sent.pe=
    ifelse(isTRUE(Low.pe) & !isTRUE(Low.pe.c),
           sprintf(a.txt%>%filter(Variable=="Autotext4")%>%pull(Description),
                   Concern),
           ifelse(isTRUE(Low.pe) & isTRUE(Low.pe.c),
                  sprintf(a.txt%>%filter(Variable=="Autotext5")%>%pull(Description),
                          Concern),
                  ifelse(isFALSE(Low.pe) & isFALSE(Low.hc) & !isTRUE(Low.pe.c),
                         sprintf(a.txt%>%filter(Variable=="Autotext6")%>%pull(Description),
                                 State.hc),
                         ifelse(isFALSE(Low.pe) & isFALSE(Low.hc) & isTRUE(Low.pe.c),
                                sprintf(a.txt%>%filter(Variable=="Autotext7")%>%pull(Description),
                                        State.hc),
                                if(isFALSE(Low.pe) & isTRUE(Low.hc) & !isTRUE(Low.pe.c)){
                                  sprintf(a.txt%>%filter(Variable=="Autotext8")%>%pull(Description),
                                          State.hc)}
                                else{a.txt%>%filter(Variable=="Autotext9")%>%pull(Description)})
                  )
                  )
           )
  
  if(is.na(data.p$Upper)){sent.pe=NULL}
  
  #********************##
  # MA and Juveniles ####
  #*******************###    
  #Ma data
  data.m<-i.df %>% 
    filter(Indicator=="Macroalgae" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.m.c<-i.df %>% 
    filter(Indicator=="Macroalgae" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.m<-data.m$Upper<0.5
  Low.m.c<-data.m.c$Upper<0.5
  
  State.ma<-data.m %>%
    mutate(St=ifelse(Upper<0.5, "above",
                     ifelse(Lower>0.5, "below", "similar to"))) %>%
    pull(St)
  
  
  Conseq.ma<-ifelse(isFALSE(Low.m.c), "likely","unlikely")
  
  # Indi.desc.m<-as.character(Indi %>% 
  #                             filter(Indicator=="Macroalgae") %>% 
  #                             pull(Ind.desc))
  # 

  #JUv data
  data.j<-i.df%>% 
    filter(Indicator=="Juvenile.density" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.j.c<-i.df %>% 
    filter(Indicator=="Juvenile.density" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.j<-data.j$Upper<0.5
  Low.j.c<-data.j.c$Upper<0.5
  
  State.j<-data.j %>%
    mutate(St=ifelse(Upper<0.5, "below",
                     ifelse(Lower>0.5, "above", "similar to"))) %>%
    pull(St)
  
  Concern.j.low<-ifelse(isTRUE(Low.j.c), "Of added concern is that","However")
  Concern.j.high<-ifelse(isTRUE(Low.j.c),"However", "In addition")
  Conseq.j.low<-ifelse(isTRUE(Low.j.c), "also below the threshold that should facilitate","adequate to promote")
  Conseq.j.high<-ifelse(isTRUE(Low.j.c), "below the threshold that should facilitate","adequate to promote")
  
    
  # Autotext10="The density of juvenile corals was %s historical reference levels." 
  #
  # Autotext11="%s the density of <i>Acropora</i> juveniles were %s the timely recovery of coral cover." 
  # 
  # Autotext12="The representation of macroalgae species within the benthic algal communities was %s historical reference levels. At current levels macroalgae are %s to be limiting coral community resilience."
  # 
 
  sent.j.state=sprintf(a.txt%>%filter(Variable=="Autotext10")%>%pull(Description),
                       State.j)
    
  sent.j.c= ifelse(isTRUE(Low.j),
           sprintf(a.txt%>%filter(Variable=="Autotext11")%>%pull(Description),
                   Concern.j.low,
                   Conseq.j.low),
          sprintf(a.txt%>%filter(Variable=="Autotext11")%>%pull(Description),
                  Concern.j.high,
                  Conseq.j.high) )
  
  sent.ma=sprintf(a.txt%>%filter(Variable=="Autotext12")%>%pull(Description),
                  State.ma,
                  Conseq.ma)
  
  if(is.na(data.j$Upper)){sent.j.state=NULL}
  if(is.na(data.j$Upper)){sent.j.c=NULL}
  if(is.na(data.m$Upper)){sent.ma=NULL}
  
  #************#####
  # Composition#####
  #************#####
  #comp data
  data.co<-i.df %>% 
    filter(Indicator=="Community.composition" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.co.low<-i.df%>%
    filter(Indicator=="Community.composition" & Reference=="Baseline") %>% 
    droplevels() %>%
    arrange(Year) %>%
    mutate(Low=ifelse(Upper<0.5,1,0),
           runID.l=data.table::rleid(Low))
  
  start.run.co<-as.character(data.co.low %>% 
                               filter(runID.l==max(runID.l)) %>%
                               summarise(runStart=first(Year)))
  
  low.years.co<-nrow(data.co.low %>% filter(runID.l==max(runID.l) & Low=='1'))
  
  data.co.c<-i.df %>% 
    filter(Indicator=="Community.composition" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.co<-data.co$Upper<0.5
  Low.co.c<-data.co.c$Upper<0.5
  
  
  # # comp low current year only - hc ok. 
  # Autotext13="Despite the cover of hard corals remaining at or above reference levels, there is evidence that the composition of communities have changed."
  # 
  # # comp low >1 year - hc ok. 
  # Autotext14="Despite the cover of hard corals remaining at or above reference levels, since %s the composition of communities have remained distinct from those historically observed."
  # 
  # # comp ok - hc ok, 
  # Autotext15="In addition to the cover of hard corals remaining within reference levels there is no evidence of a substantial change in the composition of coral communities."
  # 
  # # comp low - hc low. current year
  # Autotext16="In addition to the low cover of hard corals, the community composition has changed compared to the historical reference."
  # 
  # # comp low >1 year - hc low.
  # Autotext17="In addition to the low cover of hard corals, the composition of communities have remained distinct from those historically observed since %s."
  # 
  # # comp ok - hc low
  # Autotext18="Although the cover of hard corals is low, there is no evidence of a substantial change in the composition of coral communities."
  # 
  
  sent.co<-                                           
    ifelse(isTRUE(Low.co) & isFALSE(Low.hc)& low.years.co==1,
           a.txt%>%filter(Variable=="Autotext13")%>%pull(Description),
           ifelse(isTRUE(Low.co) & isFALSE(Low.hc) & low.years.co>1,
                  sprintf(a.txt%>%filter(Variable=="Autotext14")%>%pull(Description),
                          start.run.co),
                  ifelse(isFALSE(Low.co) & isFALSE(Low.hc),
                         a.txt%>%filter(Variable=="Autotext15")%>%pull(Description),
                         ifelse(isTRUE(Low.co) & isTRUE(Low.hc) & low.years.co==0,
                                a.txt%>%filter(Variable=="Autotext16")%>%pull(Description),
                                ifelse(isTRUE(Low.co) & isTRUE(Low.hc)& low.years.co>0,
                                       sprintf(a.txt%>%filter(Variable=="Autotext17")%>%pull(Description),
                                               start.run.co),
                                               ifelse(isFALSE(Low.co) & isFALSE(Low.hc),
                                                      a.txt%>%filter(Variable=="Autotext18")%>%pull(Description),"")
                                )
                         )
                  )
           )
    )
                               
                         
  # new stable state
  #Autotext19="However, the taxonomic composition of coral communities are similar to those more recently observed, suggesting a persistent shift."
  
  # new stable state
  #Autotext20="The taxonomic composition of coral communities are also distinct from those more recently observed, suggesting ongoing changes in community composition."
  
  sent.k3<-                                           
    ifelse(isTRUE(Low.co) & isFALSE(Low.co.c),
           a.txt%>%filter(Variable=="Autotext19")%>%pull(Description),
           ifelse(isTRUE(Low.co) & isTRUE(Low.co.c),
                  a.txt%>%filter(Variable=="Autotext20")%>%pull(Description),""))
  if(sent.k3==""){sent.k3=NULL}
  if(sent.co==""){sent.co=NULL}
  ####### --- Taxa responsible for composition shift
  taxa<-c.df %>% filter(Year==y)
  
  inc.6<-taxa %>% 
    filter(Change=="Increase" & k==6) %>%
    pull(page.desc)
  
  inc<-ifelse(length(inc.6)==1, inc.6[1],
                   ifelse(length(inc.6)==2, paste(inc.6[1],"and", inc.6[2]),
                          paste0(inc.6[1],", ", inc.6[2], " and", inc.6[3])
                   )
  )
  
  dec.6<-taxa %>% 
    filter(Change=="Decrease" & k==6) %>%
    pull(page.desc)
  
  dec<-ifelse(length(dec.6)==1, dec.6[1],
                    ifelse(length(dec.6)==2, paste(dec.6[1],"and", dec.6[2]),
                           paste0(dec.6[1],", ", dec.6[2], " and", dec.6[3])
                    )
  )
  # 
  # autotext21<-"Most infulential in observed changes in coral community composition have been increases in the relative abundance of %s and decreases in %s."
  # 
  # autotext22<-"Most infulential in observed changes in coral community composition have been increases in the relative abundance of %s."
  # 
  # autotext23<-"Most infulential in observed changes in coral community composition have been decreases in the relative abundance of %s."
  # 
  
  sent.taxa=ifelse(length(inc.6)>0 & length(dec.6>0),
                   sprintf(a.txt%>%filter(Variable=="Autotext21")%>%pull(Description),
                           inc,
                           dec),
                  ifelse(length(dec)==0,
                         sprintf(a.txt%>%filter(Variable=="Autotext22")%>%pull(Description),
                                 inc,
                                 sprintf(a.txt%>%filter(Variable=="Autotext23")%>%pull(Description),
                                         dec)
                         )
                  )
  )
                  
  
  #********************************************************************************************
  # Sampling summary lead in for context and to reduce repetition within indicator sentences####
  #********************************************************************************************
  CC='Coral.cover' %in% (i.df$Indicator)
  M="Macroalgae" %in% (i.df$Indicator)
  C="Community.composition" %in% (i.df$Indicator)
  J="Juvenile.density" %in% (i.df$Indicator)
  P="Recovery.performance" %in% (i.df$Indicator)
  
  sample.autotext="This classification reflects the assesement of indicators relative to their historical baselines"  
  sample.autotext.n1="A clasification could not be made for this reef as estimates for all five indicators were not available"
  
  sent.samp=
    ifelse(isTRUE(CC&C&J&P&M), a.txt%>%filter(Variable=="sample.autotext")%>%pull(Description),
           a.txt%>%filter(Variable=="sample.autotext.n1")%>%pull(Description))
  
 
  #combine sentences####   
  caption<-format_html_list(c(sent.class,
                              sent.hc,
                              sent.pe,
                              sent.j.state,
                              sent.j.c,
                              sent.ma, 
                              sent.co, 
                              sent.k3,
                              sent.taxa
                              ),
                            ordered = F)
  note<-paste0("<i>Disclaimer:</i>", sent.samp)
  
  
  return(list(caption, note))
  
}

