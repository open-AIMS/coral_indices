#auto text for regional summaries of Indicators


##for development 
# setwd("C:\\Users/mgonzale/OneDrive - Australian Institute of Marine Science/projects/RIMReP/Habitat_Indicators/")
# input=list(region_select="GBRMPA.MA",
# report_year=2017,
# value_select="Cairns/Cooktown Management Area")
# load("outputs/scores.RData")
# i.df<-scores%>%
#   filter(Name==input$value_select)%>%
#   droplevels()

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


sum.tx.r<-function(i.df, y){
  require(tidyverse)
  # source("scripts/Misc/HighLevel_Classification.R")
  a.txt<-read_csv(file = "scripts/Misc/Synoptic_text_region.csv")
  #a.txt<-read_csv(file = "Synoptic_text.csv")
  # Indi<-i.df %>% 
  #   dplyr::select(Indicator) %>% 
  #   unique %>%
  #   mutate(Ind.desc=case_match(Indicator, "Coral.cover"~"hard coral cover",
  #                              "Macroalgae" ~"proportion of macroalgae in the algal cover",
  #                              "Juvenile.density"~"density of juvenile hard corals",
  #                              "Recovery.performance"~ "hard coral cover recovery",
  #                              "Community.composition"~"coral community composition"))
  
  
  #************************************
  # Overall classification of condition ###########
  #************************************
  
  Cl<-i.df%>%filter(Year==y, Reference=="Baseline")%>%Cond.Class()%>%pull(Class)
  
  sent.class=
    sprintf(paste(
      a.txt%>%filter(Variable=="Autotext.class")%>%pull(Description), ##overall statement
      a.txt%>%filter(Variable==Cl)%>%pull(Description)),
      y,
      Cl)
  
  #***********************
  # Hard coral cover #####
  #***********************
  
  ### development data wrangling
  # indices<-read.csv(file="Indices.csv")
  # y=2022
  # 
  # a.txt<-read_csv(file = "Synoptic_text.csv")
  # 
  # i.df<-indices%>%
  #   filter(Name=="Burdekin" & Shelf=="Offshore")
  
 
  
  #####************************************************************************ 
  ##Need a lookup to match Composition_change Name (which are reefs) to Regions
  ## I gave up on identifying taxa most changes at the regional level as would require:
  ## Creating a lookup for each reef link it to a region -easy enough
  ## Identifying the reef as having deviated from it's reference - via the lookup I guess 
  ## then aggregating as per below which test the concept for all reefs in the Burdekin region.
  # taxaLookup<-read.csv(file="taxaLookup.csv")
  # comps<-read.csv(file="Composition_change.csv")
  # burReefs<-read.csv(file='reefs.csv') ## this is a temporary work around
  # c.df<- comps%>%
  #   filter(REPORT_YEAR==y & k==6) %>% ### to be completed with & Region=="" to allow for the region of interest
  #   right_join(burReefs) %>%
  #   group_by(REEF.d,k) %>%
  #   arrange(desc(abs(meanDiff))) %>%
  #   slice(1:3) %>%
  #   ungroup %>%
  #   mutate(Change=ifelse(meanDiff>0,"Increase","Decrease")) %>%
  #   group_by(Change, Taxon) %>%
  #   summarise(nTaxon=n()) %>%
  #   ungroup %>%
  #   left_join(taxaLookup) %>%
  #   arrange(desc(nTaxon)) %>%
  #   slice(1:4)
  
  data<-i.df %>% 
    filter(Indicator=="Coral.cover" & Reference=="Baseline") %>% 
    droplevels() %>%
    arrange(Year) %>%
    mutate(Low=ifelse(Upper<0.5,1,0),
           runID.l=data.table::rleid(Low))
  
  data.hc.c<-i.df %>% 
    filter(Indicator=="Coral.cover" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  if(is.na(data.hc.c$Median)){
    sent.hc="Coral cover data is not available."
    }else{
    
    start.run<-as.character(data %>% 
                              filter(runID.l==max(runID.l)) %>%
                              summarise(runStart=first(Year)))
    
    low.years<-nrow(data %>% filter(runID.l==max(runID.l) & Low=='1'))
    prev.low<-data %>%
      filter(Year<y,Low==1)%>%
      summarise(prev.low=min(Year))%>%
      pull(prev.low)
    
    data.hc<-data %>%
      filter(Year==y)
    
    Low.hc<-data.hc$Upper<0.5
    
    State.hc<-data.hc %>%
      mutate(St=ifelse(Upper<0.5, "below",
                       ifelse(Lower>0.5, "above", "similar to"))) %>%
      pull(St)
    
    State.hc.c<-data.hc.c %>%
      mutate(St=ifelse(Upper<0.5, "unlikely", "likely")) %>%
      pull(St)
    
    # 
    # Indi.desc<-as.character(data.hc %>% left_join(Indi) %>%
    #                           pull(Ind.desc))
    # 
    n.conseq<-as.character(data.hc.c %>%  pull(n.below))
    n.reefs<-as.character(data.hc %>%  pull(tn.reefs))
    n.no.conseq=as.character(as.numeric(n.reefs)-as.numeric(n.conseq))
    
    # # Regional cover of hard corals  declined to below historical reference levels, and below consequence at at least one reef. 
    # Autotext1="The cover of hard corals declined to be below historical reference levels. At current levels, hard corals are unlikely to be supporting positive reef accretion at %s of the reefs monitored."
    # 
    # # Regional cover of hard corals declined to below historical reference levels, but above consequence at all reefs.       
    # Autotext2="The cover of hard corals declined to be below historical reference levels. However, at current levels the cover hard corals are likely to be supporting positive reef accretion at all reefs monitored."
    # 
    # # Regional cover of hard corals remain below historical reference levels, and below consequence at at least one reef. 
    # Autotext3="The cover of hard corals has remained below historical reference levels since %s. At current levels hard corals are unlikely to be supporting positive reef accretion at %s of %s the reefs monitored."
    # 
    # # Regional cover of hard corals remain below historical reference levels, but above consequence at all reefs.       
    # Autotext4="The cover of hard corals has remained below historical reference levels since %s. However, at current levels the cover hard corals are likely to be supporting positive reef accretion at all reefs monitored."
    # 
    # # Regional cover of hard corals at or above historical reference levels, at least one reef below consequence. 
    # Autotext5="The cover of hard corals is %s historical reference levels. At current levels hard corals are likely to be supporting positive reef accretion at %s of %s the reefs monitored."
    # 
    # # Regional cover of hard corals at or above historical reference levels, all reefs above consequence.       
    # Autotext6="The cover of hard corals is %s historical reference levels. At current levels hard corals are likely to be supporting positive reef accretion at all reefs monitored"
    
    sent.hc=
      ifelse((isTRUE(Low.hc) & (as.numeric(n.conseq) >0) & low.years<=1),
             sprintf(a.txt%>%filter(Variable=="Autotext1")%>%pull(Description),
                     #y,
                     #Indi.desc,
                     n.conseq),
             ifelse((isTRUE(Low.hc) & as.numeric(n.conseq)==0 & low.years==1),
                    sprintf(a.txt%>%filter(Variable=="Autotext2")%>%pull(Description),
                            #y,
                            #Indi.desc,
                            ),
                    ifelse((isTRUE(Low.hc) & as.numeric(n.conseq) >0 & low.years>1),
                           sprintf(a.txt%>%filter(Variable=="Autotext3")%>%pull(Description),
                                   #Indi.desc,
                                   prev.low,
                                   y,
                                   n.conseq, n.reefs),
                           ifelse(isTRUE(Low.hc) & as.numeric(n.conseq==0) & low.years>1,
                                  sprintf(a.txt%>%filter(Variable=="Autotext4")%>%pull(Description),
                                          #Indi.desc,
                                          prev.low,
                                          y),
                                  ifelse(isFALSE(Low.hc) & as.numeric(n.conseq >0),
                                         sprintf(a.txt%>%filter(Variable=="Autotext5")%>%pull(Description),
                                                 #y,
                                                 #Indi.desc,
                                                 State.hc,
                                                 n.no.conseq, n.reefs),
                                         if(isFALSE(Low.hc) & as.numeric(n.conseq==0)){
                                           sprintf(a.txt%>%filter(Variable=="Autotext6")%>%pull(Description),
                                                   #y,
                                                   #,
                                                   State.hc)})
                           )
                    )
             )
      )
  }
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
  high.pe<-data.p$Lower>0.5
  
  at.or.above<-ifelse(isTRUE(high.pe), "exceeded", "been consistent with")
  
  Concern<-ifelse(isFALSE(Low.hc), "concern","additional concern")
  
  # Indi.desc.pe<-as.character(Indi %>% 
  #                              filter(Indicator=="Recovery.performance") %>% 
  #                              pull(Ind.desc))
  # 
  n.reef.pe<-as.character(data.p %>%  pull(tn.reefs))
  n.reef.pe.low<-as.character(data.p %>%  pull(n.below))
  n.reef.pe.c.low<-as.character(data.p.c %>%  pull(n.below))
  
  
  # # Regional Recovery.performance is low 
  # Autotext7="Of %s is that during  recent recovery periods rates, of increase in hard coral cover have been slower than expected."
  #
  # # Regional Recovery.performance at or above 
  # Autotext8="Encouragingly, during the most recent recovery periods, rates of increase in hard coral cover have %s historical observations."
  # 
  # # number of reefs low
  # Autotext9="Of the %s reefs monitored recovery performance was below their historical reference range at %s."
  # 
  # # Reef level Critical Recovery.performance all low
  # Autotext10="Notably, current data indicates that recovery performance has declined relative to each reef's recent trajectories."
  # 
  # # Reef level Critical Recovery.performance some low 
  # Autotext11="Additionally, at s% reefs the current observation suggests a decline in recovery performance relative to their recent trajectories."
  # 
  
  
  sent.pe=
    ifelse(isTRUE(Low.pe),
           sprintf(a.txt%>%filter(Variable=="Autotext7")%>%pull(Description),
                   Concern),
           ifelse(isFALSE(Low.pe),
                  sprintf(a.txt%>%filter(Variable=="Autotext8")%>%pull(Description),
                          at.or.above), NA)
    )
  
  if(is.na(sent.pe)){sent.pe=NULL}
  
  sent.pe.n=
    sprintf(a.txt%>%filter(Variable=="Autotext9")%>%pull(Description),
                                 n.reef.pe,
                                 n.reef.pe.low)
  sent.pe.c=
    ifelse(n.reef.pe==n.reef.pe.c.low,
           a.txt%>%filter(Variable=="Autotext10")%>%unique()%>%pull(Description),
           ifelse(n.reef.pe >n.reef.pe.c.low &n.reef.pe.c.low>0,
                  sprintf(a.txt%>%filter(Variable=="Autotext11")%>%pull(Description),
                          n.reef.pe.c.low), NA)
    )
  if(is.na(sent.pe.c)){sent.pe.c=NULL}
  
  #********************##
  #  Juveniles      ####
  #*******************###      
  #JUv data
  data.j<-i.df%>% 
    filter(Indicator=="Juvenile.density" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  Low.j<-data.j$Upper<0.5
  
  State.j<-data.j %>%
    mutate(St=ifelse(Upper<0.5, "below",
                     ifelse(Lower>0.5, "above", "similar to"))) %>%
    pull(St)
  
   add.conseq<-ifelse(isFALSE(Low.hc) & isFALSE(Low.pe),"Detracting","Further detracting")
  
   n.reef.j<-as.character(data.j %>%  pull(tn.reefs))
   n.reef.j.low<-as.character(data.j %>%  pull(n.below))
   
  #### Juveniles relative to reference
  # at or above all reefs
  # Autotext12="The median density of juvenile corals was %s historical reference levels, suggesting ongoing recovery potential."
  # # median at or above but low at some
  # Autotext13="The median density of juvenile corals was %s historical reference levels, however fell below these levels at %s of the %s reefs assessed."
  # # median below
  # Autotext14="The median density of juvenile corals was below historical reference levels, %s from the assessment of coral communities. Juvenile densities were below reference levels at %s of the %s reefs assessed." 
  
  sent.j=
    ifelse(isFALSE(Low.j) & n.reef.j.low==0,
           sprintf(a.txt%>%filter(Variable=="Autotext12")%>%pull(Description),
                   State.j),
           ifelse(isFALSE(Low.j) & n.reef.j.low>0,
                  sprintf(a.txt%>%filter(Variable=="Autotext13")%>%pull(Description),
                          State.j,
                          n.reef.j.low),
                  ifelse(isTRUE(Low.j),
                         sprintf(a.txt%>%filter(Variable=="Autotext14")%>%pull(Description),
                                 add.conseq,
                                 n.reef.j.low,
                                 n.reef.j))
           )
    )
  if(is.na(Low.j)){sent.j=NULL} 
  #### Juvenile critical 
  
  data.j.c<-i.df %>% 
    filter(Indicator=="Juvenile.density" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.j.c<-data.j.c$Upper<0.5
  
  Concern.j.low<-ifelse(isTRUE(Low.j.c), "Of added concern is that","However")
  Concern.j.high<-ifelse(isTRUE(Low.j.c),"However", "In addition")
  
  n.reef.j.c.low<-as.character(data.j.c %>%  pull(n.below))
  
  # Autotext16="%s the current densities of <i>Acropora</i> juveniles are low and likely to be limiting recovery at all reefs."
  # Autotext17="%s the current densities of <i>Acropora</i> juveniles are low and likely to be limiting recovery at %s reefs."
  # Autotext15="%s the current densities of <i>Acropora</i> juveniles are sufficient to promote recovery at all reefs."
  
  sent.j.c=
    ifelse(isTRUE(Low.j) & n.reef.j.c.low==0,
           sprintf(a.txt%>%filter(Variable=="Autotext15")%>%pull(Description),
                   Concern.j.low),
           ifelse(isFALSE(Low.j) & n.reef.j.c.low==0,
                  sprintf(a.txt%>%filter(Variable=="Autotext15")%>%pull(Description),
                          Concern.j.high),
                  ifelse(isTRUE(Low.j) & n.reef.j.c.low==n.reef.j,
                         sprintf(a.txt%>%filter(Variable=="Autotext16")%>%pull(Description),
                                 Concern.j.low),
                         ifelse(isFALSE(Low.j) & n.reef.j.c.low==n.reef.j,
                                sprintf(a.txt%>%filter(Variable=="Autotext16")%>%pull(Description),
                                        Concern.j.high),
                                ifelse(isTRUE(Low.j) & n.reef.j.c.low<n.reef.j,
                                       sprintf(a.txt%>%filter(Variable=="Autotext17")%>%pull(Description),
                                               Concern.j.low,
                                               n.reef.j.c.low),
                                       sprintf(a.txt%>%filter(Variable=="Autotext17")%>%pull(Description),
                                               Concern.j.high,
                                               n.reef.j.c.low)
                                       )
                                )
                  )
           )
    )
        
  if(is.na(Low.j.c)){sent.j.c=NULL}   
  
  #********************##
  #  Macroalage      ####
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
  
  Concern.ma<-ifelse(isTRUE(Low.j|Low.pe), "Adding to concern for recovery potential","Of concern")
  
  Concern.ma.c<-ifelse(isTRUE(Low.m),"Additionally","However due to high reference levels")
  
  n.reef.m<-as.character(data.m %>%  pull(tn.reefs))
  n.reef.m.low<-as.character(data.m %>%  pull(n.below))
  n.reef.m.c.low<-as.character(data.m.c %>%  pull(n.below))
  
  #MA at or below
  Autotext18="The representation of macroalgae species within the benthic algal communities was %s historical reference levels." 
  #MA above
  Autotext19="%s the representation of macroalgae species within the benthic algal communities was above historical reference levels."
  
 
  sent.m=
    ifelse(isFALSE(Low.m),
           sprintf(a.txt%>%filter(Variable=="Autotext18")%>%pull(Description),
                   State.ma),
                 sprintf(a.txt%>%filter(Variable=="Autotext15")%>%unique()%>%pull(Description),
                          Indi.desc.j,
                          Indi.desc.m))
                  
#
#  Autotext20="At current levels macroalgae are unlikley to be limiting coral community resilience on any reefs monitored."
#  Autotext21="%s, current levels macroalgae are likley to be limiting coral community resilience at all reefs monitored."
# Autotext22="%s, current levels macroalgae are likley to be limiting coral community resilience at %s reefs monitored"             

sent.m.c=
   ifelse(n.reef.m.c.low==0,
          a.txt%>%filter(Variable=="Autotext20")%>%pull(Description),
          ifelse(n.reef.m.c.low==n.reef.m,
                 sprintf(a.txt%>%filter(Variable=="Autotext21")%>%pull(Description),
                         Concern.ma.c),
                 sprintf(a.txt%>%filter(Variable=="Autotext22")%>%pull(Description),
                         Concern.ma.c,
                         n.reef.m.c.low)
          )
   )
  
  
  #************#####
  # Community.composition#####
  #************#####
  #comp data
  data.co<-i.df %>% 
    filter(Indicator=="Community.composition" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.co.low<-i.df %>% 
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
  
  n.reef.co<-as.character(data.co %>%  pull(tn.reefs))
  n.reef.co.low<-as.character(data.co.c %>%  pull(n.below))
  
  # comp low - hc ok.
  # Autotext23="Despite the cover of hard corals remaining at or above reference levels, there is evidence that the composition of communities have changed. Such changes are evident at %s reefs."
  # 
  # # comp low >1 year - hc ok. 
  # Autotext24="Despite the cover of hard corals remaining at or above reference levels, the composition of communities have remained distinct from those historically observed since %s. Current changes are evident at %s reefs."
  # 
  # # comp ok - hc ok, no reef level change
  # Autotext25="In addition to the cover of hard corals remaining within reference levels there is no evidence for region-wide change in the taxnomic composition of coral communities."
  # 
  # # comp ok - hc ok, some reef level change
  # Autotext26="In addition to the cover of hard corals remaining within reference levels there is no evidence for region-wide change in the taxnomic composition of coral communities. However, community composition shifts from that historically observed are evident at %s reefs."
  # 
  # # comp low - hc low.
  # Autotext27="In addition to low cover of hard corals, there is evidence that the composition of communities have changed within the region. Such changes are evident at %s reefs."
  # 
  # # comp low >1 year - hc low.
  # Autotext28="In addition to low cover of hard corals, the composition of communities have remained distinct from those historically observed since %s. Currently changes are evident at %s reefs."
  # 
  # # comp ok - hc low
  # Autotext29="Although the cover of hard corals is low, there is no evidence for region-wide change in the taxnomic composition of coral communities."
  # 
  
  sent.co<-                                           
    ifelse(isTRUE(Low.co) & isFALSE(Low.hc)& low.years.co>0,
           sprintf(a.txt%>%filter(Variable=="Autotext23")%>%unique()%>%pull(Description),
                   n.reef.co.low),
           ifelse(isTRUE(Low.co) & isFALSE(Low.hc) & low.years.co>0,
                  sprintf(a.txt%>%filter(Variable=="Autotext24")%>%unique()%>%pull(Description),
                          start.run.co,
                          n.reef.co.low),
                  ifelse(isFALSE(Low.co) & isFALSE(Low.hc) & n.reef.co.low==0,
                         a.txt%>%filter(Variable=="Autotext25")%>%unique()%>%pull(Description),
                         ifelse(isFALSE(Low.co) & isFALSE(Low.hc) & as.numeric(n.reef.co.low)>0,
                                sprintf(a.txt%>%filter(Variable=="Autotext26")%>%unique()%>%pull(Description),
                                        n.reef.co.low),
                                ifelse(isTRUE(Low.co) & isTRUE(Low.hc)& low.years.co==0,
                                       sprintf(a.txt%>%filter(Variable=="Autotext27")%>%unique()%>%pull(Description),
                                               n.reef.co.low),
                                       ifelse(isTRUE(Low.co) & isTRUE(Low.hc) & low.years.co>0,
                                              sprintf(a.txt%>%filter(Variable=="Autotext28")%>%unique()%>%pull(Description),
                                                      start.run.co,
                                                      n.reef.co.low),a.txt%>%filter(Variable=="Autotext29")%>%unique()%>%pull(Description))
                                )
                         )
                  )
           )
    )
  # new stable state
  # Autotext30="While the taxnomic community composition of coral communities remains distinct from those observed during the historic reference period they are similar to those more recently observed, suggesting a persitent shift."
  # 
  # # new stable state
  # Autotext31="In addition to the taxnomic community composition of coral communities remaining distinct from those observed during the historic reference period they are also distinct from those more recently observed, suggesting ongoing changes in community composition."
  # 
  # # new stable state
  # Autotext32="The taxnomic community composition of coral communities remains consistent over time."
  # 
  sent.k3<-                                           
    ifelse(isTRUE(Low.co) & isFALSE(Low.co.c),
           sprintf(a.txt%>%filter(Variable=="Autotext37")%>%unique()%>%pull(Description),
                   n.reef.co.low),
           case_when( (isTRUE(Low.co) & isTRUE(Low.co.c)) ~  a.txt%>%filter(Variable=="Autotext38")%>%unique()%>%pull(Description),
                      .default = a.txt%>%filter(Variable=="Autotext39")%>%unique()%>%pull(Description))
           # if(isTRUE(Low.co) & isTRUE(Low.co.c)){
           #   a.txt%>%filter(Variable=="Autotext38")%>%unique()%>%pull(Description)}else{
           #     a.txt%>%filter(Variable=="Autotext39")%>%unique()%>%pull(Description)
           #   } 
           )
  
  
  #********************************************************************************************
  # Sampling summary lead in for context and to reduce repetition within indicator sentences####
  #********************************************************************************************
  not.j=as.numeric(n.reefs)-as.numeric(n.reef.j)
  not.comp=as.numeric(n.reefs)-as.numeric(n.reef.co)
  not.pe=as.numeric(n.reefs)-as.numeric(n.reef.pe)
  not.ma=as.numeric(n.reefs)-as.numeric(n.reef.m)
  
   Indi<-i.df %>% 
     dplyr::select(Indicator) %>% 
    unique %>%
    mutate(Ind.desc=case_match(Indicator, "Coral.cover"~"hard coral cover",
                                "Macroalgae" ~"macroalgae",
                               "Juvenile.density"~"juvenile corals",
                               "Recovery.performance"~ "recovery performance",
                               "Community.composition"~"community composition"))
  
  
   Indi.desc.pe<-as.character(Indi %>% filter(Indicator=="Recovery.performance") %>% pull(Ind.desc))
   Indi.desc.j<-as.character(Indi %>% filter(Indicator=="Juvenile.density") %>% pull(Ind.desc))
   Indi.desc.co<-as.character(Indi %>% filter(Indicator=="Community.composition") %>% pull(Ind.desc))
   Indi.desc.ma<-as.character(Indi %>% filter(Indicator=="Macroalgae") %>% pull(Ind.desc))
   Indi.desc.co<-as.character(Indi %>% filter(Indicator=="Community.composition") %>% pull(Ind.desc))
   Indi.desc.hc<-as.character(Indi %>% filter(Indicator=="Coral.cover") %>% pull(Ind.desc))
  
  # sample.autotext="This synopsis was derived from indicators estimated at %s locations, across reefs and survey depths. In %s, all indicators were assessed for each those locations."  
  # sample.autotext.n1="This synopsis was derived from indicators estimated at %s locations, across reefs and survey depths. In %s, the scores for %s was not available in %s of those locations."
  # sample.autotext.n2="This synopsis was derived from indicators estimated at %s locations, across reefs and survey depths. In %s, the scores for %s and %s were not avaialble in %s and %s of these locations, respectively."
  # sample.autotext.n3="This synopsis reflects the indicator scores observed at %s reef and depth combinations. In %s,the scores for %s, %s and %s were not avaliable in %s, %s and %s of these locations, respectively."
  # 
  sent.samp=
    ifelse(max(not.comp,not.j,not.pe,not.ma)==0,
           sprintf(a.txt%>%filter(Variable=="sample.autotext")%>%unique()%>%pull(Description),
                   n.reefs,
                   y),
           ifelse(not.j>0 & max(not.comp,not.pe,not.ma)==0,
                  sprintf(a.txt%>%filter(Variable=="sample.autotext.n1")%>%unique()%>%pull(Description),
                          n.reefs,
                          y,
                          Indi.desc.j,
                          not.j),
                  ifelse(not.comp>0 & max(not.j,not.pe,not.ma)==0,
                         sprintf(a.txt%>%filter(Variable=="sample.autotext.n1")%>%unique()%>%pull(Description),
                                 n.reefs,
                                 y,
                                 Indi.desc.co,
                                 not.comp),
                         ifelse(not.pe>0 & max(not.j,not.comp,not.ma)==0,
                                sprintf(a.txt%>%filter(Variable=="sample.autotext.n1")%>%unique()%>%pull(Description),
                                        n.reefs,
                                        y,
                                        Indi.desc.pe,
                                        not.pe),
                                ifelse(not.ma>0 & max(not.j,not.pe,not.comp)==0,
                                       sprintf(a.txt%>%filter(Variable=="sample.autotext.n1")%>%unique()%>%pull(Description),
                                               n.reefs,
                                               y,
                                               Indi.desc.ma,
                                               not.ma),
                                       ifelse(not.j>0 & not.comp>0 & max(not.ma,not.pe)==0,
                                              sprintf(a.txt%>%filter(Variable=="sample.autotext.n2")%>%unique()%>%pull(Description),
                                                      n.reefs,
                                                      y,
                                                      Indi.desc.j,
                                                      Indi.desc.co,
                                                      not.j,
                                                      not.comp),
                                              ifelse(not.j>0 & not.ma>0 & max(not.comp,not.pe)==0,
                                                     sprintf(a.txt%>%filter(Variable=="sample.autotext.n2")%>%unique()%>%pull(Description),
                                                             n.reefs,
                                                             y,
                                                             Indi.desc.j,
                                                             Indi.desc.ma,
                                                             not.j,
                                                             not,ma),
                                                     ifelse(not.j>0 & not.pe>0 & max(not.comp,not.ma)==0,
                                                            sprintf(a.txt%>%filter(Variable=="sample.autotext.n2")%>%unique()%>%pull(Description),
                                                                    n.reefs,
                                                                    y,
                                                                    Indi.desc.j,
                                                                    Indi.desc.pe,
                                                                    not.j,
                                                                    not.pe),
                                                            ifelse(not.pe>0 & not.ma>0 & max(not.comp,not.j)==0,
                                                                   sprintf(a.txt%>%filter(Variable=="sample.autotext.n2")%>%unique()%>%pull(Description),
                                                                           n.reefs,
                                                                           y,
                                                                           Indi.desc.pe,
                                                                           Indi.desc.ma,
                                                                           not.pe,
                                                                           not,ma),
                                                                   ifelse(not.pe>0 & not.comp>0 & max(not.ma,not.j)==0,
                                                                          sprintf(a.txt%>%filter(Variable=="sample.autotext.n2")%>%unique()%>%pull(Description),
                                                                                  n.reefs,
                                                                                  y,
                                                                                  Indi.desc.pe,
                                                                                  Indi.desc.co,
                                                                                  not.pe,
                                                                                  not.comp),
                                                                          ifelse(not.ma>0 & not.comp>0 & max(not.pe,not.j)==0,
                                                                                 sprintf(a.txt%>%filter(Variable=="sample.autotext.n2")%>%unique()%>%pull(Description),
                                                                                         n.reefs,
                                                                                         y,
                                                                                         Indi.desc.ma,
                                                                                         Indi.desc.co,
                                                                                         not.ma,
                                                                                         not.comp),
                                                                                 ifelse(not.ma>0 & not.comp>0 & not.j>0 & not.pe==0,
                                                                                        sprintf(a.txt%>%filter(Variable=="sample.autotext.n3")%>%unique()%>%pull(Description),
                                                                                                n.reefs,
                                                                                                y,
                                                                                                Indi.desc.ma,
                                                                                                Indi.desc.co,
                                                                                                Indi.desc.j,
                                                                                                not.ma,
                                                                                                not.comp,
                                                                                                not.j),
                                                                                        ifelse(not.ma>0 & not.pe>0 & not.j>0 & not.comp==0,
                                                                                               sprintf(a.txt%>%filter(Variable=="sample.autotext.n3")%>%unique()%>%pull(Description),
                                                                                                       n.reefs,
                                                                                                       y,
                                                                                                       Indi.desc.ma,
                                                                                                       Indi.desc.pe,
                                                                                                       Indi.desc.j,
                                                                                                       not.ma,
                                                                                                       not.pe,
                                                                                                       not.j),
                                                                                               ifelse(not.ma>0 & not.comp>0 & not.pe>0 & not.j==0,
                                                                                                      sprintf(a.txt%>%filter(Variable=="sample.autotext.n3")%>%unique()%>%pull(Description),
                                                                                                              n.reefs,
                                                                                                              y,
                                                                                                              Indi.desc.ma,
                                                                                                              Indi.desc.co,
                                                                                                              Indi.desc.pe,
                                                                                                              not.ma,
                                                                                                              not.comp,
                                                                                                              not.pe),
                                                                                                      ifelse(not.pe>0 & not.comp>0 & not.j>0 & not.ma==0,
                                                                                                             sprintf(a.txt%>%filter(Variable=="sample.autotext.n3")%>%unique()%>%pull(Description),
                                                                                                                     n.reefs,
                                                                                                                     y,
                                                                                                                     Indi.desc.pe,
                                                                                                                     Indi.desc.co,
                                                                                                                     Indi.desc.j,
                                                                                                                     not.pe,
                                                                                                                     not.comp,
                                                                                                                     not.j)
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
                  )
           )
    )
  
  
  #combine sentences####   
  caption<-format_html_list(c(sent.class,
                              sent.hc,
                              sent.pe,
                              sent.pe.n,
                              sent.pe.c,
                              sent.j,
                              sent.j.c,
                              sent.m,
                              sent.m.c,
                              sent.co,
                              sent.k3),
                            ordered = F)
  note<-paste0("<i>Disclaimer:</i>", "\n",
               sent.samp)
  
  
  return(list(caption, note))
  
}

