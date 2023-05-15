####Surprise script
---------------------------------------------------
'This script is associated with the article:
<< Recovery of insular seabird populations years after rodent eradication >>
Philippe-Lesaffre et al, 2022'
---------------------------------------------------
###Packages
library('distributional')
library('MuMIn')
library('dplyr')
library('tidyverse')
library('tidyr')
library('ggplot2')
library('brms')
library('bblme')
library('rstan')
library('ggeffects')
library('lme4')
library('MASS')
library('readxl')
library('DHARMa')
library('tidybayes')
library('splines')
library('performance')
library('segmented')
library('ggdist')
library('bayesplot')
library('loo')
library('gt')
library('plyr')
library('cowplot')
library('emmeans')
library('sjstats')
library('readxl')
library('ggrepel')


###Data preparation

##Data exctration
donnees.oiseaux <- read_xlsx(path ="count_data_surprise_raw.xlsx",sheet = 1) #data.frame containing the count data for the 4 bird species
donnees.env <- read_xlsx(path ="env_data_surprise.xlsx",sheet = 1) #data.frame containing only the environmental variables for the 8 years of survey

donnees.oiseaux.raw.2002 <- read_xlsx(path ="count_data_surprise_raw.xlsx",sheet = 1) #data.frame containing the count data for the 4 bird species
donnees.oiseaux.raw.2002 <- donnees.oiseaux.raw.2002 %>% mutate(Rats=1) %>% mutate(year=2002) %>% dplyr::select(Nom,X1,Y1,transect_section,FOBR,FOPR,FOMA,NOCO,FRsp,Vegetation,Rats,year)

donnees.oiseaux.raw.2003 <- read_xlsx(path ="count_data_surprise_raw.xlsx",sheet = 2)
donnees.oiseaux.raw.2003 <- donnees.oiseaux.raw.2003 %>% mutate(Rats=1) %>% mutate(year=2003) %>% dplyr::select(Nom,X1,Y1,transect_section,FOBR,FOPR,FOMA,NOCO,FRsp,Vegetation,Rats,year)

donnees.oiseaux.raw.2004 <- read_xlsx(path ="count_data_surprise_raw.xlsx",sheet = 3) 
donnees.oiseaux.raw.2004 <- donnees.oiseaux.raw.2004 %>% mutate(Rats=1) %>% mutate(year=2004) %>% dplyr::select(Nom,X1,Y1,transect_section,FOBR,FOPR,FOMA,NOCO,FRsp,Vegetation,Rats,year)

donnees.oiseaux.raw.2005 <- read_xlsx(path ="count_data_surprise_raw.xlsx",sheet = 4)
donnees.oiseaux.raw.2005 <- donnees.oiseaux.raw.2005 %>% mutate(Rats=1) %>% mutate(year=2005) %>% dplyr::select(Nom,X1,Y1,transect_section,FOBR,FOPR,FOMA,NOCO,FRsp,Vegetation,Rats,year)

donnees.oiseaux.raw.2006 <- read_xlsx(path ="count_data_surprise_raw.xlsx",sheet = 5)
donnees.oiseaux.raw.2006 <- donnees.oiseaux.raw.2006 %>% mutate(Rats=0) %>% mutate(year=2006) %>% dplyr::select(Nom,X1,Y1,transect_section,FOBR,FOPR,FOMA,NOCO,FRsp,Vegetation,Rats,year)

donnees.oiseaux.raw.2007 <- read_xlsx(path ="count_data_surprise_raw.xlsx",sheet = 6)
donnees.oiseaux.raw.2007 <- donnees.oiseaux.raw.2007 %>% mutate(Rats=0) %>% mutate(year=2007) %>% dplyr::select(Nom,X1,Y1,transect_section,FOBR,FOPR,FOMA,NOCO,FRsp,Vegetation,Rats,year)

donnees.oiseaux.raw.2008 <- read_xlsx(path ="count_data_surprise_raw.xlsx",sheet = 7)
donnees.oiseaux.raw.2008 <- donnees.oiseaux.raw.2008 %>% mutate(Rats=0) %>% mutate(year=2008) %>% dplyr::select(Nom,X1,Y1,transect_section,FOBR,FOPR,FOMA,NOCO,FRsp,Vegetation,Rats,year)

donnees.oiseaux.raw.2009 <- read_xlsx(path ="count_data_surprise_raw.xlsx",sheet = 8)
donnees.oiseaux.raw.2009 <- donnees.oiseaux.raw.2009 %>% mutate(Rats=0) %>% mutate(year=2009) %>% dplyr::select(Nom,X1,Y1,transect_section,FOBR,FOPR,FOMA,NOCO,FRsp,Vegetation,Rats,year)



donnees.oiseaux.raw <- rbind(donnees.oiseaux.raw.2002,donnees.oiseaux.raw.2003,donnees.oiseaux.raw.2004,donnees.oiseaux.raw.2005,
                             donnees.oiseaux.raw.2006,donnees.oiseaux.raw.2007,donnees.oiseaux.raw.2008,donnees.oiseaux.raw.2009)

##Data cleaning

donnees.oiseaux.no.Na.raw <- donnees.oiseaux.raw %>%  mutate(Communaute = FOBR + FOPR + FOMA + NOCO) %>% mutate(Year = year-2001) #adding a year effect from 1 to 8
donnees.oiseaux.no.Na.raw$Vegetation <- as.character(donnees.oiseaux.no.Na.raw$Vegetation)
donnees.oiseaux.no.Na.raw$Rats <- as.factor(donnees.oiseaux.no.Na.raw$Rats)



donnees.oiseaux.no.Na.raw[donnees.oiseaux.no.Na.raw == 'plaine'] <- 'Plaine'
donnees.oiseaux.no.Na.raw[donnees.oiseaux.no.Na.raw == 'plage'] <- NA
donnees.oiseaux.no.Na.raw[donnees.oiseaux.no.Na.raw == 'Hibiscus tilacaeus'] <- NA
donnees.oiseaux.no.Na.raw[donnees.oiseaux.no.Na.raw == 'Suriana maritima'] <- NA
donnees.oiseaux.no.Na.raw[donnees.oiseaux.no.Na.raw == 'Leucaena glauca'] <- NA
donnees.oiseaux.no.Na.raw$Vegetation <- as.character(donnees.oiseaux.no.Na.raw$Vegetation)
donnees.oiseaux.no.Na.raw$Vegetation <- donnees.oiseaux.no.Na.raw$Vegetation %>% replace_na('NS')
donnees.oiseaux.no.Na.raw[is.na(donnees.oiseaux.no.Na.raw)] <- 'NS'

donnees.oiseaux.clean.raw <- donnees.oiseaux.no.Na.raw
unique(donnees.oiseaux.clean.raw$Vegetation)


exploration.plot.count.raw <- ggplot(data = donnees.oiseaux.clean.raw, aes(x=Communaute)) + geom_histogram() + theme_classic()
exploration.plot.count.raw #we can observe an zero-inflated distribution, i.e. a lot of 0 on the poisson-like distribution




##Table construction
'We will pursue a Bayesian approach for the modelisation because most of the frequentist approach fail to converge and we obviously cannot choose to eliminate a model only because the model is not converging.
According to most of the articles we will use the brms R package becuase this package offers the possibility to implement the zero-inflated models but also because it is very simple to use it especially to choose the prior'.

donnees.oiseaux.raw.models <- data.frame(Com = as.integer(donnees.oiseaux.clean.raw$Communaute),
                                     FOBR = as.integer(donnees.oiseaux.clean.raw$FOBR),
                                     FOPR = as.integer(donnees.oiseaux.clean.raw$FOPR),
                                     FOMA = as.integer(donnees.oiseaux.clean.raw$FOMA),
                                     FRsp =as.integer(donnees.oiseaux.clean.raw$FRsp),
                                     NOsp = as.integer(donnees.oiseaux.clean.raw$NOCO),
                                     Year = as.factor(donnees.oiseaux.clean.raw$Year),
                                     Hab = as.character(donnees.oiseaux.clean.raw$Vegetation),
                                     Invasion = as.factor(donnees.oiseaux.clean.raw$Rats),
                                     transect = as.factor(donnees.oiseaux.clean.raw$transect_section),
                                     Nom = as.factor(donnees.oiseaux.clean.raw$Nom))


donnees.eradication <- data.frame(Year=c(1:8),eradication=c(0,0,0,0,1,2,3,4))
donnees.oiseaux.raw.models <- merge(donnees.oiseaux.raw.models,donnees.eradication,by=c('Year'))

tableau.oiseaux.raw <- donnees.oiseaux.raw.models %>% dplyr::select(Year,Hab,transect,FOBR,FOPR,FRsp,NOsp,FOMA,eradication,Nom)

FOBR.matrix <- tableau.oiseaux.raw %>% dplyr::select(Year,Hab,transect,FOBR,eradication,Nom)
FOPR.matrix <- tableau.oiseaux.raw %>% dplyr::select(Year,Hab,transect,FOPR,eradication,Nom)
FRsp.matrix <- tableau.oiseaux.raw %>% dplyr::select(Year,Hab,transect,FRsp,eradication,Nom)
NOsp.matrix <- tableau.oiseaux.raw %>% dplyr::select(Year,Hab,transect,NOsp,eradication,Nom)
FOMA.matrix <- tableau.oiseaux.raw %>% dplyr::select(Year,Hab,transect,FOMA,eradication,Nom)


FOBR.matrix <- FOBR.matrix  %>% dplyr::mutate(Species=rep('BRBO',length(FOBR.matrix$Year)))
FOPR.matrix <- FOPR.matrix  %>% dplyr::mutate(Species=rep('RFBO',length(FOPR.matrix$Year)))
FRsp.matrix <- FRsp.matrix  %>% dplyr::mutate(Species=rep('FRsp',length(FRsp.matrix$Year)))
NOsp.matrix <- NOsp.matrix  %>% dplyr::mutate(Species=rep('NOsp',length(NOsp.matrix$Year)))
FOMA.matrix <- FOMA.matrix  %>% dplyr::mutate(Species=rep('MABO',length(FOMA.matrix$Year)))



colnames(FOBR.matrix) <- c('Year','Hab','transect','abundance','eradication','Nom','Species')
colnames(FOPR.matrix) <- c('Year','Hab','transect','abundance','eradication','Nom','Species')
colnames(FRsp.matrix) <- c('Year','Hab','transect','abundance','eradication','Nom','Species')
colnames(NOsp.matrix) <- c('Year','Hab','transect','abundance','eradication','Nom','Species')
colnames(FOMA.matrix) <- c('Year','Hab','transect','abundance','eradication','Nom','Species')



tableau.oiseaux.species.raw <- rbind(FOBR.matrix,FOPR.matrix,FRsp.matrix,NOsp.matrix,FOMA.matrix)
tableau.oiseaux.species.raw$Hab <- as.factor(tableau.oiseaux.species.raw$Hab)
tableau.oiseaux.species.raw$transect <- as.factor(tableau.oiseaux.species.raw$transect)
tableau.oiseaux.species.raw$Species <- as.factor(tableau.oiseaux.species.raw$Species)

###Overview of the surveys





tableau.oiseaux.species.info.year <- tableau.oiseaux.species.raw %>% mutate(Rats = case_when(eradication ==0 ~ 1,
                                                                                         eradication >0 ~ 0))
tableau.oiseaux.species.info.year.1 <- ddply(tableau.oiseaux.species.info.year,.(Year,Species,Rats),summarize,Number_of_couples=sum(abundance))
tableau.oiseaux.species.info.year.2 <- ddply(tableau.oiseaux.species.info.year.1,.(Species,Rats),summarize,mean_number_of_couples=mean(Number_of_couples),sd_number_of_couples=sd(Number_of_couples),min_number_of_couples=min(Number_of_couples),max_number_of_couples=max(Number_of_couples))

BRBO.erad <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==0 & Species=='BRBO') %>% dplyr::select(Number_of_couples)
BRBO.erad.no <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==1 & Species=='BRBO') %>% dplyr::select(Number_of_couples)
wilcox.test(t(BRBO.erad),t(BRBO.erad.no))

FRsp.erad <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==0 & Species=='FRsp') %>% dplyr::select(Number_of_couples)
FRsp.erad.no <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==1 & Species=='FRsp') %>% dplyr::select(Number_of_couples)
wilcox.test(t(FRsp.erad),t(FRsp.erad.no))

MABO.erad <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==0 & Species=='MABO') %>% dplyr::select(Number_of_couples)
MABO.erad.no <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==1 & Species=='MABO') %>% dplyr::select(Number_of_couples)
wilcox.test(t(MABO.erad),t(MABO.erad.no))

NOsp.erad <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==0 & Species=='NOsp') %>% dplyr::select(Number_of_couples)
NOsp.erad.no <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==1 & Species=='NOsp') %>% dplyr::select(Number_of_couples)
wilcox.test(t(NOsp.erad),t(NOsp.erad.no))

RFBO.erad <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==0 & Species=='RFBO') %>% dplyr::select(Number_of_couples)
RFBO.erad.no <- tableau.oiseaux.species.info.year.1 %>% filter(Rats==1 & Species=='RFBO') %>% dplyr::select(Number_of_couples)
wilcox.test(t(RFBO.erad),t(RFBO.erad.no))






tableau.oiseaux.species.test <- tableau.oiseaux.species.raw %>% mutate(test=1)
tableau.oiseaux.species.test<- ddply(tableau.oiseaux.species.test,.(year),summarize,Number_of_section=sum(test))
plot.barplot.veg <- ggplot(data=tableau.oiseaux.species.test,aes(x= year,y=Number_of_section))+ geom_col(aes(fill=Hab),position='stack',color='black',alpha=0.5) + theme_bw() + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")
plot.barplot.veg <- plot.barplot + ylab('Total number of breeding pairs counted') + theme(legend.position ="bottom",text = element_text(size = 15))
plot.barplot.veg





tableau.oiseaux.species.fig1 <- tableau.oiseaux.species.raw
tableau.oiseaux.species.fig1 <- ddply(tableau.oiseaux.species.fig1,.(Year,Species),summarize,Number_of_couples=sum(abundance))


tableau.oiseaux.species.fig1$Year <- as.numeric(tableau.oiseaux.species.fig1$Year)
tableau.oiseaux.species.fig1 <- tableau.oiseaux.species.fig1 %>% mutate(year=Year+2001)
tableau.oiseaux.species.fig1$Year <- as.factor(tableau.oiseaux.species.fig1$Year)
tableau.oiseaux.species.fig1$year <- as.factor(tableau.oiseaux.species.fig1$year)

eradication.state.table <- data.frame(c('Pre-eradication','Pre-eradication','Pre-eradication','Pre-eradication','Post-eradication','Post-eradication','Post-eradication','Post-eradication'),c(2002:2009))
colnames(eradication.state.table)=c("survey","year")
eradication.state.table$survey = factor(eradication.state.table$survey , levels=c('Pre-eradication','Post-eradication'))
tableau.oiseaux.species.fig1 <- merge(tableau.oiseaux.species.fig1,eradication.state.table,by="year")
new <- c('brown booby','frigatebirds','masked booby','noddies','red-footed booby')
names(new) <- c('BRBO','FRsp','MABO','NOsp','RFBO')
plot.barplot <- ggplot(data=tableau.oiseaux.species.fig1,aes(x= year,y=Number_of_couples))+ geom_col(aes(fill=Species),position='stack',color='black',alpha=0.5) + theme_bw() + scale_fill_brewer(palette = "Dark2",limits=c('BRBO','FRsp','MABO','NOsp','RFBO'),labels=c('brown booby','frigatebirds','masked booby','noddies','red-footed booby')) + facet_grid(Species~survey,labeller = labeller(Species = new),scales = 'free',space = "free_x") 
plot.barplot <- plot.barplot + ylab('Number of breeding pairs') + theme(legend.position = 'none',text = element_text(size = 15))+xlab('Year')
plot.barplot

tableau.oiseaux.species.fig1 <- tableau.oiseaux.species.raw
tableau.oiseaux.species.fig1 <- ddply(tableau.oiseaux.species.fig1,.(Year,Species),summarize,Number_of_couples=sum(abundance))
tableau.oiseaux.species.fig1$Year <- as.numeric(tableau.oiseaux.species.fig1$Year)
tableau.oiseaux.species.fig1 <- tableau.oiseaux.species.fig1 %>% mutate(year=Year+2001)
tableau.oiseaux.species.fig1$Year <- as.factor(tableau.oiseaux.species.fig1$Year)
tableau.oiseaux.species.fig1$year <- as.factor(tableau.oiseaux.species.fig1$year)

eradication.state.table <- data.frame(c('Pre-eradication','Pre-eradication','Pre-eradication','Pre-eradication','Post-eradication','Post-eradication','Post-eradication','Post-eradication'),c(2002:2009))
colnames(eradication.state.table)=c("survey","year")
eradication.state.table$survey = factor(eradication.state.table$survey , levels=c('Pre-eradication','Post-eradication'))
tableau.oiseaux.species.fig1 <- merge(tableau.oiseaux.species.fig1,eradication.state.table,by="year")

plot.barplot.bis <- ggplot(data=tableau.oiseaux.species.fig1,aes(x= year,y=Number_of_couples))+ geom_col(aes(fill=Species),position='stack',color='black',alpha=0.5) + theme_bw() + scale_fill_brewer(palette = "Dark2",limits=c('BRBO','FRsp','MABO','NOsp','RFBO'),labels=c('brown booby','frigatebirds','masked booby','noddies','red-footed booby')) + scale_color_brewer(palette = "Dark2") + facet_grid(. ~ survey,scales = "free_x")
plot.barplot.bis <- plot.barplot.bis + ylab('Total number of breeding pairs counted') + theme(legend.position = c(0.15, 0.85),text = element_text(size = 15))
plot.barplot.bis

plot.barplot

tableau.oiseaux.species.veg <- tableau.oiseaux.species.raw
tableau.oiseaux.species.veg <- ddply(tableau.oiseaux.species.veg,.(Species,Hab),summarize,Number_of_couples=sum(abundance))
plot.barplot.veg <- ggplot(data=tableau.oiseaux.species.veg,aes(x= Hab,y=Number_of_couples))+ geom_col(aes(fill=Species),position='dodge',color='black',alpha=0.5) + theme_bw() + scale_fill_brewer(palette = "Dark2",limits=c('BRBO','FRsp','MABO','NOsp','RFBO'),labels=c('brown booby','frigatebirds','masked booby','noddies','red-footed booby')) + scale_color_brewer(palette = "Dark2") 
plot.barplot.veg <- plot.barplot.veg + ylab('Total number of breeding pairs counted')+xlab('') + theme(legend.position = c(0.15, 0.85),text = element_text(size = 15))
plot.barplot.veg



 ###Early response to eradication of main seabird species

##Model computations



priors <- get_prior(formula = abundance ~ Species*eradication +(1|transect) +(1|Hab),
                    data=tableau.oiseaux.species)
priors

priors.brms <- c(prior("normal(0,5)", class = "b", coef = eradication),  
                 prior("normal(0,5)", class = "b", coef = SpeciesRFBO),
                 prior("normal(0,5)", class = "b", coef = SpeciesFRsp),
                 prior("normal(0,5)", class = "b", coef = SpeciesMABO),
                 prior("normal(0,5)", class = "b", coef = SpeciesNOsp),
                 prior("normal(0,5)", class = "b", coef = SpeciesRFBO:eradication),
                 prior("normal(0,5)", class = "b", coef = SpeciesFRsp:eradication),
                 prior("normal(0,5)", class = "b", coef = SpeciesMABO:eradication),
                 prior("normal(0,5)", class = "b", coef = SpeciesNOsp:eradication))




##Model computations






model.brms.H1.raw <-brm(formula = abundance ~ Species*eradication +(1|transect) +(1|Hab),
                    family = negbinomial(link = "log", link_shape = "log"),
                    control = list(adapt_delta = 0.99,max_treedepth = 15),
                    prior = priors.brms,   
                    data=tableau.oiseaux.species.raw,
                    save_pars = save_pars(all = TRUE)) 





##Model analysis
##saveRDS(model.brms.H1.raw,'model_brms_H1_raw.RDS')
##model.brms.H1 <- readRDS('model_brms_H1.RDS')
##model.brms.H1.raw <- readRDS('model_brms_H1_raw.RDS')



model.brms <- model.brms.H1.raw
data.FixedEffect <- as.data.frame(fixef(model.brms))
Covariates <- rownames(data.FixedEffect)
data.FixedEffect <- data.FixedEffect %>% mutate(Covariate = Covariates,Est.Error.2 = -Est.Error) #We create a dataframe containing the values of the parameters estimated with the confidence interval
plot.estimation.parameters.brms <- ggplot(data=data.FixedEffect,aes(x=Covariate,y=Estimate,ymin=Estimate-Est.Error,ymax=Estimate+Est.Error,dist = dist_normal(Estimate, Est.Error))) + geom_pointrange(size=0.5) + geom_hline(yintercept = 0 , linetype='dashed')+ coord_flip() + theme_bw() + geom_pointrange(aes(x=Covariate,y=Estimate,ymin=Q2.5,ymax=Q97.5),size=0.1) +  stat_dist_halfeye(position = "dodge",color='black',alpha=0.5) + scale_fill_grey(start = 0.4,end=0.5)
plot.estimation.parameters.brms <- plot.estimation.parameters.brms + labs(y=expression(~beta ~"effect size"),x='') + theme(legend.position = "none") + ylim(-5,5)
plot.estimation.parameters.brms

R.squared.bayes <- bayes_R2(model.brms)
R.squared.bayes
R.squared.bayes <- as.data.frame(R.squared.bayes)
tab <- data.frame(Model=c("count ~ Species +(1|transect) +(1|Hab)","count ~ Species*eradication +(1|transect) +(1|Hab)"))
rownames(tab) <- c("H0","H1")
loo(model.brms.H1,model.brms.H0) #H1 has the lowest LOOIC so it is the best model
brms::pp_check(model.brms.H1)
ranef(model.brms)

model.check.raw <- createDHARMa(
  simulatedResponse = t(posterior_predict(model.brms.H1.raw)),
  observedResponse = tableau.oiseaux.species.raw$abundance,
  fittedPredictedResponse = apply(t(posterior_epred(model.brms.H1.raw)), 1, mean),
  integerResponse = TRUE)

plot(model.check.raw)
testDispersion(model.check.raw)
testZeroInflation(model.check.raw)

pp_check(model.brms, nsamples = 100) + xlim(0, 5)


new <- c('brown booby','frigatebirds','masked booby','noddies','red-footed booby')
names(new) <- c('BRBO','FRsp','MABO','NOsp','RFBO')

plot(conditional_effects(model.brms.H1.raw,effects = c("eradication:Species"),points = TRUE, rug = TRUE))[[1]] +
  facet_wrap(. ~ Species,scales = "free",labeller = labeller(Species = new)) + theme_bw()+
  theme(legend.position = c(0.8, 0.25),legend.title=element_blank(),text = element_text(size = 15))+
  labs(y='Estimated number of breeding pairs per 100 m² section',x='Number of years after eradication')+ scale_fill_brewer(palette = "Dark2",limits=c('BRBO','FRsp','MABO','NOsp','RFBO'),labels=c('brown booby','frigatebirds','masked booby','noddies','red-footed booby')) + scale_color_brewer(palette = "Dark2",limits=c('BRBO','FRsp','MABO','NOsp','RFBO'),labels=c('brown booby','frigatebirds','masked booby','noddies','red-footed booby'))


##Supplementary

conditionnal_response_raw <- emtrends(model.brms.H1.raw, ~ eradication:Species,var='eradication')
conditionnal_response_raw
###Late response to eradication of seabird species

##Other survey compilation


donnees.surprise.recentes <- read.csv("recent_data_surprise.csv",sep='')
donnees.surprise.recentes <- as.data.frame(donnees.surprise.recentes)
donnees.surprise <- donnees.surprise.recentes %>% 
  dplyr::select(year,month,genera,species,couples) %>% 
  dplyr::filter(genera == 'sula' | genera == 'fregata' | genera == 'anous')

for(i in 1:length(donnees.surprise$species)){
  if(donnees.surprise$species[i]=='dactylatra'){
    donnees.surprise$species[i] <- 'dactylara'
  }
}
for(i in 1:length(donnees.surprise$species)){
  if(donnees.surprise$species[i]=='ariel'|donnees.surprise$species[i]=='minor'|donnees.surprise$species[i]=='fregata'|donnees.surprise$species[i]=='sp'){
    donnees.surprise$species[i] <- 'FRsp'
  }
}
for(i in 1:length(donnees.surprise$species)){
  if(donnees.surprise$species[i]=='minutus'|donnees.surprise$species[i]=='stolidus'){
    donnees.surprise$species[i] <- 'NOsp'
  }
}
for(i in 1:length(donnees.surprise$species)){
  if(donnees.surprise$species[i]=='sula'){
    donnees.surprise$species[i] <- 'RFBO'
  }
}
for(i in 1:length(donnees.surprise$species)){
  if(donnees.surprise$species[i]=='leucogaster'){
    donnees.surprise$species[i] <- 'BRBO'
  }
}
for(i in 1:length(donnees.surprise$species)){
  if(donnees.surprise$species[i]=='dactylara'){
    donnees.surprise$species[i] <- 'MABO'
  }
}
donnees.surprise$couples <- as.numeric(donnees.surprise$couples)
donnees.surprise <- ddply(donnees.surprise, .(species,year,month,species), summarise,nbr_couples=sum(couples))
donnees.surprise.FRsp <- donnees.surprise %>% filter(species=='FRsp') %>% filter(month==9) %>% dplyr::select(-month)
donnees.surprise.FRsp.bis <- donnees.surprise %>% filter(species=='FRsp') %>% filter(month!=9) %>% filter(year!=2017) %>% mutate(nbr_couples=NA)
donnees.surprise <- donnees.surprise %>% filter(species!='FRsp')
donnees.surprise <- rbind(donnees.surprise,donnees.surprise.FRsp.bis)
donnees.surprise <- ddply(donnees.surprise, .(species,year), summarise,nbr_couples=max(nbr_couples))
donnees.surprise <- rbind(donnees.surprise,donnees.surprise.FRsp)
donnees.surprise <- donnees.surprise %>% mutate(condition=rep('after eradication',length(donnees.surprise$year)))
donnees.anciennes <- data.frame(c('RFBO','RFBO','BRBO','BRBO','FRsp','FRsp','NOsp','NOsp','MABO','MABO'),c(1995,1996,1995,1996,1995,1996,1995,1996,1995,1996),c(2883,2139,365,352,383,397,400,181,59,40),rep('before eradication',10))
colnames(donnees.anciennes) <- c('species','year','nbr_couples','condition')
donnees.anciennes <- donnees.anciennes %>% mutate(condition=rep('before eradication',length(donnees.anciennes$year)))
donnees.surprise <- rbind(donnees.surprise,donnees.anciennes)

##on prend le max par ann?e en terme de ph?no

data.area <- donnees.oiseaux.raw.models %>% dplyr::select(Year,Hab) %>% mutate(quantity=1)
data.area <- ddply(data.area,.(Year),summarise,quantity = sum(quantity))
data.area <- arrange(data.area,Year)
data.area <- data.area %>% mutate(Area = 100*quantity)
data.area <- ddply(data.area,.(Year),summarise,Area = sum(Area))
data.area <- data.area %>% mutate(alpha=204000/Area)
data.total.couples <- ddply(tableau.oiseaux.species.fig1,.(Year,Species),summarize,nbr_couples = sum(Number_of_couples))
data.dyna <- merge(data.total.couples,data.area,by='Year')
data.dyna <- data.dyna %>% mutate(estimate=round(alpha*nbr_couples))
data.dyna$year <- as.numeric(data.dyna$Year)+2001 



compa.dyna <- ddply(donnees.surprise,.(year,condition,species),summarize,nbr_couples=round(sum(nbr_couples)))
data.dyna.compa <- data.dyna %>% dplyr::select(year,estimate,Species)
colnames(data.dyna.compa) <- c('year','nbr_couples','species')
data.dyna.compa <- cbind(data.dyna.compa,condition=c(rep('before eradication',20),rep('after eradication',20)))
compa.dyna <- rbind(data.dyna.compa,compa.dyna)

compa.dyna$year <- as.numeric(compa.dyna$year)
compa.dyna <- compa.dyna %>% filter(species!='Community')
new <- c('brown booby','frigatebirds','masked booby','noddies','red-footed booby')
names(new) <- c('BRBO','FRsp','MABO','NOsp','RFBO')


compa.dyna$condition = factor(compa.dyna$condition , levels=c('before eradication','after eradication'))
plot.fig5 <- ggplot(data = compa.dyna)+ scale_fill_brewer(palette = "Dark2") + 
  theme_bw()+
  geom_col(aes(x=year,y=nbr_couples,fill=condition,),color='black',alpha=0.5,width = 0.8,
           position = position_dodge2(width = 0.8, preserve = "single")) + 
  scale_fill_manual(values=c("#D95F02","#1B9E77"))+
  facet_grid(species~condition,labeller = labeller(species = new),scales = 'free',space = "free_x") +
  labs(y='Total number of breeding pairs (counted or extrapolated)',x='Year')+ 
  theme(legend.position = 'bottom',legend.title=element_blank(),text = element_text(size = 15))


plot.fig5

##Environmental effect on Community

tableau.oiseaux.species.env <- ddply(tableau.oiseaux.species.fig1,.(Year), summarize, Number_of_couples = sum(Number_of_couples))
tableau.oiseaux.species.env$Year <- as.numeric(tableau.oiseaux.species.env$Year)+2001
donnees.env <- data.frame(Year =donnees.env$Year,
                          Ch65 =donnees.env$chlomax65_S,
                          Ch200 = donnees.env$chlomax200_S,
                          Precip =donnees.env$Precipitation_S,
                          Wind = donnees.env$Wind_S,
                          mmT = donnees.env$mmT_S,
                          meanT = donnees.env$MeanT_S,
                          Anom1 =donnees.env$AnomSOI_S,
                          Anom2 = donnees.env$AnomNINO4_S)
tableau.oiseaux.species.env <- merge(tableau.oiseaux.species.env,donnees.env, by='Year')






plot.1 <- ggplot(data = tableau.oiseaux.species.env,aes(x=Ch65,y=Number_of_couples)) + geom_jitter(size=2) + geom_smooth(linetype='dotted',color='black', se=FALSE) + theme_bw() + ylab('Total number of breeding pairs counted')
plot.1
plot.3 <- ggplot(data = tableau.oiseaux.species.env,aes(x=Precip,y=Number_of_couples))  + geom_jitter(size=2) + geom_smooth(linetype='dotted',color='black', se=FALSE) + theme_bw()+ ylab('')
plot.3
plot.4 <- ggplot(data = tableau.oiseaux.species.env,aes(x=Wind,y=Number_of_couples))+ geom_jitter(size=2) + geom_smooth(linetype='dotted',color='black', se=FALSE) + theme_bw()+ ylab('')
plot.4
plot.5 <- ggplot(data = tableau.oiseaux.species.env,aes(x=mmT,y=Number_of_couples)) +geom_jitter(size=2) + geom_smooth(linetype='dotted',color='black', se=FALSE) + theme_bw()+ ylab('Total number of breeding pairs counted')
plot.5
plot.6 <- ggplot(data = tableau.oiseaux.species.env,aes(x=meanT,y=Number_of_couples)) + geom_jitter(size=2) + geom_smooth(linetype='dotted',color='black', se=FALSE) + theme_bw()+ ylab('')
plot.6
plot.7 <- ggplot(data = tableau.oiseaux.species.env,aes(x=Anom1,y=Number_of_couples)) + geom_jitter(size=2)  + geom_smooth(linetype='dotted',color='black', se=FALSE) + theme_bw()+ ylab('')
plot.7
plot_grid(plot.1,plot.3,plot.4,plot.5,plot.6,plot.7)
##

