##########################################################

 # Title: Appearing moderate or radical? Radical left party success and the two-dimensional political space
 # Original Author: Werner Krause
 # Replication Author: Luka De Lacey

##########################################################

#######################
# Setup
#######################


cat( '\14' )
rm( list = ls( ))
pkgs <- c( 'dplyr' , 'magrittr' , 'ggplot2')
lapply( pkgs , library , c = TRUE )

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

load( 'rlp2d_krause_wep.Rdata' )
source( 'rlp2d_krause_wep_functions.R' )
s <- "$$\\surd$$"


#######################
# Script functions
#######################

sessioninfo::platform_info( )

sessioninfo::package_info( pkgs = c( 'ggplot2' , 'dplyr' , 'magrittr' , 'stargazer'
                                     , 'multiwayvcov' , 'lmtest' , 'stringr' , 'tidyverse')
                           , dependencies = F )


#######################
# REPLICATION: ADDING MAIN CENTRE RIGHT
#######################

#load datasets
mp <- read.csv("MPDataset_MPDS2025a.csv")
pbe <- read.csv("Party by election.csv")

#
pbe <- pbe %>%
select(iso, country, election, year, right_code, right_1) %>% 
  mutate(iso = na_if(trimws(iso), "")) %>% 
  na.omit()

#Mutate to change "Family of the Irish" to FG in abbreviations + 

mp <- mp %>%
  mutate(partyabbrev = case_when(partyname == "Family of the Irish" ~ "FG",
                                 TRUE ~ partyabbrev))

mp <- mp %>%
  mutate(edate = dmy(edate),
         year  = as.integer(substr(as.character(date), 1, 4))) %>%
  arrange(countryname, year, edate, partyabbrev, partyname)

# Change pbe to align with abbreviations in mp
pbe <- pbe %>%
  mutate(partyabbrev = case_when(
   country == "Germany"  & right_code == "relig1"   ~ "CDU/CSU",
   country == "Greece" & right_code == "conserv3"   ~ "ND",
   country == "Austria" & right_code == "relig1"    ~ "ÖVP",
   country == "Spain" & right_code == "conserv2"   ~ "PP",
   country == "Finland" & right_code == "conserv1"   ~ "KK",
   country == "Iceland" & right_code == "conserv1"   ~ "Sj",
   country == "Ireland" & right_code == "relig1"   ~ "FG",
   country == "Norway" & right_code == "conserv1"   ~ "H",
   country == "Luxembourg" & right_code == "relig1"   ~ "CSV/PCS",
   country == "Sweden" & right_code == "conserv1"   ~ "MSP",
   country == "Denmark" & right_code == "liberal2"   ~ "V",
   
   #Countries that vary
   country == "Portugal" & year != 2015 & right_code == "liberal1"   ~ "PSD",
   country == "Portugal" & year == 2015 & right_code == "liberal1"   ~ "PàF",
   country == "Switzerland" & year >= 2011 & right_code == "liberal1"   ~ "FDP/PLR",
   country == "Switzerland" & year <= 2007 & right_code == "liberal1"   ~ "FDP/PRD",
   country == "Belgium" & year <= 1999 & right_code == "relig3"   ~ "PSC",
   country == "Belgium" & year >= 2003 & right_code == "relig3"   ~ "cdH",
   country == "Italy" & year == 1992 & right_code == "relig1" ~ "DC",
   country == "Italy" & year > 1992 & right_code == "conserv1" ~ "FI",
   country == "Italy" & right_code == "conserv4" ~ "PdL",
   country == "France" & year <= 1997 & right_code == "conserv2"   ~ "RPR",
   country == "France" & year >= 2002 & right_code == "conserv2"   ~ "UMP",
   country == "France" & right_code == "liberal3"   ~ "LREM",
   country == "Netherlands" & right_code == "relig5"   ~ "CDA",
   country == "Netherlands" & right_code == "liberal1"   ~ "VVD"))


# adjust pbe var names
head(pbe)

pbe <- pbe %>%
  rename(countryname = country,
         edate = election,
         iso2c = iso) %>%
  mutate(edate = dmy(edate),
         year = as.integer(year)) %>%
  arrange(countryname, year, edate, partyabbrev)

# add cyrprus + Iceland rows to pbe

Cyprus_rows <- tribble(
  ~countryname,    ~year, ~edate,
  "Cyprus",   1996,  "1996-05-26",
  "Cyprus",   2001,  "2001-05-27",
  "Cyprus",    2006,  "2006-05-21",
  "Cyprus",    2011,  "2011-05-22",
  "Cyprus",    2016,  "2016-05-22") %>%
  mutate(
    edate      = as.Date(edate),
    year       = as.integer(year),
    partyabbrev = "DISY",
    iso2c = "CY")

Iceland_rows <- tribble(
  ~countryname,    ~year, ~edate,
  "Iceland",   1991,  "1991-04-20",
  "Iceland",   1995,  "1995-04-08") %>%
  mutate(
    edate      = as.Date(edate),
    year       = as.integer(year),
    partyabbrev = "Sj",
    iso2c = "IS")

pbe <- pbe %>%
  bind_rows(Cyprus_rows) %>%
  bind_rows(Iceland_rows) %>%
  arrange(countryname, year, edate, partyabbrev)

#######################
# REPLICATION: CODE MAIN CENTRE RIGHT POSITIONING
#######################

#identify relevant MP codes used by Krause (2020)
eleft<- c("per403", "per404", "per406", "per409",
          "per412", "per413", "per415", "per504", 
          "per701")

eright <- c("per401", "per402", "per407", "per410",
            "per411", "per414", "per505", "per702")

libl <- c("per103", "per105", "per106", "per107",
          "per202", "per416", "per501", "per602",
          "per604", "per606" ,"per607", "per705")

authr <- c("per104", "per109", "per305", "per601",
           "per603", "per605", "per608")

#merge datasets

merged <- pbe %>%
  left_join(mp, by = c("countryname", "year", "edate", "partyabbrev")) %>%
  select(iso2c, countryname, year, edate, partyabbrev, partyname, rile,
         eleft, libl, eright, authr)

# Follow coding of Krause (2020), Benoit et al. (2012), Lowe et al. (2011) -
# i.e. using relevant variables, then log transform them.

#prep new colums
mcr_df <- merged %>%
  mutate(ec_left = rowSums(select(., all_of(eleft))),
         ec_right = rowSums(select(., all_of(eright))),
         lib_left = rowSums(select(., all_of(libl))),
         auth_right = rowSums(select(., all_of(authr))))

#log transformation
mcr_df <- mcr_df %>%
  mutate(mcr.eco.rile_logit = log((ec_right + 0.5) / (ec_left + 0.5)),
         mcr.noneco.rile_logit = log((auth_right + 0.5) / (lib_left + 0.5))) %>%
  select(iso2c, countryname, year, edate, partyabbrev, partyname,
         mcr.eco.rile_logit, mcr.noneco.rile_logit)

#merge with main ds

ds <- ds %>%
  left_join(mcr_df, by = c("iso2c", "edate"))

# add SD_MCR ideological distance

ds <- ds %>%
  mutate(
    sd_mcr_diffe = abs(sd.eco.rile_logit - mcr.eco.rile_logit),
    sd_mcr_diffc = abs(sd.noneco.rile_logit - mcr.noneco.rile_logit)
  )

#######################
# Figure 1: Economic and non-economic positions of West European radical left parties, 1990-2018
#######################

ds %>%
  ggplot( aes( x = rl.eco.rile_logit , y = rl.noneco.rile_logit )) +
  geom_point( ) +
  scale_x_continuous( limits = c( -5.0937 , 5.6168 ) , name = 'RLP Economic positions' ) +
  scale_y_continuous( limits = c( -6.9992 , 4.9052 ) , name = 'RLP Non-economic positions' )

#######################
# Table 1: Summary Statistics
#######################

ds.act <- ds %>% select( rl.v_share_wgt , rl.v_share_wgt_l
                         , rl.eco.rile_logit , rl.noneco.rile_logit
                         , sd.eco.rile_logit , sd.noneco.rile_logit
                         , sd.gov.wogc , rl.gov_l , rl.supp_l
                         , unem_ilo , gdp_wb_log , comp.v
                         , turnout, mcr.eco.rile_logit, mcr.noneco.rile_logit,
                         sd_mcr_diffe, sd_mcr_diffc) %>% na.omit( )

ds.act %>% stargazer::stargazer( summary = T , header = F , align = T
                                 , font.size = 'footnotesize' , no.space = T )

#######################
# Table 2: Regression resilts
#######################

ds.act <- ds %>%
  select( iso2c , edate , rl.govelec_id , rl.pname , sd.partyname
          , rl.v_share_wgt , rl.v_share_wgt_l
          , rl.eco.rile_logit , rl.noneco.rile_logit
          , sd.eco.rile_logit , sd.noneco.rile_logit
          , sd.gov.wogc , rl.gov_l , rl.supp_l
          , unem_ilo , gdp_wb_log , comp.v , turnout
          , closest.eco.rile_logit , closest.noneco.rile_logit
          , rl.eco.entre , rl.non.eco.entre, mcr.eco.rile_logit, 
          mcr.noneco.rile_logit, sd_mcr_diffe, sd_mcr_diffc) %>%
  na.omit( ) %>%
  mutate( eco.ia = rl.eco.rile_logit*sd.eco.rile_logit
          , noneco.ia = rl.noneco.rile_logit*sd.noneco.rile_logit )

dv = 'rl.v_share_wgt'
fe = 'rl.govelec_id'
ia = NULL
c1 = 'edate'
c2 = 'rl.govelec_id'
cntrls = c( 'sd.gov.wogc' , 'rl.gov_l' , 'rl.supp_l' , 'unem_ilo' , 'gdp_wb_log'
            , 'comp.v' , 'turnout' , 'rl.v_share_wgt_l' )
m1 <- tw.cl.lm( ds.act , dv , cntrls , ia , fe , c1 , c2 )

cntrls = c( cntrls , c( 'rl.eco.rile_logit' , 'rl.noneco.rile_logit'
                        , 'sd.eco.rile_logit' , 'sd.noneco.rile_logit' ))

m2 <- tw.cl.lm( ds.act , dv , cntrls , ia , fe , c1 , c2 )

cntrls = c( cntrls , c( 'eco.ia' , 'noneco.ia' ))
m3 <- tw.cl.lm( ds.act , dv , cntrls , ia , fe , c1 , c2 )

stargazer::stargazer( m1$lm , m2$lm , m3$lm
                      , se = list( m1$tw.se[ , 2 ] , m2$tw.se[ , 2 ] , m3$tw.se[ , 2 ] )
                      , omit = 'rl.govelec_id'
                      , omit.stat = c( 'f' , 'ser' )
                      , font.size = 'footnotesize'
                      , no.space = T , align = T , header = F )

#######################
# Figure 2: Marginal effect of RLPs’ economic positions conditional on MLPs’economic positions
#######################

me.ds <- ia.plot.df( m3$lm , 'rl.eco.rile_logit' , 'sd.eco.rile_logit' , 'eco.ia'
                     , 'continuous' , m3$vcv , conf = .95 )

ia.plot( me.ds , ds.act , m3$tw.se , 'sd.eco.rile_logit' , 'eco.ia'
         , x.name = 'MLP economic position'
         , y.name = 'Marginal effect of RLP economic position \n on RLP vote share'
         , binw = .1
         , type = 'continuous'
         , incl.beta = F )


#######################
# REPLICATION Table 3: Modelling for ideological distance of SD and MCR
#######################

ds.rep <- ds.act %>% 
  mutate(sdmcr_eco.ia = rl.eco.rile_logit*sd_mcr_diffe,
          sdmcr_noneco.ia = rl.noneco.rile_logit*sd_mcr_diffc )

rep_cntrls = c( 'sd.gov.wogc' , 'rl.gov_l' , 'rl.supp_l' , 'unem_ilo' , 'gdp_wb_log'
            , 'comp.v' , 'turnout' , 'rl.v_share_wgt_l', 'rl.eco.rile_logit' , 'rl.noneco.rile_logit'
            , 'sd.eco.rile_logit' , 'sd.noneco.rile_logit', "sd_mcr_diffe","sd_mcr_diffc" )

m4 <- tw.cl.lm( ds.rep, dv , rep_cntrls , ia , fe , c1 , c2 )
m4

rep_cntrls = c( rep_cntrls , c( 'sdmcr_eco.ia' , 'sdmcr_noneco.ia' ))
m5 <- tw.cl.lm( ds.rep, dv , rep_cntrls , ia , fe , c1 , c2 )

stargazer::stargazer( m4$lm , m5$lm
                      , se = list( m4$tw.se[ , 2 ] , m5$tw.se[ , 2 ])
                      , omit = 'rl.govelec_id'
                      , omit.stat = c( 'f' , 'ser' )
                      , font.size = 'footnotesize'
                      , no.space = T , align = T , header = F )

# without sd positions directly

rep_cntrls2 = c( 'sd.gov.wogc' , 'rl.gov_l' , 'rl.supp_l' , 'unem_ilo' , 'gdp_wb_log'
                , 'comp.v' , 'turnout' , 'rl.v_share_wgt_l', 'rl.eco.rile_logit' , 'rl.noneco.rile_logit'
                , "sd_mcr_diffe","sd_mcr_diffc" )

m6 <- tw.cl.lm( ds.rep, dv , rep_cntrls2 , ia , fe , c1 , c2 )
m6

rep_cntrls2 = c( rep_cntrls2 , c( 'sdmcr_eco.ia' , 'sdmcr_noneco.ia' ))
m7 <- tw.cl.lm( ds.rep, dv , rep_cntrls2 , ia , fe , c1 , c2 )
m7

stargazer::stargazer(m4$lm , m5$lm, m6$lm, m7$lm
                     , se = list( m4$tw.se[ , 2 ] , m5$tw.se[ , 2 ],  m6$tw.se[ , 2 ] , m7$tw.se[ , 2 ])
                     , omit = 'rl.govelec_id'
                     , omit.stat = c( 'f' , 'ser' )
                     , font.size = 'footnotesize'
                     , no.space = T , align = T , header = F )
