# create data for migtree viz 
# this hasn't been tested...copied over from previous location - file paths may be out of date 

##################################################################################
##################################################################################
# 1) WRANGLE 
#################################################################################

library(data.table)
library(stringi)
library(usmap)
library(haven)
library(dplyr)
cfips = read_sas("inputs/county_fips.sas7bdat")




infile_names <- list.files(path = "inputs/IRSmig1120", pattern= 'countyinflow', full.names = F, recursive = F)
outfile_names <- list.files(path = "inputs/IRSmig1120", pattern= 'countyoutflow', full.names = F, recursive = F)


years = c("1112", "1213", "1314", "1415", "1516", "1617", "1718", "1819", "1920")

#intot = list()
#outtot = list()

inout = data.frame() 

for (i in 1:length(years)){
  in1 = read.csv(paste0("inputs/IRSmig1120/", infile_names[i]))
  in1$y2_statefips = stringr::str_pad(in1$y2_statefips, 2, pad = "0")
  in1$y2_countyfips = stringr::str_pad(in1$y2_countyfips, 3, pad = "0")
  in1$y1_statefips = stringr::str_pad(in1$y1_statefips, 2, pad = "0")
  in1$y1_countyfips = stringr::str_pad(in1$y1_countyfips, 3, pad = "0")
  
  in1$destid = paste0(in1$y2_statefips, in1$y2_countyfips)
  in1$origid = paste0(in1$y1_statefips, in1$y1_countyfips)
  
  out1 = read.csv(paste0("inputs/IRSmig1120/", outfile_names[i]))
  out1$y2_statefips = stringr::str_pad(out1$y2_statefips, 2, pad = "0")
  out1$y2_countyfips = stringr::str_pad(out1$y2_countyfips, 3, pad = "0")
  out1$y1_statefips = stringr::str_pad(out1$y1_statefips, 2, pad = "0")
  out1$y1_countyfips = stringr::str_pad(out1$y1_countyfips, 3, pad = "0")
  
  out1$destid = paste0(out1$y2_statefips, out1$y2_countyfips)
  out1$origid = paste0(out1$y1_statefips, out1$y1_countyfips)
  
  
  in1sub = in1[,c("destid", "n1", "n2", "origid", "agi")]
  in1s = in1sub %>% group_by(destid) %>% summarise(totin = sum(n2), #n2 represents an estimate of individuals (based on dependents claimed) who migrated
                                                   inagi = sum(agi))
  
  out1sub = out1[,c("destid", "n1", "n2", "origid", "agi")]
  out1s = out1sub %>% group_by(origid) %>% summarise(totout = sum(n2),
                                                     outagi = sum(agi))
  inouttemp = merge(out1s, in1s, by.x = "origid", by.y = "destid") 
  inouttemp$netmig = inouttemp$totin - inouttemp$totout
  inouttemp$netagi = inouttemp$inagi - inouttemp$outagi
  inouttemp$year = years[i]
  inouttemp = inouttemp[!endsWith(inouttemp$origid, "000"),]
  
  
  inout = rbind(inout,inouttemp)
  
}

#positive or negative netmigration and positive or negative agi 
inout$pn_mig = ifelse(inout$netmig>0, 1, -1)
inout$pn_agi = ifelse(inout$netagi>0, 1, -1)





#ADD IRS DATA FOR 2008 thru 2011 
#(still need 1990-2008....many files, very messy) 

#0809, 0910 are saved the same way as the latest datasets, but they have different col names 
years811 = c("0809", "0910", "1011")
for (i in 1:length(years811)) {
  in1 = read.csv(paste0("inputs/IRSmig0811/countyinflow",years811[i], ".csv"))
  in1$y2_statefips = stringr::str_pad(in1$State_Code_Dest, 2, pad = "0")
  in1$y2_countyfips = stringr::str_pad(in1$County_Code_Dest, 3, pad = "0")
  in1$y1_statefips = stringr::str_pad(in1$State_Code_Origin, 2, pad = "0")
  in1$y1_countyfips = stringr::str_pad(in1$County_Code_Origin, 3, pad = "0")
  
  in1$destid = paste0(in1$y2_statefips, in1$y2_countyfips)
  in1$origid = paste0(in1$y1_statefips, in1$y1_countyfips)
  
  out1 = read.csv(paste0("inputs/IRSmig0811/countyoutflow",years811[i], ".csv"))
  out1$y2_statefips = stringr::str_pad(out1$State_Code_Dest, 2, pad = "0")
  out1$y2_countyfips = stringr::str_pad(out1$County_Code_Dest, 3, pad = "0")
  out1$y1_statefips = stringr::str_pad(out1$State_Code_Origin, 2, pad = "0")
  out1$y1_countyfips = stringr::str_pad(out1$County_Code_Origin, 3, pad = "0")
  
  out1$destid = paste0(out1$y2_statefips, out1$y2_countyfips)
  out1$origid = paste0(out1$y1_statefips, out1$y1_countyfips)
  
  
  in1 = in1[!(in1$y1_statefips>57 | in1$y1_statefips == 00),]
  in1 = in1[!(in1$y2_statefips>57 | in1$y2_statefips == 00),]
  out1 = out1[!(out1$y1_statefips>57 | out1$y1_statefips == 00),]
  out1 = out1[!(out1$y2_statefips>57 | out1$y2_statefips == 00),]
  
  
  
  in1sub = in1[,c("destid", "Return_Num", "Exmpt_Num", "origid", "Aggr_AGI")]
  in1s = in1sub %>% group_by(destid) %>% summarise(totin = sum(Exmpt_Num), #exmpt_num is the number of dependents who migrated 
                                                   #estpopin = sum(Return_Num, Exmpt_Num), #this is the sum of returns and exemptions 
                                                   inagi = sum(Aggr_AGI))
  
  out1sub = out1[,c("destid", "Return_Num", "Exmpt_Num", "origid", "Aggr_AGI")]
  out1s = out1sub %>% group_by(origid) %>% summarise(totout = sum(Exmpt_Num),
                                                     outagi = sum(Aggr_AGI))
  #estpopout = sum(Return_Num, Exmpt_Num))
  inouttemp = merge(out1s, in1s, by.x = "origid", by.y = "destid") 
  inouttemp$netmig = inouttemp$totin - inouttemp$totout
  inouttemp$netagi = inouttemp$inagi - inouttemp$outagi
  inouttemp$year = years811[i]
  #positive or negative netmigration and positive or negative agi 
  inouttemp$pn_mig = ifelse(inouttemp$netmig>0, 1, -1)
  inouttemp$pn_agi = ifelse(inouttemp$netagi>0, 1, -1)
  
  inouttemp = inouttemp[!endsWith(inouttemp$origid, "000"),]
  
  
  inout = rbind(inout,inouttemp)
  
}



#################################################################################

# 2) Function to explore county-specific trends in netmig and netagi 
#Faster attempt

inout$year = as.numeric(inout$year)
angle = 20*pi/180

inout$prop = abs(inout$netmig/(inout$totin + inout$totout))

library(dplyr)

# add initial conditions 
inoutp = inout %>% group_by(origid) %>% 
  arrange(year) %>% 
  summarise(pn_mig = c(0, pn_mig), 
            prop = c(0, prop), 
            pn_agi = c(0, pn_agi), 
            year = c(0, year))  



plotdat = inoutp %>% dplyr::group_by(origid) %>% 
  dplyr::mutate(angle_mig = pn_mig * angle,
                newangle_mig = cumsum(angle_mig), 
                newx_mig = sin(newangle_mig),
                x_adj_mig = cumsum(lag(newx_mig, default = 0, order_by = year)),
                newy_mig = cos(newangle_mig),
                y_adj_mig = cumsum(lag(newy_mig, default = 0, order_by = year)),
                
                angle_agi = pn_agi * angle,
                newangle_agi = cumsum(angle_agi), 
                newx_agi = sin(newangle_agi),
                x_adj_agi = cumsum(lag(newx_agi, default = 0, order_by = year)),
                newy_agi = cos(newangle_agi),
                y_adj_agi = cumsum(lag(newy_agi, default = 0, order_by = year)))


#remove counties that do not have all years of data 
plotdat <- plotdat %>% group_by(origid) %>%
  mutate(Var=n_distinct(year)) %>%
  filter(Var>12) %>% select(-Var)


urbcodes = readxl::read_excel("inputs/NCHSURCodes2013.xlsx")
urbcodes$fips = stringr::str_pad(urbcodes$`FIPS code`, 5, pad = "0")
urbcodes$name = urbcodes$`County name`
urbcodes$urbcode = urbcodes$`2013 code`
urbcodes$statename = urbcodes$`State Abr.`
urb = urbcodes[,c("fips", "name", "urbcode", "statename")]

ppu = merge(plotdat, urb, by.y = "fips", by.x = "origid", all.x = TRUE)
ppu2 = ppu[!(ppu$pn_mig == 0),]

#save(ppu2, file = "migtree.Rdata")