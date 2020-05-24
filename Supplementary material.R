library(extrafont)
library(tidyverse)
library(ggplot2)
library(jtools)
library(ggstance)
library(GGally)
library(car)
library(sjPlot)
library(olsrr)
library(broom)

#ggplot(data=junk)+
#  geom_density(aes(x=V1, colour= "Fake news"), lwd=1)

#plot(dta_all$year, junk$V1, type="l", col="black",lwd=3, 
#     xlab="time", ylab="Junk News", main="Fake News Spread")

dta_all <- `V-Dem-CY-Core-v10` %>%
  dplyr::filter(year>1989) %>%
  dplyr::filter(country_text_id == "BRA")

pre_dem <- `V-Dem-CY-Core-v10` %>%
  dplyr::filter(year>1890) %>%
  dplyr::filter(country_text_id == "BRA")

######################################### TIME SERIES OF MEDIA ##############################################

ggplot(data=dta_all, aes(x=year)) + 
  geom_line(aes(y = v2mecenefm_mean, 
            color = "Government effort for \ncensorship\n"), linetype="twodash",lwd=1.4) + 
  geom_line(aes(y = v2mecenefi_mean, 
            color="Censorship effort in \nInternet\n"), linetype="longdash",lwd=1.4) +
  geom_line(aes(y = v2meharjrn_mean, 
            color="\nJournalist Harassment\n"), linetype="dotdash",lwd=1.4) +
  geom_line(aes(y = v2mebias_mean, 
            color="\nMedia bias\n"), linetype="solid",lwd=1.4) +
  ggtitle("Figure 3 - Brazilian V-dem Scores on Media Indices")+
  labs(x="Year", 
       y="Vdem Score (0 to 4)",
       color="V-Dem indicators")+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))+
  xlim(1990,2019)+
  ylim(0,4)


ggplot(data=dta_all, aes(x=year)) + 
  geom_line(aes(y = v2mecrit_mean, color="Media critical to \ngovernment\n"),
            lwd=1.4, linetype="twodash") + 
  geom_line(aes(y = v2merange_mean, color="Different media \noutlet perspectives\n"),
            lwd=1.4,linetype="longdash") +
  geom_line(aes(y = v2meslfcen_mean, color="Media \nself-censorship\n"),
            lwd=1.4,linetype="solid") +
  ggtitle("Figure 4 - Brazilian V-dem Scores on Media Indices")+
  labs(x="Year", y="Vdem Score (0 to 3)")+
  labs(x="Year", 
       y="Vdem Score (0 to 3)",
       color="V-Dem Indicators")+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))+
  xlim(1990,2019)+
  ylim(0,3)

ggplot(data=dta_all, aes(x=year)) + 
  geom_line(aes(y = v2x_polyarchy, color="Polyarchy"),
            lwd=1.4, linetype="solid") + 
  geom_line(aes(y = v2x_libdem, color="Liberal Democracy"),
            lwd=1.4,linetype="solid") +
  geom_line(aes(y = v2x_partipdem, color="Participatory Democracy"),
            lwd=1.4,linetype="solid") +
  ggtitle("Figure 2 - Brazilian V-dem Scores \non Democracy Indices")+
  labs(x="Year", y="Vdem Score (0 to 3)")+
  labs(x="Year", 
       y="Vdem Score (0 to 1)",
       color="V-Dem Indicators")+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))+
  xlim(1990,2019)+
  ylim(0,1)

#Government censorship effort to media
ggplot(data=dta_all, aes(x=year, y=v2mecenefm_mean, group=1)) +
  geom_line(color="red")+
  geom_point()+
  ylab("Censorship effort toward media")+
  xlim(1989,2019)+
  ylim(0,4)

#Internet Censorship effort
ggplot(data=dta_all, aes(x=year, y=v2mecenefi_mean, group=1)) +
  geom_line(color="red")+
  geom_point()+
  xlim(1988,2019)+
  ylim(0,4)

#Print/broadcast media critical to government (0 = few critical outlets)
ggplot(data=dta_all, aes(x=year, y=v2mecrit_mean, group=1)) +
  geom_line(color="red")+
  geom_point()+
  xlim(1988,2019)+
  ylim(0,3)

#Print/Broadcast media perspectives (0 = major media represent only the government perspective)
ggplot(data=dta_all, aes(x=year, y=v2merange_mean, group=1)) +
  geom_line(color="red")+
  geom_point()+
  xlim(1988,2019)+
  ylim(0,3)

#Journalist harassment (0 = journalistic acitivies are too dangerous)
ggplot(data=dta_all, aes(x=year, y=v2meharjrn_mean, group=1)) +
  geom_line(color="red")+
  geom_point()+
  xlim(1988,2019)+
  ylim(0,4)

#Media self-censorship (0 = self-censorship is complete and thorough)
ggplot(data=dta_all, aes(x=year, y=v2meslfcen_mean, group=1)) +
  geom_line(color="red")+
  geom_point()+
  xlim(1988,2020)+
  ylim(0,3)

#Media bias (0 = biased)
ggplot(data=dta_all, aes(x=year, y=v2mebias_mean, group=1)) +
  geom_line(color="red")+
  geom_point()+
  xlim(1988,2020)+
  ylim(0,4)

###############################Correlation and causality of press for democracy###################################

OLS1=lm(v2xcl_rol~v2mecenefm_mean+v2mecenefi_mean+v2mecrit_mean+
          v2merange_mean+v2meharjrn_mean+v2meslfcen_mean+v2mebias_mean, data=dta_all)
summ(OLS1)
vif(OLS1)
par(mfrow =c(2,2))
plot(OLS1)

OLS11=lm(v2xcl_rol~v2merange_mean+v2meharjrn_mean+v2mecenefm_mean, data=dta_all) #final
summ(OLS11)
vif(OLS11)
par(mfrow =c(2,2))
plot(OLS11)

#effect_plot(OLS11, pred = v2mecenefm_mean, interval = TRUE, plot.points = TRUE)
#effect_plot(OLS11, pred = v2merange_mean, interval = TRUE, plot.points = TRUE)
#effect_plot(OLS11, pred = v2meharjrn_mean, interval = TRUE, plot.points = TRUE)

OLS2=lm(v2x_jucon~v2mecenefm_mean+v2mecenefi_mean+v2mecrit_mean+
          v2merange_mean+v2meharjrn_mean+v2meslfcen_mean+v2mebias_mean, data=dta_all)
summ(OLS2)
vif(OLS2)
par(mfrow =c(2,2))
plot(OLS2)

OLS22=lm(v2x_jucon~v2mecenefm_mean+v2mecrit_mean+v2merange_mean+v2meharjrn_mean, data=dta_all) #final
summ(OLS22)
vif(OLS22)
par(mfrow =c(2,2))
plot(OLS22)

OLS3=lm(v2xlg_legcon~v2mecenefm_mean+v2mecenefi_mean+v2mecrit_mean+
          v2merange_mean+v2meharjrn_mean+v2meslfcen_mean+v2mebias_mean, data=dta_all)
summ(OLS3)
vif(OLS3)
par(mfrow =c(2,2)
plot(OLS3)

OLS33=lm(v2xlg_legcon~v2meharjrn_mean+v2mecenefm_mean+v2mecrit_mean+v2merange_mean, data=dta_all) #final
summ(OLS33)
vif(OLS33)
par(mfrow=c(2,2))
plot(OLS33)

dta_all_outliers <- dta_all[-c(1,2, 27, 28, 29, 30), ] #To check diagnostics without outliers


#############################################GARBAGE#############################################
