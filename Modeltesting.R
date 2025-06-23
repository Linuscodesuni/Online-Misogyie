
#clm model, Prädiktor: Parteizugehörigkeit
ordinal= clm(TimesMisogyny ~  Parteizugehörigkeit, data = df)
summary(ordinal)

vglm= vglm(TimesMisogyny ~  Parteizugehörigkeit,cumulative(parallel = TRUE, reverse = FALSE), data=df)
summary(vglm)

#clm model, Prädiktoren: Parteizugehörigkeit und Alter
ordinal2= clm(TimesMisogyny ~  Parteizugehörigkeit + Age, data = df)
summary(ordinal2)

vglm2= vglm(TimesMisogyny ~  Parteizugehörigkeit + Age,cumulative(parallel = TRUE, reverse = FALSE), data=df)
summary(vglm2)

#vglm ohne proportional odds
vglm3= vglm(TimesMisogyny ~  Parteizugehörigkeit + Age,cumulative(parallel = FALSE, reverse = FALSE), data=df)
summary(vglm3)

AIC(vglm2)
AIC(vglm3)
AIC(ordinal2)
AIC(modelt)

anova(vglm3, vglm2)
anova(ordinal2, vglm2)

#semi 
ordinal2_n= clm(TimesMisogyny ~ Parteizugehörigkeit,
                nominal = ~ Parteizugehörigkeit,
                data = df)
summary(ordinal2_n)

anova(ordinal, ordinal2_n)

#Parteizugehörigkeit und Politische Ebene als Kontrollvariable
ordinal3= clm(TimesMisogyny ~  Parteizugehörigkeit * Age, data = df)
summary(ordinal3)

#Age regrouping
unique(df$Age)
df$AgeGruppe <- ifelse(df$Age %in% c("18-24", "25-34", "35-44"), 
                       "unter 44", 
                       "über 44")

#Parteizugehörigkeit und AgeGruppe Interaktion
ordinal4= clm(TimesMisogyny ~  Parteizugehörigkeit * AgeGruppe, data = df)
summary(ordinal4)

anova(ordinal2,ordinal3)

polr= polr(TimesMisogyny ~  Age, data = df)
brant(polr)

table(df$TimesMisogyny, df$Parteizugehörigkeit)

AIC(ordinal2)
AIC(vglm2)
AIC(vglm3)
AIC(polr)