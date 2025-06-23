#Finale modelle:


#Referenzkategorien festlegen
  df$Age= factor(df$Age)
  df$Age= relevel(df$Age, ref= "35-44")
  
  df$Parteizugehörigkeit= factor(df$Parteizugehörigkeit)
  df$Parteizugehörigkeit= relevel(df$Parteizugehörigkeit, ref = "CDU/CSU")
  
  df$Diskriminierung= factor(df$Diskriminierung)
  df$Diskriminierung= relevel(df$Diskriminierung, ref= "Nein")

model=clm(TimesMisogyny ~  Parteizugehörigkeit + Age + Diskriminierung, data=df)
model2=clm(TimesMisogyny ~  Parteizugehörigkeit + Age + Diskriminierung + PolitischeEbene_grouped, data=df)

df_SPD=df
df_SPD$Parteizugehörigkeit= relevel(df$Parteizugehörigkeit, ref= "SPD")

model3=clm(TimesMisogyny ~  Parteizugehörigkeit + Age + Diskriminierung, data=df_SPD)
model4=clm(TimesMisogyny ~  Parteizugehörigkeit + Age + Diskriminierung + PolitischeEbene_grouped, data=df_SPD)


summary(model)
summary(model2)
summary(model3)
summary(model4)

exp(confint(model))
exp(confint(model2))
exp(confint(model3))
exp(confint(model4))
