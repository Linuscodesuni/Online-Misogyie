#ordinal stufenweise bivariat

#clm model, Prädiktor: Parteizugehörigkeit
df$Parteizugehörigkeit= factor(df$Parteizugehörigkeit)
df$Parteizugehörigkeit= relevel(df$Parteizugehörigkeit, ref = "CDU/CSU")
ordinal= clm(TimesMisogyny ~  Parteizugehörigkeit, data = df)
summary(ordinal)
    
    #Alter 
    df$Age= factor(df$Age)
    df$Age= relevel(df$Age, ref= "35-44")
    ordinalAge= clm(TimesMisogyny ~  Parteizugehörigkeit + Age, data = df)
    summary(ordinalAge)
    
    anova(ordinal,ordinalAge)
    
    #YearsActive
    df$YearsActive= factor(df$YearsActive)
    df$YearsActive= relevel(df$YearsActive, ref= "1- unter 3 Jahre")
    ordinalYearsActive=clm(TimesMisogyny ~  Parteizugehörigkeit + YearsActive, data = df)
    summary(ordinalYearsActive)
        
        #Anova, selbe Datengrundlage
        df_test_years= na.omit(df[, c("TimesMisogyny", "Parteizugehörigkeit", "YearsActive")])
        yearsmod1= clm(TimesMisogyny ~ Parteizugehörigkeit, data = df_test_years)
        yearsmod2= clm(TimesMisogyny ~ Parteizugehörigkeit + YearsActive, data = df_test_years)
        anova(yearsmod1,yearsmod2)
    
    
    #Guter Prädiktor wenig Verlust
    ordinalDiskriminierung=clm(TimesMisogyny ~  Parteizugehörigkeit + Diskriminierung, data = df)
    summary(ordinalDiskriminierung)
    
    #Anova test ergibt signifikanz
    ordinalPolitischeEbene= clm(TimesMisogyny ~  Parteizugehörigkeit + PolitischeEbene, data = df)
    summary(ordinalPolitischeEbene)
    
    ordinalEbenegrouped= clm(TimesMisogyny ~  Parteizugehörigkeit + PolitischeEbene_grouped, data=df)
    summary(ordinalEbenegrouped)
    
    anova(ordinalPolitischeEbene,ordinalEbenegrouped)
    
    df$PolitischeEbene= factor(df$PolitischeEbene)
    df$PolitischeEbene= relevel(df$PolitischeEbene, ref= "Bundesebene")
    
    df_test= na.omit(df[, c("TimesMisogyny", "Parteizugehörigkeit", "PolitischeEbene","PolitischeEbene_grouped")])
    
      #Anova test mit selber datenbasis
      mod1= clm(TimesMisogyny ~ Parteizugehörigkeit, data = df_test)
      anova(mod1, ordinalPolitischeEbene)
    
    
    #nicht signifikant
    ordinalNutzung= clm(TimesMisogyny ~  Parteizugehörigkeit + Nutzung, data = df)
    summary(ordinalNutzung)
  
    #Beruf kein Einfluss, da wenn dann nur sonstiges signifkant und da lässt sich ja wirklich gar nichts sagen
    ordinalBeruf=clm(TimesMisogyny ~  Parteizugehörigkeit + Beruf, data = df)
    summary(ordinalBeruf)
    
    #Mandat kein Einfluss!
    ordinalVergangenheitMandat=clm(TimesMisogyny ~  Parteizugehörigkeit + VergangenheitMandat, data = df)
    summary(ordinalVergangenheitMandat)
      
    ordinalAktuellesMandat=clm(TimesMisogyny ~  Parteizugehörigkeit + AktuellesMandat, data = df)
    summary(ordinalAktuellesMandat)
      
    ordinalZukunftMandat=clm(TimesMisogyny ~  Parteizugehörigkeit + ZukunftMandat, data = df)
    summary(ordinalZukunftMandat)
    
#partial proportional odds Model Test
    
    modeln=clm(TimesMisogyny ~  Parteizugehörigkeit + Age + Diskriminierung ,
               nominal = ~  Parteizugehörigkeit,
               data=df )
    summary(modeln)
    anova(model,modeln)
    
    #mit oder ohne Politische Ebene Anova
    
    dffinaltest= na.omit(df[, c("TimesMisogyny", "Parteizugehörigkeit", "PolitischeEbene","Diskriminierung","Age")])

    finaltest1=clm(TimesMisogyny ~  Parteizugehörigkeit + Age + Diskriminierung + PolitischeEbene , data=dffinaltest )
    finaltest2=clm(TimesMisogyny ~  Parteizugehörigkeit + Age + Diskriminierung, data=dffinaltest )
    anova(finaltest1,finaltest2)
    summary(finaltest2)
    summary(finaltest1)
    
    #Politische ebene grouped Test
    
    Ebenetest1= clm(TimesMisogyny ~  Parteizugehörigkeit + Age + Diskriminierung + PolitischeEbene_grouped , data=df )
    Ebenetest2= clm(TimesMisogyny ~  Parteizugehörigkeit + Age + Diskriminierung + PolitischeEbene , data=df)
    summary(Ebenetest1)
    summary(Ebenetest2)
    anova(Ebenetest1,Ebenetest2)
    

    
    
    
    
    
    
    
    
    
    
    
  
