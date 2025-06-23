#Daten einlesen
data= read_excel(path='excel_path', sheet=1)

#Dataframe zu codebook
codebook= t(data[1,])

#preprocessing
df=data
df= df[-1, ]

  #Aktuelles Mandat
  names(df)[names(df) == "Aktuelles Mandat"] = "AktuellesMandat"
  df$AktuellesMandat[df$AktuellesMandat == "Ich möchte keine Angaben machen."] = NA
  #Vergangenes Mandat
  names(df)[names(df) == "Vergangenheit Mandat"] = "VergangenheitMandat"
  df$VergangenheitMandat[df$VergangenheitMandat == "Ich möchte keine Angaben machen."] = NA
  
  #Zukunft Mandat
  names(df)[names(df) == "Zukunft Mandat"] = "ZukunftMandat"
  df$ZukunftMandat[df$ZukunftMandat == "Ich möchte keine Angaben machen."] = NA
  
  
  #Beruf
  df$Beruf[df$Beruf == "Ich möchte keine Angaben machen."] = NA
  
  
  #Diskriminierung
  df= df %>%
  mutate(Diskriminierung = coalesce(IntersectionalityCha_1, IntersectionalityCha_4))
  
  #Nutzung
  df= df %>%
    mutate(Nutzung = coalesce(Platforms1_15, Platforms1_23))
  df$Nutzung[is.na(df$Nutzung)] = "Selbstnutzung"

  #Parteizugehörigkeit
  df=df %>% filter(!is.na(Partei) | !is.na(Q222))
  df= df %>%
    mutate(Parteizugehörigkeit = coalesce(Partei, Q222))

  #Misogyniefragen cleanen und mergen
  df=df %>% filter(!is.na(Q180))
  df=df[df$Q180 != "Ich möchte keine Angaben machen.",]
  df$Q180[df$Q180 == "Ja"] = NA
  df <- df %>%
    mutate(TimesMisogyny = coalesce(Q180, TimesMisogyny))
  
  
  
  names(df)[names(df) == "Politische Ebene"] = "PolitischeEbene"

  keepSpalten= c("Parteizugehörigkeit","TimesMisogyny","Geschlecht",  
               "YearsActive","PolitischeEbene", "Beruf","Age","Diskriminierung","Nutzung","AktuellesMandat","VergangenheitMandat","ZukunftMandat")
  df= df[,keepSpalten]
  
  #nur Frauen
  df=df %>% filter(Geschlecht == "Weiblich")
  
  #cleaning Parteizugehörigkeit 
  df=df[df$Parteizugehörigkeit !="Ich möchte keine Angaben machen.",]
  df=df[df$Parteizugehörigkeit != "Sonstige, bitte angeben:",]
  df=df[df$Parteizugehörigkeit != "Ich habe nicht gewählt.",]

  #ohne parteien, mit zu kleiner datengrundlage
  df=df[df$Parteizugehörigkeit !="AfD",]
  df=df[df$Parteizugehörigkeit !="FDP",]
  df=df[df$Parteizugehörigkeit !="Die Linke",]

  #TimesMisogyny Kategorien ordnen 
  df$TimesMisogyny = ordered(df$TimesMisogyny, 
                             levels= c("Nein","Seltener","Mehrmals pro Jahr", "Mehrmals pro Monat", "Mehrmals pro Woche"))
  
  df=df %>% filter(!is.na(TimesMisogyny))
  
  
#df Politische Ebene Kategorie Grouped
  df$PolitischeEbene_grouped <- ifelse(df$PolitischeEbene == "Kommunaler Ebene",
                                   "Kommunaler Ebene",
                                   "Über kommunaler Ebene")

 
  
  
  
  
  
   
  
  
