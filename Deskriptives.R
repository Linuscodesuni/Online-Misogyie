#Politische Ebene Testing und Deskriptive Statistik

table(df_test$Parteizugehörigkeit)
table(df$Parteizugehörigkeit)
table(df$Parteizugehörigkeit, df$PolitischeEbene)
table(df_test$Parteizugehörigkeit, df_test$PolitischeEbene_grouped)

table(df$Parteizugehörigkeit, df$Age)