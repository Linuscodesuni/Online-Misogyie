#TimesMisogyny Ordnung

ordinal_definition = data.frame(
  Stufe = c("Nie", "Seltener", "Mehrmals pro Jahr", "Mehrmals pro Monat", "Mehrmals pro Woche"),
  Beschreibung = c("1","2","3","4", "5"))

ftTM = flextable(ordinal_definition)
ftTM = autofit(ftTM)
ftTM
doc = read_docx()
doc = body_add_par(doc, "Regressionsergebnisse (Odds Ratios)", style = "heading 1")
doc = body_add_flextable(doc, value = ftTM)
print(doc, target = "TimesMisogyny.docx")

coefs = summary(model2)$coefficients
results = data.frame(
  Variable = rownames(coefs),
  Estimate = coefs[, "Estimate"],
  Odds_Ratio = exp(coefs[, "Estimate"]),
  Std_Error = coefs[, "Std. Error"],
  p_value = coefs[, "Pr(>|z|)"]
)
results = results[!grepl("\\|", results$Variable), ]
results$Estimate = round(results$Estimate, 3)
results$Odds_Ratio = round(results$Odds_Ratio, 3)
results$Std_Error = round(results$Std_Error, 3)
results$p_value = signif(results$p_value, 3)

# flextable erstellen
ft = flextable(results)
ft = autofit(ft)  # passt Spaltenbreiten automatisch an
ft = width(ft, j = 1:5, width = 1.5)  # alle Spalten auf 1,5 cm Breite setzen (kann angepasst werden)

# Word-Dokument erstellen und Tabelle hinzufügen
doc = read_docx()
doc = body_add_par(doc, "Regressionsergebnisse (Odds Ratios)", style = "heading 1")
doc = body_add_flextable(doc, value = ft)
print(doc, target = "regression_results_2.docx")

