library(readxl)
library(tidyverse)

#Les data
byberg_dat <- readRDS("byberg et al.rds")

#Funksjon for chi^2- og Fischer-tester.
#Funksjonen er delvis lagd med ChatGPT og er kontrollert av forskerne
association_test <- function(data_set) {
  # Test if expected cells < 5
  if (suppressWarnings(any(chisq.test(data_set)$expected < 5))) {
    # Perform a Fisher's exact test
    test_result <- fisher.test(data_set, simulate.p.value = TRUE, B = 1000000)
  } else {
    # Perform a chi-squared test
    test_result <- chisq.test(data_set)
  }
  # Get the name of the data set
  data_set_name <- deparse(substitute(data_set))
  # Ask user for indication
  indication <- readline("Enter '1' for 'utdanning' or '2' for 'years': ")
  # Set prediktor name based on indication
  if (indication == "1") {
    prediktor_name <- "utdanning"
  } else if (indication == "2") {
    prediktor_name <- "years"
  } else {
    stop("Invalid indication provided. Please enter '1' or '2'.")
  }
  # Perform tidy and mutate operations
  test_refined <- broom::tidy(test_result) %>%
    mutate(leartMestFra = data_set_name, prediktor = prediktor_name)
  # Create the variable name
  variable_name <- paste0(data_set_name, "_association_test")
  # Assign the result to the variable
  assign(variable_name, test_refined, envir = .GlobalEnv)
  # Return the result
  return(test_refined)
}

#Chi^2 tester av UTDANNING krysset med LÆRT MEST FRA
kollega <- table(byberg_dat$utdanning, byberg_dat$Kolleger)
# A will be rows, B will be columns
colnames(kollega) <- c("har ikke lært med fra kollega",
"har lært mest fra kollega")
association_test(kollega)

erfaring <- table(byberg_dat$utdanning, byberg_dat$`Erfaring/praksis`)
colnames(erfaring) <- c("har ikke lært med fra erfaring",
"har lært mest fra erfaring")
association_test(erfaring)

reflektere <- table(byberg_dat$utdanning, byberg_dat$`Reflektere over egen praksis`)
colnames(reflektere) <- c("har ikke lært med fra reflektere",
"har lært mest fra reflektere")
association_test(reflektere)

l_utdanning <- table(byberg_dat$utdanning, byberg_dat$Utdanning)
colnames(l_utdanning) <- c("har ikke lært med fra lUtdanning",
"har lært mest fra lUtdanning")
association_test(l_utdanning)

videre_utd <- table(byberg_dat$utdanning, byberg_dat$Videreutd.)
colnames(videre_utd) <- c("har ikke lært med fra videreUtd",
"har lært mest fra videreUtd")
association_test(videre_utd)

kurs_skrive_senter <- table(byberg_dat$utdanning,
byberg_dat$`Kurs u/uni.poeng (FUS, Norm, På sporet), metodikk på skolen TIEY`)
colnames(kurs_skrive_senter) <- c("har ikke lært med fra kursSkriveSenter",
"har lært mest fra kursSkriveSenter")
association_test(kurs_skrive_senter)

faglitteratur <- table(byberg_dat$utdanning, byberg_dat$Faglitteratur)
colnames(faglitteratur) <- c("har ikke lært med fra faglitteratur",
"har lært mest fra faglitteratur")
association_test(faglitteratur)

nettsider <- table(byberg_dat$utdanning,
byberg_dat$`Nettsider/nettressurser/facebook`)
colnames(nettsider) <- c("har ikke lært med fra nettsider",
"har lært mest fra nettsider")
association_test(nettsider)

skolebesoek <- table(byberg_dat$utdanning,
byberg_dat$`Skolebesøk (Charlottenlundmetodikken)`)
colnames(skolebesoek) <- c("har ikke lært med fra skolebesoek",
"har lært mest fra skolebesoek")
association_test(skolebesoek)


praksisstudenter <- table(byberg_dat$utdanning, byberg_dat$`Å ha praksisstudenter`)
colnames(praksisstudenter) <- c("har ikke lært med fra praksisstudenter",
"har lært mest fra praksisstudenter")
association_test(praksisstudenter)

egna_barn <- table(byberg_dat$utdanning, byberg_dat$`Egne barn`)
colnames(egna_barn) <- c("har ikke lært med fra egnaBarn",
"har lært mest fra egnaBarn")
association_test(egna_barn)

#Chi^2 tester av ERFARING krysset med LÆRT MEST FRA
kollega_ansi <- table(byberg_dat$erfarenhet,
byberg_dat$Kolleger) # A will be rows, B will be columns
association_test(kollega_ansi)

erfaring_ansi <- table(byberg_dat$erfarenhet,
byberg_dat$`Erfaring/praksis`)
association_test(erfaring_ansi)

reflektere_ansi <- table(byberg_dat$erfarenhet,
byberg_dat$`Reflektere over egen praksis`)
association_test(reflektere_ansi)

l_utdanning_ansi <- table(byberg_dat$erfarenhet,
byberg_dat$Utdanning)
association_test(l_utdanning_ansi)

videre_utd_ansi <- table(byberg_dat$erfarenhet, byberg_dat$Videreutd.)
association_test(videre_utd_ansi)

kurs_skrive_senter_ansi <- table(byberg_dat$erfarenhet,
byberg_dat$`Kurs u/uni.poeng (FUS, Norm, På sporet), metodikk på skolen TIEY`)
association_test(kurs_skrive_senter_ansi)

faglitteratur_ansi <- table(byberg_dat$erfarenhet,
byberg_dat$Faglitteratur)
association_test(faglitteratur_ansi)

nettsider_ansi <- table(byberg_dat$erfarenhet,
byberg_dat$`Nettsider/nettressurser/facebook`)
association_test(nettsider_ansi)

skolebesoek_ansi <- table(byberg_dat$erfarenhet,
byberg_dat$`Skolebesøk (Charlottenlundmetodikken)`)
association_test(skolebesoek_ansi)

praksisstudenter_ansi <- table(byberg_dat$erfarenhet,
byberg_dat$`Å ha praksisstudenter`)
association_test(praksisstudenter_ansi)

egna_barn_ansi <- table(byberg_dat$erfarenhet, byberg_dat$`Egne barn`)
association_test(egna_barn_ansi)

colnames(kollega_ansi) <- c("har ikke lært med fra kollega",
"har lært mest fra kollega")
colnames(erfaring_ansi) <- c("har ikke lært med fra erfaring",
"har lært mest fra erfaring")
colnames(reflektere_ansi) <- c("har ikke lært med fra reflektere",
"har lært mest fra reflektere")
colnames(l_utdanning_ansi) <- c("har ikke lært med fra lUtdanning",
"har lært mest fra lUtdanning")
colnames(videre_utd_ansi) <- c("har ikke lært med fra videreUtd",
"har lært mest fra videreUtd")
colnames(kurs_skrive_senter_ansi) <- c("har ikke lært med fra kursSkriveSenter",
"har lært mest fra kursSkriveSenter")
colnames(faglitteratur_ansi) <- c("har ikke lært med fra faglitteratur",
"har lært mest fra faglitteratur")
colnames(nettsider_ansi) <- c("har ikke lært med fra nettsider",
"har lært mest fra nettsider")
colnames(skolebesoek_ansi) <- c("har ikke lært med fra skolebesoek<",
"har lært mest fra skolebesoek")
colnames(praksisstudenter_ansi) <- c("har ikke lært med fra praksisstudenter",
"har lært mest fra praksisstudenter")
colnames(egna_barn_ansi) <- c("har ikke lært med fra egnaBarn",
"har lært mest fra egnaBarn")

#Utdanning
all_Utdanning <- bind_rows(
  egna_barn_association_test,
  erfaring_association_test,
  faglitteratur_association_test,
  kollega_association_test,
  kurs_skrive_senter_association_test,
  l_utdanning_association_test,
  nettsider_association_test,
  praksisstudenter_association_test,
  reflektere_association_test,
  skolebesoek_association_test,
  videre_utd_association_test)

#Ansienntitet
all_ANSI <- bind_rows(
  egna_barn_ansi_association_test,
  erfaring_ansi_association_test,
  faglitteratur_ansi_association_test,
  kollega_ansi_association_test,
  kurs_skrive_senter_ansi_association_test,
  l_utdanning_ansi_association_test,
  nettsider_ansi_association_test,
  praksisstudenter_ansi_association_test,
  reflektere_ansi_association_test,
  skolebesoek_ansi_association_test,
  videre_utd_ansi_association_test)