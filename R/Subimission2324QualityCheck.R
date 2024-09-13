## Quality Check for Submission form 2025-2026 ####
## Author: Francis Fayolle, R4V IM Team ###
## Maintained by Jorge Cabrera, R4V IM Team ###

## Get data ##
# Get and rename 5W data ##

## Get indicators ##

activityinfo::activityInfoToken(Sys.getenv("ACTIVITYINFOTOKEN"),
                                prompt = FALSE)



dfindicators <- queryTable("cwk3g8tm07falxuu7j",
                           "CODE" = "cdhugiblctco28h3",
                           "Sector" = "cagw22hlctcp2vu5",
                           "Sector Objective" = "cyyqv3alctcperj6",
                           "Indicator" = "c1oo0eclctcqtjp8",
                           "Rationale" = "cpaq2z8m07feolz3",
                           "Indicator Type" = "cuskmf7lctcszoga",
                           "Definitions" = "cmfe24fm07ff9y24",
                           "Sector SP" = "cviy6p0lctcud9xc",
                           "Objetivo" = "c4ut4hjlctcunimd",
                           "Indicador" = "c9ck1g2lctcuxg4e",
                           "Descripcion" = "chilz3cm07fgc9g5",
                           "Tipo de indicador" = "cw1sv7hlctd1pk5g",
                           "Definiciones" = "cyznpw1m07fgv8l6", truncateStrings = FALSE)|>
  rename(Indicador.SP = Indicador)

  



dfindEN <- dfindicators %>%
  select(CODE, 
         Sector,
         Indicator,
         Indicator.Type)

dfindSP <- dfindicators %>%
  select(CODE, 
         Sector.SP,
         Indicador.SP,
         Tipo.de.indicador)

# get Admin1 data 

dfGIS <- queryTable("cnkrge1m07falxuu7o",
                    "Country" = "c8u26b8kxeqpy0k4",
                    "Admin1" = "c3ns3zikxeqq4h95",
                    "ISOCode" = "cl3sspjkxeqq8yq6")
#   

ErrorSubEN <- function(data) {

## Script in English
req(data)
dfENG <- data
if(nrow(dfENG)>0){
colnames(dfENG) <- c("Year",
                     "Country",
                     "Admin1",
                     "Organisation",
                     "Organisation_verif",
                     "Sector",
                     "Indicator",
                     "IndicatorType",
                     "ActivityName",
                     "ActivityDescrp",
                     "InKindBudget",
                     "CVABudget",
                     "TotalBudget",
                     "InDestination",
                     "InTransit",
                     "HostCom",
                     "Pendular",
                     "Returnees",
                     "Girls",
                     "Boys",
                     "Women",
                     "Men",
                     "TotalPers",
                     "Output",
                     "Verif1",
                     "Verif2",
                     "Verif3"
                     )

dfENG$InKindBudget = gsub("[\\$,]", "", dfENG$InKindBudget)
dfENG$CVABudget = gsub("[\\$,]", "", dfENG$CVABudget)
dfENG$TotalBudget = gsub("[\\$,]", "", dfENG$TotalBudget)
dfENG$InDestination = gsub("[\\,]", "", dfENG$InDestination)
dfENG$InTransit = gsub("[\\,]", "", dfENG$InTransit)
dfENG$HostCom = gsub("[\\,]", "", dfENG$HostCom)
dfENG$Pendular = gsub("[\\,]", "", dfENG$Pendular)
dfENG$Returnees = gsub("[\\,]", "", dfENG$Returnees)
dfENG$Girls = gsub("[\\,]", "", dfENG$Girls)
dfENG$Boys = gsub("[\\,]", "", dfENG$Boys)
dfENG$Women = gsub("[\\,]", "", dfENG$Women )
dfENG$Men = gsub("[\\,]", "", dfENG$Men)
dfENG$TotalPers = gsub("[\\,]", "", dfENG$TotalPers)
dfENG$Output = gsub("[\\,]", "", dfENG$Output)


dfENG <- dfENG%>%
  mutate_at(c("Year",
              "InKindBudget",
              "CVABudget",
              "TotalBudget",
              "InDestination",
              "InTransit",
              "HostCom",
              "Pendular",
              "Returnees",
              "Girls",
              "Boys",
              "Women",
              "Men",
              "TotalPers",
              "Output"), as.numeric)|>
  mutate(IndicatorType = ifelse(IndicatorType == "Capacity building", "Capacity Building", IndicatorType))

## Data joints and wrangling

dfENGControl <- dfENG |>
  left_join(dfindEN, by = c("Sector", "Indicator", "IndicatorType" = "Indicator.Type"))|>
  left_join(dfGIS, by = c("Country", "Admin1"))|>
## data quality check starts here ##
  rowwise() |>
  mutate( YearMissing = ifelse( is.na(Year) | !(Year %in% c(2025,2026)) , "Review", "" ),
          CountryAdmin1 = ifelse(is.na(ISOCode), "Review", ""),
          PartnerMissing = ifelse(is.na(Organisation), "Review", ""),
          SectorIndicatorError = ifelse(is.na(CODE), "Review", ""),
          ActivityMissing = ifelse( is.na(ActivityName) | is.na(ActivityDescrp), "Review", "" ),
          BudgetError = ifelse( (sum(InKindBudget, CVABudget, na.rm = TRUE) == "0" ) | 
                                   (TotalBudget != sum(InKindBudget, CVABudget, na.rm = TRUE)), "Review", ""),
          MPCSectorBudget = ifelse( Sector == "Multipurpose Cash Assistance (MPC)" & 
                                      (CVABudget  == "0" | is.na(CVABudget)),"Review" , ""),
          DirectAssistNoBenef = ifelse(  IndicatorType == "Direct Assistance" & (TotalPers  == "0" | is.na(TotalPers)),"Review" , ""),
          DirectAssistPopType = ifelse(  IndicatorType == "Direct Assistance" & TotalPers != sum(InDestination,
                                                                                                 InTransit, 
                                                                                                 HostCom,
                                                                                                 Pendular,
                                                                                                 Returnees,
                                                                                                 na.rm = TRUE), "Review" , ""),
          DirectAssistAGD = ifelse(  IndicatorType == "Direct Assistance" & TotalPers != sum(Girls,
                                                                                             Boys,
                                                                                             Women,
                                                                                             Men,
                                                                                             na.rm = TRUE), "Review" , ""),
          CBuildNoBenef = ifelse(IndicatorType == "Capacity Building" & (TotalPers  == "0" | is.na(TotalPers)),"Review" , ""),
          NoOutput = ifelse((IndicatorType == "Infrastructure" | 
                               IndicatorType == "Campaign" |
                               IndicatorType == "Mechanism/Advocacy" |
                               IndicatorType == "Other") & (Output  == "0" | is.na(Output)),"Review" , "")
          )|>
  ungroup()|>
  mutate (id = row_number())


# Eliminate empty error columns
dfENGControl1 <- dfENGControl|>
  select( Year,
          Country,
          Admin1,
          Organisation,
          Organisation_verif,
          Sector,
          Indicator,
          IndicatorType,
          ActivityName,
          ActivityDescrp,
          InKindBudget,
          CVABudget,
          TotalBudget,
          InDestination,
          InTransit,
          HostCom,
          Pendular,
          Returnees,
          Girls,
          Boys,
          Women,
          Men,
          TotalPers,
          Output,
          Verif1,
          Verif2,
          Verif3,
          id)

dfENGControl2 <- dfENGControl |>
  select( YearMissing,
          CountryAdmin1,
          PartnerMissing ,
          SectorIndicatorError,
          ActivityMissing ,
          BudgetError ,
          MPCSectorBudget ,
          DirectAssistNoBenef ,
          DirectAssistPopType,
          DirectAssistAGD ,
          CBuildNoBenef,
          NoOutput,
          id) |>
  discard(~all(is.na(.) | . ==""))|>
  mutate( Review = NA)

dfENGControl2$Review[apply(dfENGControl2, 1, function(r) any(r %in% c("Review"))) == TRUE] <- "Please review activity"

dfENGControl0 <- dfENGControl1 |>
  left_join(dfENGControl2 , by = "id") |>
  select (-id)

return(dfENGControl0)

}

} 

## Script in Spanish ##

ErrorSubSP <- function(dataSP) {
req(dataSP)
dfSP <- dataSP
colnames(dfSP) <- c("Year",
                     "Country",
                     "Admin1",
                     "Organisation",
                     "Organisation_verif",
                     "Sector",
                     "Indicator",
                     "IndicatorType",
                     "ActivityName",
                     "ActivityDescrp",
                     "InKindBudget",
                     "CVABudget",
                     "TotalBudget",
                     "InDestination",
                     "InTransit",
                     "HostCom",
                     "Pendular",
                     "Returnees",
                     "Girls",
                     "Boys",
                     "Women",
                     "Men",
                     "TotalPers",
                     "Output",
                     "Verif1",
                     "Verif2",
                     "Verif3"
)

dfSP$InKindBudget = gsub("[\\$,]", "", dfSP$InKindBudget)
dfSP$CVABudget = gsub("[\\$,]", "", dfSP$CVABudget)
dfSP$TotalBudget = gsub("[\\$,]", "", dfSP$TotalBudget)
dfSP$InDestination = gsub("[\\,]", "", dfSP$InDestination)
dfSP$InTransit = gsub("[\\,]", "", dfSP$InTransit)
dfSP$HostCom = gsub("[\\,]", "", dfSP$HostCom)
dfSP$Pendular = gsub("[\\,]", "", dfSP$Pendular)
dfSP$Returnees = gsub("[\\,]", "", dfSP$Returnees)
dfSP$Girls = gsub("[\\,]", "", dfSP$Girls)
dfSP$Boys = gsub("[\\,]", "", dfSP$Boys)
dfSP$Women = gsub("[\\,]", "", dfSP$Women )
dfSP$Men = gsub("[\\,]", "", dfSP$Men)
dfSP$TotalPers = gsub("[\\,]", "", dfSP$TotalPers)
dfSP$Output = gsub("[\\,]", "", dfSP$Output)

dfSP <- dfSP%>%
  mutate_at(c("Year",
              "InKindBudget",
              "CVABudget",
              "TotalBudget",
              "InDestination",
              "InTransit",
              "HostCom",
              "Pendular",
              "Returnees",
              "Girls",
              "Boys",
              "Women",
              "Men",
              "TotalPers",
              "Output"), as.numeric)

## Data joints and wrangling

dfSPControl <- dfSP %>%
  left_join(dfindSP, by = c("Sector" = "Sector.SP", "Indicator" = "Indicador.SP", "IndicatorType" = "Tipo.de.indicador"))%>%
  left_join(dfGIS, by = c("Country", "Admin1"))%>%
  ## data quality check starts here ##
  rowwise() %>%
  mutate( YearMissing = ifelse( is.na(Year) | !(Year %in% c(2025,2026)) , "Review", "" ),
          CountryAdmin1 = ifelse(is.na(ISOCode), "Review", ""),
          PartnerMissing = ifelse(is.na(Organisation), "Review", ""),
          SectorIndicatorError = ifelse(is.na(CODE), "Review", ""),
          ActivityMissing = ifelse( is.na(ActivityName) | is.na(ActivityDescrp), "Review", "" ),
          BudgetError = ifelse( (sum(InKindBudget, CVABudget, na.rm = TRUE) == "0" ) |
                                  (TotalBudget != sum(InKindBudget, CVABudget, na.rm = TRUE)), "Review", ""),
          MPCSectorBudget = ifelse( Sector == "Transferencias Monetarias Multipropósito (MPC)" &
                                      (CVABudget  == "0" | is.na(CVABudget)),"Review" , ""),
          DirectAssistNoBenef = ifelse(  IndicatorType == "Asistencia directa" & (TotalPers  == "0" | is.na(TotalPers)),"Review" , ""),
          DirectAssistPopType = ifelse(  IndicatorType == "Asistencia directa" & TotalPers != sum(InDestination,
                                                                                                 InTransit,
                                                                                                 HostCom,
                                                                                                 Pendular,
                                                                                                 Returnees,
                                                                                                 na.rm = TRUE), "Review" , ""),
          DirectAssistAGD = ifelse(  IndicatorType == "Asistencia directa" & TotalPers != sum(Girls,
                                                                                             Boys,
                                                                                             Women,
                                                                                             Men,
                                                                                             na.rm = TRUE), "Review" , ""),
          CBuildNoBenef = ifelse(IndicatorType == "Capacitaciones" & (TotalPers  == "0" | is.na(TotalPers)),"Review" , ""),
          NoOutput = ifelse((IndicatorType == "Infraestructura" |
                               IndicatorType == "Campaña" |
                               IndicatorType == "Mecanismo/Abogacía" |
                               IndicatorType == "Otro") & (Output  == "0" | is.na(Output)),"Review" , "")
  )%>%
  ungroup()%>%
  mutate (id = row_number())


# Eliminate empty error columns
dfSPControl1 <- dfSPControl %>%
  select( Year,
          Country,
          Admin1,
          Organisation,
          Organisation_verif,
          Sector,
          Indicator,
          IndicatorType,
          ActivityName,
          ActivityDescrp,
          InKindBudget,
          CVABudget,
          TotalBudget,
          InDestination,
          InTransit,
          HostCom,
          Pendular,
          Returnees,
          Girls,
          Boys,
          Women,
          Men,
          TotalPers,
          Output,
          Verif1,
          Verif2,
          Verif3,
          id)

dfSPControl2 <- dfSPControl %>%
  select( YearMissing,
          CountryAdmin1,
          PartnerMissing ,
          SectorIndicatorError,
          ActivityMissing ,
          BudgetError ,
          MPCSectorBudget ,
          DirectAssistNoBenef ,
          DirectAssistPopType,
          DirectAssistAGD ,
          CBuildNoBenef,
          NoOutput,
          id) %>%
  discard(~all(is.na(.) | . ==""))%>%
  mutate( Review = NA)

dfSPControl2$Review[apply(dfSPControl2, 1, function(r) any(r %in% c("Review"))) == TRUE] <- "Please review activity"

dfSPControl0 <- dfSPControl1 %>%
  left_join(dfSPControl2 , by = "id") %>%
  select (-id)
return(dfSPControl0)
}
