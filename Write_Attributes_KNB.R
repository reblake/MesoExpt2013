library(writexl); library(EML); library(dataone); library(datapack); library(arcticdatautils)

###

Photo_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/LICOR_Files_Meso_Expt/LICOR_PhotosynMeas_MesoExpt_2013.csv", header=TRUE)

attributeNames <- names(Photo_attr)

attributes <- data.frame(
    attributeName = names(Photo_attr),
    attributeDefinition = NA_character_,
    measurementScale = NA_character_,
    domain = NA_character_,
    formatString = NA_character_,
    definition = NA_character_,
    unit = NA_character_,
    numberType = NA_character_,
    missingValueCode = NA_character_,
    missingValueCodeExplanation = NA_character_,
    stringsAsFactors = FALSE)

write_xlsx(attributes, "./Attributes_LICOR_PhotosynMeas.xlsx")

###

AllDat_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Attributes_Data_Publication/Attributes_ALL_DATA_SEM.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = AllDat_attr)

OilConc_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Attributes_Data_Publication/Attributes_OilConc.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = OilConc_attr)

Sulfide_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Attributes_Data_Publication/Attributes_Sulfide.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = Sulfide_attr)

Isotopes_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Attributes_Data_Publication/Attributes_Isotopes.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = Isotopes_attr)

Temp_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Attributes_Data_Publication/Attributes_Temp_Data_MASTER.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = Temp_attr)

###
# From https://nceas.github.io/datateam-training/training/editing-eml.html

cn <- CNode('PROD')
knb <- getMNode(cn, "urn:node:KNB")

urn_uuid <- "urn:uuid:9fb92ec3-fd9c-46ee-91d6-345886cf287b"

pkg <- get_package(knb, urn_uuid, file_names = TRUE)



