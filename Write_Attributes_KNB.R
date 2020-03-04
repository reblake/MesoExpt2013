library(writexl); library(EML); library(emld); library(dataone); library(datapack); library(arcticdatautils)

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

AllDat_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Analysis/Attributes_Data_Publication/Attributes_ALL_DATA_SEM.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = AllDat_attr)

OilConc_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Analysis/Attributes_Data_Publication/Attributes_OilConc.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = OilConc_attr)

Sulfide_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Analysis/Attributes_Data_Publication/Attributes_Sulfide.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = Sulfide_attr)

Isotopes_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Analysis/Attributes_Data_Publication/Attributes_Isotopes.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = Isotopes_attr)

Temp_attr <- read.csv("D:/Documents/LSU/MesoExp_2013/Analysis/Attributes_Data_Publication/Attributes_Temp_Data_MASTER.csv", stringsAsFactors = FALSE)
shiny_attributes(attributes = Temp_attr)

##################################
# From https://nceas.github.io/datateam-training/training/editing-eml.html
# Public: http://training.arcticdata.io/materials/arctic-data-center-training/programming-metadata-and-data-publishing.html

cn <- CNode('PROD')
knb <- getMNode(cn, "urn:node:KNB")

urn_uuid <- "urn:uuid:63454430-40b2-4334-88d0-3859e5924443"

pkg <- get_package(knb, urn_uuid, file_names = TRUE) # lists out basic metdata for objects in package
dat_files <- read_eml(getObject(knb, pkg$metadata))  # lists out the metadata for data files/figures in package

# updates EXISTING EML document using EML package
# set attributes
AllDat_attr_list <- set_attributes(AllDat_attr)
OilConc_attr_list <- set_attributes(OilConc_attr)
Sulfide_attr_list <- set_attributes(Sulfide_attr)
Isotopes_attr_list <- set_attributes(Isotopes_attr)
Temp_attr_list <- set_attributes(Temp_attr)

# set custom units
custom_units <- read.csv("D:/Documents/LSU/MesoExp_2013/Analysis/Attributes_Data_Publication/Custom_Units_Sulfide.csv", stringsAsFactors = FALSE)
custom_unitList <- set_unitList(custom_units, as_metadata = TRUE)

old_ulist <- dat_files$additionalMetadata$metadata$unitList
dat_files$additionalMetadata$metadata$unitList <- c(old_ulist, custom_unitList)
test <- c(old_ulist, custom_unitList)

dat_files$dataset$otherEntity[[1]]$attributeList <- Temp_attr_list
dat_files$dataset$otherEntity[[2]]$attributeList <- Isotopes_attr_list
dat_files$dataset$otherEntity[[3]]$attributeList <- Sulfide_attr_list
dat_files$dataset$otherEntity[[4]]$attributeList <- OilConc_attr_list
dat_files$dataset$otherEntity[[5]]$attributeList <- AllDat_attr_list


eml_validate(dat_files) 

write_eml(dat_files, "Spartina_data_eml.xml")

# publish!
pkg <- publish_update(knb, pkg$metadata, pkg$resource_map, pkg$data, metadata_path = "Spartina_data_eml.xml")

###################################################
# makes NEW  EML document using EML package
# should be functionalized but for now not
AllDat_attr_list <- set_attributes(AllDat_attr)
AllDat_attr_phys <- set_physical("ALL_DATA_SEM_MesoExpt2013.csv")

AllDat_dataTable <- list(entityName = "ALL_DATA_SEM_MesoExpt2013.csv", attributeList = AllDat_attr_list,
                         physical = AllDat_attr_phys)
#
OilConc_attr_list <- set_attributes(OilConc_attr)
OilConc_attr_phys <- set_physical("OilConc_FINAL_MesoExpt_2013.csv")

OilConc_dataTable <- list(entityName = "OilConc_FINAL_MesoExpt_2013.csv", attributeList = OilConc_attr_list,
                          physical = OilConc_attr_phys)
#
Sulfide_attr_list <- set_attributes(Sulfide_attr)
Sulfide_attr_phys <- set_physical("Sulfide_MesoExpt2013.csv")

Sulfide_dataTable <- list(entityName = "Sulfide_MesoExpt2013.csv", attributeList = Sulfide_attr_list,
                          physical = Sulfide_attr_phys)
# 
Isotopes_attr_list <- set_attributes(Isotopes_attr)
Isotopes_attr_phys <- set_physical("Isotope_figures.txt")

Isotope_dataTable <- list(entityName = "Isotope_figures.txt", attributeList = Isotopes_attr_list,
                          physical = Isotopes_attr_phys)
# 
Temp_attr_list <- set_attributes(Temp_attr)
Temp_attr_phys <- set_physical("Temp_Data_MASTER_MesoExpt2013.csv")

Temp_dataTable <- list(entityName = "Temp_Data_MASTER_MesoExpt2013.csv", attributeList = Temp_attr_list,
                          physical = Temp_attr_phys)

# set custom units
custom_units <- read.csv("D:/Documents/LSU/MesoExp_2013/Analysis/Attributes_Data_Publication/Custom_Units_Sulfide.csv", stringsAsFactors = FALSE)

custom_unitList <- set_unitList(custom_units, as_metadata = TRUE)

# add attributes metadata to package
data_pkg <- list(dataTable = c(AllDat_dataTable, OilConc_dataTable, Sulfide_dataTable,
                               Isotope_dataTable, Temp_dataTable))

data_eml <- list(packageId = "urn:uuid:9fb92ec3-fd9c-46ee-91d6-345886cf287b", system = "knb",
                 dataset = list(dataTable = data_pkg), 
                 additionalMetadata = list(metadata = list(unitList = custom_unitList)))

write_eml(data_eml, "Spartina_data_eml.xml")


eml_validate("Spartina_data_eml.xml") # make sure this returns TRUE. If it says anything other than TRUE you will not be able 
# to update. the document has to be valid























