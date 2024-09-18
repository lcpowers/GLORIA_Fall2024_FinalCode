# **Climate and species traits shape responses of alpine flora in the U.S. Rocky Mountains to a changing world**

Final Code

Authors: Claire Powers, Erin Borgmann, Dan Doak

> This repository contains code that (1) prepares data for analyses, (2) performs a model selection process of two data sets of interest (species % cover and total plant cover), and (3) has a simple script for looking a basic model outputs. In order to manage the size of this repository, the only uploaded data sets are those needed for the model selection and results scripts. All other data sets are publicly available (see associated report for sources.)

## **Scripts**

### 1_DataPrep

#### VegetationData

##### *1x1VegData.R*

Takes raw species-level data from 1x1m quadrat surveys from 5 parks, combines it into one dataset, aggregates to the park-summit-aspect level, and fills in meaningful 0-value data.

##### *speciesRanges_Final.R*

Takes herbarium records with spatial data downloaded from iDigBio, selects relevant attributes, and finds various latitudinal summaries at that species level. Requires an intermediate step in where species’ points are cropped to an outline of North America. We performed this in QGIS rather an R for ease of visualization of points before and after being cropped.

##### *surfaceCoverfromSpecCover_final.R*

Generates data for total plant cover from 1x1-m species-level quadrat survey data.

#### **TemperatureData**

##### *1_CompileRawTemp_FindGaps.R*

##### Compiles all raw temperature data and finds missing data/data gaps.

##### *2_WaterBalanceModel_Thoma.R*

Uses David Thoma’s water balance model to generate water stress estimates at the summit level.

##### *3_AggregateDailyClimate.R*

Compiles daily climate data from PRISM, raw temperature data, and water balance model output.

##### *4_FillMissingTempData*

Summit-aspect level linear models to fill in missing climate data, using PRISM and other summit-aspect data as possible predictors.

##### *AnalysisData SpeciesLevel_AnalysisDataPrep.Rmd*

Compiles climate and 1x1-m species-level vegetation data into a table format ready for model input.

##### *PlantCover_AnalysisDataPrep.Rmd*

Compiles climate and total plant cover vegetation data into a table format ready for model input.

### **2_ModelSelection**

##### *ModelSelection_1x1m_SpeciesLevel.Rmd*

Finds best supported mixed effects model predicting changes in species cover using climate PCA axes 1 & 2 and then explicit climate variables.

##### *ModelSelection_SpeciesofInterest.Rmd*

Finds best supported mixed effects model predicting changes in species cover for individual species of special interest (with adequate data) using explicit climate variables.

##### *ModelSelection_VascPlantCover.Rmd*

Finds best supported mixed effects model predicting changes in total plant cover at the summit-aspect level for using climate PCA axes 1 & 2 and then explicit climate variables.

### **3_Results**

##### *Final Models.Rmd*

Contains data and final models to easy plot results.

##### *Report figures.Rmd*

Code to generate figures in the final report.

# Data/AnalysisData

##### specieslevel_analysisdata_final.csv

Input data for model selection and results scripts for species models (species-level and SOI)

##### vascplantcover_final.csv

Input data for model selection and results for total plant cover models.
