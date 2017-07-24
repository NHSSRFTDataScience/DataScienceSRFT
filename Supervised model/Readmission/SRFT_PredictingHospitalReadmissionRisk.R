library(MicrosoftML)
library(RODBC)


colClassesGIB = c(ReAdmitted30days = "numeric",BACKPAIN = "numeric", obesity   = "numeric",
                  weightloss  = "numeric", renalfailure = "numeric",
                  rheumaticarthr  = "numeric", coagulopathy  = "numeric",
                  bloodlossanaemia  = "numeric", defficiencyanaemia = "numeric",
                  drugabuse = "numeric", psychosis  = "numeric",
                  depression = "numeric", IndexMultipleDeprivation  = "numeric",
                  DiagnosisNationalCode1 = "numeric", InvestigationNationalCode1 = "numeric",
                  AgeAtAdmission  = "numeric",  FALLS  = "numeric",
                  AbdominalPainAdults  = "numeric", ShortnessBreathAdults = "numeric"
                  )

sqlConnString <- "Driver=SQL Server;server=srhtsql2db;database=Inpatients;Trusted_Connection=True"
dataSetSource <- RxSqlServerData(connectionString = sqlConnString, table = "era.ReadmitSelectedDatasetNewDatset2",colClasses = colClassesGIB, rowsPerRead = 2000000)


colX2 <- list("Dim" = list(type="numeric"),
"NoPreAdmissionLast12mths" = list(type="numeric"),
"NoPreEmergencyAdmissionsLast12Mt" = list(type="numeric"),
"ReAdmitted" = list(type="numeric"),
"congestiveheartfail" = list(type="numeric"),
"cardiacarrythmias" = list(type="numeric"),
"FCECount" = list(type="numeric"),
"valvulardisease" = list(type="numeric"),
"pulmonarycirculardis" = list(type="numeric"),
"peripheralvasculardis" = list(type="numeric"),
"hypertensionuncom" = list(type="numeric"),
"hypertensioncomplicated" = list(type="numeric"),
"paralysis" = list(type="numeric"),
"otherneurodis" = list(type="numeric"),
"chronicpulmonarydis" = list(type="numeric"),
"diabetesuncomp" = list(type="numeric"),
"diabetescomp" = list(type="numeric"),
"hypothyroidism" = list(type="numeric"),
" renalfailure" = list(type="numeric"),
"liverdisease" = list(type="numeric"),
"pepticulcer" = list(type="numeric"),
"HIV" = list(type="numeric"),
"lymphoma" = list(type="numeric"),
"metacancer" = list(type="numeric"),
"solidtumour" = list(type="numeric"),
"rheumaticarthr " = list(type="numeric"),
"coagulopathy " = list(type="numeric"),
"obesity " = list(type="numeric"),
"weightloss " = list(type="numeric"),
"fluidandelecimba" = list(type="numeric"),
"bloodlossanaemia " = list(type="numeric"),
"defficiencyanaemia " = list(type="numeric"),
"alcohol" = list(type="numeric"),
"drugabuse " = list(type="numeric"),
"psychosis " = list(type="numeric"),
"depression " = list(type="numeric"),
"IndexMultipleDeprivation " = list(type="numeric"),
"DiagnosisNationalCode1 " = list(type="numeric"),
"InvestigationNationalCode1 " = list(type="numeric"),
"AgeAtAdmission " = list(type="numeric"),
"FALLS " = list(type="numeric"),
"AbdominalPainAdults " = list(type="numeric"),
"ShortnessBreathAdults " = list(type="numeric"),
"BACKPAIN " = list(type="numeric",newName = "BP"),
"LIMBPROBLEMS" = list(type="numeric"),
"SELFHARM" = list(type="numeric"),
"STROKEPATHWAY" = list(type="numeric"),
"UNWELLADULT" = list(type="numeric"),
"URINARYPROBLEMS" = list(type="numeric"),
"VOMITING" = list(type="numeric"),
"Chestpain" = list(type="numeric"),
"RadmitTime" = list(type="numeric"),
"numberOfProcedure" = list(type="numeric"),
"numberOfDiagnosis" = list(type="numeric"),
"NoOfInvestigationNatCode" = list(type="numeric"),
"AdmissionDayOfMonthKey" = list(type="numeric"),
"AdmissionHourOfDay" = list(type="numeric"),
"DischargeDayOfMonthKey" = list(type="numeric"),
"DischargeHourOfDay" = list(type="numeric"))

dataset <- rxImport(dataSetSource, stringsAsFactors = FALSE,outFile="DATA527",colInfo=colX2)


CharSubVars <- c("ReAdmitted30days","ArrivalModeCode","Complaint1","InvestigationNo1","PostCode",
                 "Sex","TriagePriorityCode","PatientNumber","NHSNumber","InitialAdmissionType",
                  "AdmissionWard","AdmissionSpecialty","PrimarySpecialty","PrimaryDiagnosis",
                   "PrimaryProcedure","PresentingComplaint","TriageComments","AdmissionMonth",
                   "AdmissionDayOfWeekKey2","AdmissionQuarter",
                   " DischargeMonth","DischargeDayOfWeekKey2","DischargeQuarter")

CharSubVars2 <- c("ReAdmitted30days","ArrivalModeCode","Complaint1","InvestigationNo1","PostCode",
                 "Sex","TriagePriorityCode","PatientNumber","NHSNumber","InitialAdmissionType",
                  "AdmissionWard","AdmissionSpecialty","PrimarySpecialty","PrimaryDiagnosis",
                   "PrimaryProcedure","PresentingComplaint","TriageComments","AdmissionMonth",
                   "AdmissionDayOfWeekKey2","AdmissionQuarter","NoOfInvestigationNatCode","ReAdmitted",
                   " DischargeMonth","DischargeDayOfWeekKey2","DischargeQuarter","SpellSerial", "NonElecSpellTariff")




CharVars <- rxDataStep(inData="DATA527",varsToKeep= CharSubVars)

NumVars <- rxDataStep(inData="DATA527",varsToDrop= CharSubVars2,maxRowsByCols = 30000000)


# Sample operation
pca = prcomp(NumVars)
top_pca_scores = data.frame(pca$x[,1:10])
dataset = cbind(CharVars,top_pca_scores)


# Set the random seed for reproducibility of randomness.
set.seed(2345, "L'Ecuyer-CMRG")
# Randomly split the data 75-25 between train and test sets.
dataProb <- c(Train = 0.50, Test = 0.50)
dataSplit <-
    rxSplit(dataset,
            splitByFactor = "splitVar",
            transforms = list(splitVar =
                                sample(dataFactor,
                                       size = .rxNumRows,
                                       replace = TRUE,
                                       prob = dataProb)),
            transformObjects =
                list(dataProb = dataProb,
                     dataFactor = factor(names(dataProb),
                                         levels = names(dataProb))),
            outFilesBase = tempfile())

# Name the train and test datasets.
dataTrain <- dataSplit[[1]]
dataTest <- dataSplit[[2]]



# create transforms
transformRule = list(
  featurizeText(
    vars = c(Features1 = "TriageComments"),
    wordFeatureExtractor = ngramCount(
      weighting = "tfidf",
      ngramLength = 2,
      skipLength = 1),
    language = "English"
  ),
  categoricalHash(vars = c(Features2 = "ArrivalModeCode","Complaint1","InvestigationNo1",
                        "PostCode","Sex","TriagePriorityCode","InitialAdmissionType",
                        "AdmissionWard","AdmissionSpecialty","PrimarySpecialty","PrimaryDiagnosis","PrimaryProcedure",
                        "PresentingComplaint")),

  selectFeatures(ReAdmitted30days ~ Features1+Features2, mode = mutualInformation(numFeaturesToKeep = 50))
 
 
)

# train using transforms !
model <- rxLogisticRegression(
  ReAdmitted30days ~ Features1+Features2+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
  data = dataTrain,
  mlTransforms = transformRule,
  type = "binary" # not binary (numeric regression)
)


summary(model,top = 50)


 # Use the model to score
 scoreOutDF5 <- rxPredict(model, data = dataTest, 
     extraVarsToWrite = "ReAdmitted30days")

 head(scoreOutDF5)
