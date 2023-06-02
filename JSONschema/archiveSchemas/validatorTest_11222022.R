
library(tidyverse)
library(jsonvalidate)

emoFiles <- list.files("C:/Users/ajk128/Northwestern University/Mobile Toolbox - EMO Android 1.4")
emoFiles_ex <- paste0("C:/Users/ajk128/Northwestern University/Mobile Toolbox - EMO Android 1.4/", emoFiles) # add full path

combined_validator <- json_validator(schema="~/MTBfx/JSONschema/archiveSchemas/taskData_combinedSchemaCopy.json",
                                     engine='ajv')


emoFiles_iOS <- list.files("C:/Users/ajk128/Northwestern University/Mobile Toolbox - EMO iOS 1.14.285")
emoFiles_iOS_ex <- paste0("C:/Users/ajk128/Northwestern University/Mobile Toolbox - EMO iOS 1.14.285/", emoFiles_iOS)


# UNLESS OTHERWISE NOTED: missing `schemaIdentifier`
AngerAffect_current <- combined_validator(emoFiles_ex[1], verbose=T)
AngerHostility_current <- combined_validator(emoFiles_ex[2],verbose=T)
AngerAggression_current <- combined_validator(emoFiles_ex[3], verbose=T)
Apathy_current <- combined_validator(emoFiles_ex[4],verbose=T)
CogFx_current <- combined_validator(emoFiles_ex[5], verbose=T) # has `schemaIdentifier` but missing `steps` - also no `scores` (not required)
EmotSupp_current <- combined_validator(emoFiles_ex[6],verbose=T)
FearAffect_current <- combined_validator(emoFiles_ex[7], verbose=T)
FearSomatic_current <- combined_validator(emoFiles_ex[8],verbose=T)
Friendship_current <- combined_validator(emoFiles_ex[9], verbose=T)
GlobalHealth_current <- combined_validator(emoFiles_ex[10],verbose=T) # has `schemaIdentifier` but missing `steps` - also no `scores` (not required)
InstrumentalSupp_current <- combined_validator(emoFiles_ex[11], verbose=T)
Loneliness_current <- combined_validator(emoFiles_ex[12],verbose=T)
MeaningPurpose_current <- combined_validator(emoFiles_ex[13], verbose=T)
PerceivedHostility_current <- combined_validator(emoFiles_ex[14],verbose=T)
PerceivedRej_current <- combined_validator(emoFiles_ex[15], verbose=T)
PerceivedStress_current <- combined_validator(emoFiles_ex[16],verbose=T)
PosAffect_current <- combined_validator(emoFiles_ex[17],verbose=T)
PROMIS29_current <- combined_validator(emoFiles_ex[18], verbose=T) # passes
Sadness_current <- combined_validator(emoFiles_ex[19],verbose=T)
Satisfaction_current <- combined_validator(emoFiles_ex[20], verbose=T)
SelfEff_current <- combined_validator(emoFiles_ex[21],verbose=T)


# iOS versions - unless otherwise noted, missing taskRunUUID
AngerAffect_ios <- combined_validator(emoFiles_iOS_ex[1], verbose=T)
AngerHostility_ios <- combined_validator(emoFiles_iOS_ex[2],verbose=T)
AngerAggression_ios <- combined_validator(emoFiles_iOS_ex[3], verbose=T)
Apathy_ios <- combined_validator(emoFiles_iOS_ex[4],verbose=T)
CogFx_ios <- combined_validator(emoFiles_iOS_ex[5], verbose=T) # wrong test!
EmotSupp_ios <- combined_validator(emoFiles_iOS_ex[6],verbose=T)
FearAffect_ios <- combined_validator(emoFiles_iOS_ex[7], verbose=T)
Friendship_ios <- combined_validator(emoFiles_iOS_ex[8], verbose=T)
GlobalHealth_ios <- combined_validator(emoFiles_iOS_ex[9],verbose=T) # wrong test!
InstrumentalSupp_ios <- combined_validator(emoFiles_iOS_ex[10], verbose=T)
Loneliness_ios <- combined_validator(emoFiles_iOS_ex[11],verbose=T)
MeaningPurpose_ios <- combined_validator(emoFiles_iOS_ex[12], verbose=T)
PerceivedHostility_ios <- combined_validator(emoFiles_iOS_ex[13],verbose=T)
PerceivedRej_ios <- combined_validator(emoFiles_iOS_ex[14], verbose=T)
PerceivedStress_ios <- combined_validator(emoFiles_iOS_ex[15],verbose=T)
PosAffect_ios <- combined_validator(emoFiles_iOS_ex[16],verbose=T)
PROMIS29_ios <- combined_validator(emoFiles_iOS_ex[17], verbose=T) # added property to steps and stepHistory
Sadness_ios <- combined_validator(emoFiles_iOS_ex[18],verbose=T)
Satisfaction_ios <- combined_validator(emoFiles_iOS_ex[19], verbose=T)
SelfEff_ios <- combined_validator(emoFiles_iOS_ex[20],verbose=T)
FearSomatic_ios <- combined_validator(emoFiles_iOS_ex[21],verbose=T)


# test the errors here:
colnames(attr(FearSomatic_ios, "errors"))
table(attr(FearSomatic_ios, "errors")[,"schemaPath"])
table(attr(FearSomatic_ios, "errors")[,"keyword"])
attr(FearSomatic_ios, "errors")[which(attr(FearSomatic_ios, "errors")[,"schemaPath"] == "#/required"),
                                    c("instancePath","schemaPath","keyword","message")]


#### Test some scores ####

library(jsonlite)

SelfEfficacy <- read_json(emoFiles_ex[21], simplifyVector = T)
names(SelfEfficacy)

SelfEfficacy$steps
SelfEfficacy$scores
