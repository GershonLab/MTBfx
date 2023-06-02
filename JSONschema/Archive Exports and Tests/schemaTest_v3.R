# This is an attempt to consolidate the schema and use one resource at a time rather than separating out int-int and str-int.

library(jsonvalidate)

taskData_ex <- list.files("./JSONschema/round3Exports") # list all files
taskData_ex <- taskData_ex[stringr::str_detect(taskData_ex, ".json")] # reduce to JSON files
taskData_ex <- taskData_ex[!stringr::str_detect(taskData_ex, "chema")] # remove the schemas
taskData_ex <- paste0("./JSONschema/round3Exports/", taskData_ex) # add full path

# Integer-Integer Data
combined_validator <- json_validator(schema="./JSONschema/taskData_combinedSchema.json",
                                   engine='ajv')

dccs_android <- combined_validator(taskData_ex[1], verbose=T)
dccs_iOS <- combined_validator(taskData_ex[10], verbose=T)
flanker_android <- combined_validator(taskData_ex[2], verbose=T)
flanker_iOS <- combined_validator(taskData_ex[11], verbose=T)
fnameLearning_android <- combined_validator(taskData_ex[3], verbose=T)
fnameLearning_ios <- combined_validator(taskData_ex[12], verbose=T)
fnameTest_android <- combined_validator(taskData_ex[4], verbose=T)
fnameTest_iOS <- combined_validator(taskData_ex[13], verbose=T)
mfs_android <- combined_validator(taskData_ex[5], verbose=T)
mfs_iOS <- combined_validator(taskData_ex[14], verbose=T)
nm_android <- combined_validator(taskData_ex[8], verbose=T)
nm_iOS <- combined_validator(taskData_ex[15], verbose=T)
psm_android <- combined_validator(taskData_ex[9], verbose=T) #fails - missing `didTimeout` within `steps` and `stepHistory`
psm_iOS <- combined_validator(taskData_ex[16], verbose=T)
spelling_android <- combined_validator(taskData_ex[6], verbose=T) #fails - top-level scores
spelling_iOS <- combined_validator(taskData_ex[17], verbose=T) #fails - top-level scores
vocabForm1_android <- combined_validator(taskData_ex[20], verbose=T) #fails - top-level scores AND `identifier`
vocabForm1_ios <- combined_validator(taskData_ex[18], verbose=T) #fails - top-level scores
vocabForm2_android <- combined_validator(taskData_ex[21], verbose=T) #fails - top-level scores AND `identifier`
vocabForm2_ios <- combined_validator(taskData_ex[19], verbose=T) #fails - top-level scores

# these are expected to fail and will not be transferred to Bridge so that's ok.
persistantData_spelling_android  <- combined_validator(taskData_ex[7], verbose=T) #fails
persistantData_vocab1_android  <- combined_validator(taskData_ex[22], verbose=T) #fails
persistantData_vocab2_android  <- combined_validator(taskData_ex[23], verbose=T) #fails


# change these to help troubleshoot validation errors
colnames(attr(psm_android, "errors"))

table(attr(psm_android, "errors")[,"schemaPath"])
table(attr(psm_android, "errors")[,"keyword"])
table(attr(vocabForm2_ios, "errors")[,"params"],useNA='ifany')

# confirm scores are comparable in the exports

library(jsonlite)

dccsScoreAndroid <- read_json(taskData_ex[1], simplifyVector = T)
dccsScoreIOS <- read_json(taskData_ex[10], simplifyVector = T)
all(names(dccsScoreAndroid$scores) %in% names(dccsScoreIOS$scores) & names(dccsScoreIOS$scores) %in% names(dccsScoreAndroid$scores))


FICAScoreAndroid <- read_json(taskData_ex[2], simplifyVector = T)
FICAScoreIOS <- read_json(taskData_ex[11], simplifyVector = T)
all(names(FICAScoreAndroid$scores) %in% names(FICAScoreIOS$scores) & names(FICAScoreIOS$scores) %in% names(FICAScoreAndroid$scores))


FNameLScoreAndroid <- read_json(taskData_ex[3], simplifyVector = T)
FNameLScoreIOS <- read_json(taskData_ex[12], simplifyVector = T)
all(names(FNameLScoreAndroid$scores) %in% names(FNameLScoreIOS$scores) & names(FNameLScoreIOS$scores) %in% names(FNameLScoreAndroid$scores))
# doesn't exist

FNameTScoreAndroid <- read_json(taskData_ex[4], simplifyVector = T)
FNameTScoreIOS <- read_json(taskData_ex[13], simplifyVector = T)
all(names(FNameTScoreAndroid$scores) %in% names(FNameTScoreIOS$scores) & names(FNameTScoreIOS$scores) %in% names(FNameTScoreAndroid$scores))


MFSScoreAndroid <- read_json(taskData_ex[5], simplifyVector = T)
MFSScoreIOS <- read_json(taskData_ex[14], simplifyVector = T)
all(names(MFSScoreAndroid$scores) %in% names(MFSScoreIOS$scores) & names(MFSScoreIOS$scores) %in% names(MFSScoreAndroid$scores))
# doesn't exist

nmScoreAndroid <- read_json(taskData_ex[8], simplifyVector = T)
nmScoreIOS <- read_json(taskData_ex[15], simplifyVector = T)
all(names(nmScoreAndroid$scores) %in% names(nmScoreIOS$scores) & names(nmScoreIOS$scores) %in% names(nmScoreAndroid$scores))


PSMScoreAndroid <- read_json(taskData_ex[9], simplifyVector = T)
PSMScoreIOS <- read_json(taskData_ex[16], simplifyVector = T)
all(names(PSMScoreAndroid$scores) %in% names(PSMScoreIOS$scores) & names(PSMScoreIOS$scores) %in% names(PSMScoreAndroid$scores))


SpellScoreAndroid <- read_json(taskData_ex[6], simplifyVector = T)
SpellScoreIOS <- read_json(taskData_ex[17], simplifyVector = T)
all(names(SpellScoreAndroid$scores) %in% names(SpellScoreIOS$scores) & names(SpellScoreIOS$scores) %in% names(SpellScoreAndroid$scores))
# doesn't exist


Vocab1ScoreAndroid <- read_json(taskData_ex[20], simplifyVector = T)
Vocab1ScoreIOS <- read_json(taskData_ex[18], simplifyVector = T)
all(names(Vocab1ScoreAndroid$scores) %in% names(Vocab1ScoreIOS$scores) & names(Vocab1ScoreIOS$scores) %in% names(Vocab1ScoreAndroid$scores))
# doesn't exist


Vocab2ScoreAndroid <- read_json(taskData_ex[21], simplifyVector = T)
Vocab2ScoreIOS <- read_json(taskData_ex[19], simplifyVector = T)
all(names(Vocab2ScoreAndroid$scores) %in% names(Vocab2ScoreIOS$scores) & names(Vocab2ScoreIOS$scores) %in% names(Vocab2ScoreAndroid$scores))
# doesn't exist
