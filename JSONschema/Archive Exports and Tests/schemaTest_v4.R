# This is an attempt to consolidate the schema and use one resource at a time rather than separating out int-int and str-int.

library(jsonvalidate)

taskData_ex <- list.files("./JSONschema/round4Exports") # list all files
taskData_ex <- taskData_ex[stringr::str_detect(taskData_ex, ".json")] # reduce to JSON files
taskData_ex <- taskData_ex[!stringr::str_detect(taskData_ex, "chema")] # remove the schemas
taskData_ex <- paste0("./JSONschema/round4Exports/", taskData_ex) # add full path

# Integer-Integer Data
combined_validator <- json_validator(schema="./JSONschema/taskData_combinedSchema.json",
                                   engine='ajv')

dccs_android <- combined_validator(taskData_ex[2], verbose=T)
dccs_iOS <- combined_validator(taskData_ex[1], verbose=T)
flanker_android <- combined_validator(taskData_ex[3], verbose=T)
flanker_iOS <- combined_validator(taskData_ex[4], verbose=T)
fnameLearning_android <- combined_validator(taskData_ex[6], verbose=T)
fnameLearning_ios <- combined_validator(taskData_ex[5], verbose=T)
fnameTest_android <- combined_validator(taskData_ex[8], verbose=T)
fnameTest_iOS <- combined_validator(taskData_ex[7], verbose=T)
mfs_android <- combined_validator(taskData_ex[10], verbose=T)
mfs_iOS <- combined_validator(taskData_ex[9], verbose=T)
nm_android <- combined_validator(taskData_ex[13], verbose=T)
nm_iOS <- combined_validator(taskData_ex[12], verbose=T)
psm_android <- combined_validator(taskData_ex[14], verbose=T) #fails - missing `didTimeout` within `steps` and `stepHistory`
psm_iOS <- combined_validator(taskData_ex[15], verbose=T)
spelling_android <- combined_validator(taskData_ex[11], verbose=T) #fails - top-level scores
spelling_iOS <- combined_validator(taskData_ex[16], verbose=T)
vocabForm1_android <- combined_validator(taskData_ex[19], verbose=T) #fails - top-level scores AND `identifier`
vocabForm1_ios <- combined_validator(taskData_ex[17], verbose=T)
vocabForm2_android <- combined_validator(taskData_ex[20], verbose=T) #fails - top-level scores AND `identifier`
vocabForm2_ios <- combined_validator(taskData_ex[18], verbose=T)


# change these to help troubleshoot validation errors
colnames(attr(psm_iOS, "errors"))

table(attr(psm_iOS, "errors")[,"schemaPath"])
table(attr(psm_iOS, "errors")[,"keyword"])
table(attr(psm_android, "errors")[,"params"],useNA='ifany')

# confirm scores are comparable in the exports

library(jsonlite)

dccsScoreAndroid <- read_json(taskData_ex[2], simplifyVector = T)
dccsScoreIOS <- read_json(taskData_ex[1], simplifyVector = T)
all(names(dccsScoreAndroid$scores) %in% names(dccsScoreIOS$scores)) &
  all(names(dccsScoreIOS$scores) %in% names(dccsScoreAndroid$scores)) &
  all(!is.null(dccsScoreAndroid$scores)) &
  all(!is.null(dccsScoreIOS))

FICAScoreAndroid <- read_json(taskData_ex[3], simplifyVector = T)
FICAScoreIOS <- read_json(taskData_ex[4], simplifyVector = T)
all(names(FICAScoreAndroid$scores) %in% names(FICAScoreIOS$scores)) &
  all(names(FICAScoreIOS$scores) %in% names(FICAScoreAndroid$scores)) &
  all(!is.null(FICAScoreAndroid$scores)) &
  all(!is.null(FICAScoreIOS))

FNameLScoreAndroid <- read_json(taskData_ex[6], simplifyVector = T)
FNameLScoreIOS <- read_json(taskData_ex[5], simplifyVector = T)
all(names(FNameLScoreAndroid$scores) %in% names(FNameLScoreIOS$scores)) &
  all(names(FNameLScoreIOS$scores) %in% names(FNameLScoreAndroid$scores)) &
  all(!is.null(FNameLScoreAndroid$scores)) &
  all(!is.null(FNameLScoreIOS))
# doesn't exist

FNameTScoreAndroid <- read_json(taskData_ex[8], simplifyVector = T)
FNameTScoreIOS <- read_json(taskData_ex[7], simplifyVector = T)
all(names(FNameTScoreAndroid$scores) %in% names(FNameTScoreIOS$scores)) &
  all(names(FNameTScoreIOS$scores) %in% names(FNameTScoreAndroid$scores)) &
  all(!is.null(FNameTScoreAndroid$scores)) &
  all(!is.null(FNameTScoreIOS))

MFSScoreAndroid <- read_json(taskData_ex[10], simplifyVector = T)
MFSScoreIOS <- read_json(taskData_ex[9], simplifyVector = T)
all(names(MFSScoreAndroid$scores) %in% names(MFSScoreIOS$scores)) &
  all(names(MFSScoreIOS$scores) %in% names(MFSScoreAndroid$scores)) &
  all(!is.null(MFSScoreAndroid$scores)) &
  all(!is.null(MFSScoreIOS))
# doesn't exist

nmScoreAndroid <- read_json(taskData_ex[13], simplifyVector = T)
nmScoreIOS <- read_json(taskData_ex[12], simplifyVector = T)
all(names(nmScoreAndroid$scores) %in% names(nmScoreIOS$scores)) &
  all(names(nmScoreIOS$scores) %in% names(nmScoreAndroid$scores)) &
  all(!is.null(nmScoreAndroid$scores)) &
  all(!is.null(nmScoreIOS))

PSMScoreAndroid <- read_json(taskData_ex[14], simplifyVector = T)
PSMScoreIOS <- read_json(taskData_ex[15], simplifyVector = T)
all(names(PSMScoreAndroid$scores) %in% names(PSMScoreIOS$scores)) &
  all(names(PSMScoreIOS$scores) %in% names(PSMScoreAndroid$scores)) &
  all(!is.null(PSMScoreAndroid$scores)) &
  all(!is.null(PSMScoreIOS))

SpellScoreAndroid <- read_json(taskData_ex[11], simplifyVector = T)
SpellScoreIOS <- read_json(taskData_ex[16], simplifyVector = T)
all(names(SpellScoreAndroid$scores) %in% names(SpellScoreIOS$scores)) &
  all(names(SpellScoreIOS$scores) %in% names(SpellScoreAndroid$scores)) &
  all(!is.null(SpellScoreAndroid$scores)) &
  all(!is.null(SpellScoreIOS))
# exists only on iOS


Vocab1ScoreAndroid <- read_json(taskData_ex[19], simplifyVector = T)
Vocab1ScoreIOS <- read_json(taskData_ex[17], simplifyVector = T)
all(names(Vocab1ScoreAndroid$scores) %in% names(Vocab1ScoreIOS$scores)) &
  all(names(Vocab1ScoreIOS$scores) %in% names(Vocab1ScoreAndroid$scores)) &
  all(!is.null(Vocab1ScoreAndroid$scores)) &
  all(!is.null(Vocab1ScoreIOS))
# exists only on iOS


Vocab2ScoreAndroid <- read_json(taskData_ex[20], simplifyVector = T)
Vocab2ScoreIOS <- read_json(taskData_ex[18], simplifyVector = T)
all(names(Vocab2ScoreAndroid$scores) %in% names(Vocab2ScoreIOS$scores)) &
  all(names(Vocab2ScoreIOS$scores) %in% names(Vocab2ScoreAndroid$scores)) &
  all(!is.null(Vocab2ScoreAndroid$scores)) &
  all(!is.null(Vocab2ScoreIOS))
# exists only on iOS

#### Final Checks for Andoid Dichotomous ####


newFiles <- paste0("P:/Mobile Toolbox/Data Core/Schemas/Oct4dichotomous/",list.files("P:/Mobile Toolbox/Data Core/Schemas/Oct4dichotomous"))


newSpelling <- combined_validator(newFiles[1], verbose=T)
newVocab <- combined_validator(newFiles[2], verbose=T)

spellingNewScores <- read_json(newFiles[1], simplifyVector = T)
vocabNewScores <- read_json(newFiles[2], simplifyVector = T)
all(names(spellingNewScores$scores) %in% names(SpellScoreIOS$scores)) &
  all(names(SpellScoreIOS$scores) %in% names(spellingNewScores$scores)) &
  all(!is.null(spellingNewScores$scores)) &
  all(!is.null(SpellScoreIOS))

all(names(vocabNewScores$scores) %in% names(Vocab2ScoreIOS$scores)) &
  all(names(Vocab2ScoreIOS$scores) %in% names(vocabNewScores$scores)) &
  all(!is.null(vocabNewScores$scores)) &
  all(!is.null(Vocab2ScoreIOS))
