
library(jsonvalidate)


taskData_ex <- list.files("./JSONschema/June22 revision/Initial taskData") # list all files from the initial set
taskData_ex <- taskData_ex[stringr::str_detect(taskData_ex, ".json")] # reduce to JSON files
taskData_ex <- taskData_ex[!stringr::str_detect(taskData_ex, "chema")] # remove the schemas
taskData_ex <- paste0("./JSONschema/June22 revision/Initial taskData/", taskData_ex) # add full path

combined_validator <- json_validator(schema="./JSONschema/coreMTB_taskData.json",
                                     engine='ajv')

spelling_android <- combined_validator(taskData_ex[1], verbose=T) # pass after modification for SE==0
vocab_android <- combined_validator(taskData_ex[2], verbose=T) # pass after modification for SE==0
dccs_ios <- combined_validator(taskData_ex[3], verbose=T) # pass with allowance for accuracy vs rateScore
flanker_ios <- combined_validator(taskData_ex[4], verbose=T) # pass
mfs_ios <- combined_validator(taskData_ex[5], verbose=T) # pass after modification for rawMFS vs rawScore
numbermatch_ios <- combined_validator(taskData_ex[6], verbose=T) # pass
psm_android <- combined_validator(taskData_ex[7], verbose=T)
psm_ios <- combined_validator(taskData_ex[8], verbose=T)
spelling_ios <- combined_validator(taskData_ex[9], verbose=T) # pass after modification for SE==0
vocab_ios <- combined_validator(taskData_ex[10], verbose=T) # pass after modification for SE==0
dccs_android <- combined_validator(taskData_ex[11], verbose=T) # pass with allowance for accuracy vs rateScore
flanker_android <- combined_validator(taskData_ex[12], verbose=T) # pass
mfs_android <- combined_validator(taskData_ex[13], verbose=T) # pass after modification for rawMFS vs rawScore
numbermatch_android <- combined_validator(taskData_ex[14], verbose=T) # pass



# change these to help troubleshoot validation errors
colnames(attr(psm_android, "errors"))

table(attr(psm_android, "errors")[,"schemaPath"])
table(attr(psm_android, "errors")[,"keyword"])

attr(psm_android, "errors")[which(attr(psm_android, "errors")[,"schemaPath"] == "#/properties/steps/items/oneOf/3/additionalProperties"),]
