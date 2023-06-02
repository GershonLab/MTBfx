# This is an attempt to consolidate the schema and use one resource at a time rather than separating out int-int and str-int.

library(jsonvalidate)

taskData_ex <- list.files("./JSONschema/round2Exports") # list all files
taskData_ex <- taskData_ex[stringr::str_detect(taskData_ex, ".json")] # reduce to JSON files
taskData_ex <- taskData_ex[!stringr::str_detect(taskData_ex, "chema")] # remove the schemas
taskData_ex <- paste0("./JSONschema/round2Exports/", taskData_ex) # add full path

# Integer-Integer Data
combined_validator <- json_validator(schema="./JSONschema/taskData_combinedSchema.json",
                                   engine='ajv')
dccs <- combined_validator(taskData_ex[1], verbose=T) # note - made schema changes throughout to get this to work.
flanker <- combined_validator(taskData_ex[2], verbose=T)
FNAME_Learning <- combined_validator(taskData_ex[3], verbose=T)
FNAME_Test <- combined_validator(taskData_ex[4], verbose=T)
NumberMatch <- combined_validator(taskData_ex[6], verbose=T)
psm <- combined_validator(taskData_ex[7], verbose=T)
vocab1 <- combined_validator(taskData_ex[9], verbose=T) # needed to modify SE in the taskData to get this to validate
vocab2 <- combined_validator(taskData_ex[10], verbose=T)

# String-Integer Data
mfs <- combined_validator(taskData_ex[5], verbose=T)
spelling <- combined_validator(taskData_ex[8], verbose=T) # needed to modify SE in the taskData to get this to validate



# change these to help troubleshoot validation errors
colnames(attr(vocab2, "errors"))

table(attr(vocab2, "errors")[,"schemaPath"])
table(attr(vocab2, "errors")[,"keyword"])
table(attr(vocab2, "errors")[,"params"],useNA='ifany')

