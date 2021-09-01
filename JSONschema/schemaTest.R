library(jsonvalidate)

taskData_ex <- list.files("~/schemaTest")
taskData_ex <- taskData_ex[stringr::str_detect(taskData_ex, ".json")]
taskData_ex <- paste0("~/schemaTest/", taskData_ex)

dccs <- json_validate(json=taskData_ex[1], schema="./JSONschema/taskData_schemaIntInt.json", verbose=T)
