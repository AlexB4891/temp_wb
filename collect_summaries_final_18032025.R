library(tidyverse)
library(fixest)
library(broom)
library(scales)
library(here)
library(readxl)
library(openxlsx)

source("global.R")

# population <- c("all","cit_m0","main_group_0",
#                 "main_group_50","inv_50","domestic_50",
#                 "main_group_0_citm0","main_group_50_citm0",
#                 "inv_50_citm0","domestic_50_citm0",
#                 "citm0_alt","domestic_50_alt",
#                 "domestic_50_citm0_alt")

readxl::excel_sheets("20250204/Tablas/diccionario_ejecucion_temp.xlsx")
# 
# dicc_label <- read_excel("data/diccionario_con_labels.xlsx",
#                          sheet = "Diccionario 20240624") 

excel_sheets("data/diccionario_con_labels.xlsx")

dicc_label <- read_excel("20250314/Tablas/diccionario_ejecucion_temp.xlsx") 
# dicc_label_2 <- read_excel("20241210/Tablas/diccionario_ejecucion_temp.xlsx")

# dicc_label_sri <- read_excel("20241014/Tablas/diccionario_ejecucion.xlsx",
#                          sheet = "Hoja1")
# 
# dicc_label_sri <- dicc_label_sri %>%
#   filter(!is.na(variable))

presentes <- list.files("20250318/Tablas/params/") %>%
  map_chr(str_remove_all,"^dep_var_|.txt$")

dicc_label_sri <- tibble(variable = presentes)

dicc_label_sri <- dicc_label_sri %>% 
  left_join(dicc_label)

wb <- createWorkbook()
# 
addWorksheet(wb, "Hoja1")
# 
writeData(wb, "Hoja1", dicc_label_sri)
# 
saveWorkbook(wb, "20250318/Tablas/diccionario_ejecucion_temp.xlsx", overwrite = TRUE)

fechas <- c("20250318")

dicc_label <- read_excel("20250318/Tablas/diccionario_ejecucion_temp.xlsx") 


presentes <-  fechas %>%
  str_c("/params") %>%
  map(list.files) %>%
  map(unlist)

# presentes[[1]] <- presentes[[1]][!presentes[[1]] %in% presentes[[2]]]

# fechas <- c("20240624")
# 
# presentes <-  fechas %>% 
#   str_c("/params") %>% 
#   map(list.files) %>% 
#   map(unlist)

# presentes[[1]] <- presentes[[1]][!presentes[[1]] %in% presentes[[2]]]

rutas <- map2(.x = fechas, .y = presentes,
     .f = ~{
       
       list(ruta_params = str_c(.x,"/params/",.y),
            ruta_perfor = str_c(.x,"/performance/",.y))
     }) %>% 
  transpose() %>% 
  map(reduce,c)

presentes <- presentes %>% reduce(c)


diccionario_global <- tibble(variable = presentes, 
       ruta_param = rutas[[1]],
       ruta_perfo = rutas[[2]]) %>% 
  mutate(variable = str_remove_all(variable,"^dep_var_|.txt$")) %>% 
  left_join(dicc_label) 

# ruta <- diccionario_global[1,"ruta_param"] 

diccionario_global <- diccionario_global %>% 
  mutate(tamano = file.info(ruta_param)%>% pull(size)) %>% 
  filter(tamano > 2000,
         !is.na(label)) %>% 
  select(-tamano)

# diccionario_global <- diccionario_global %>% 
#   mutate(label = variable)

# file.info("20240624/params/dep_var_bin_total_assets.txt") 

procesamiento <- pmap(.l = diccionario_global,
     function(variable, ruta_param, ruta_perfo, label){
       
       suma <- sum(file.exists(ruta_param,ruta_perfo))
       
       if(suma == 2){
         
        print(variable)
         
         # Lee el archivo y omite la primera columna
         param <-  read.delim(ruta_param,sep = "\t") %>% 
           as_tibble()
         
         
         perfor <-  read.delim(ruta_perfo,sep = "\t") %>% 
           as_tibble()
         
         # browser()
         
         list(param,perfor) %>% 
           map(~.x %>% 
                 mutate(treatment_spec = str_extract(population,"_joint|_majors|_minors"),
                        sample_spec = str_remove(population,treatment_spec)) %>%
                 mutate(design = if_else(str_detect(model, "Event study"),
                                         "event_study",
                                         "did"),
                        weight_spec = if_else(str_detect(model, "assets"),
                                              "log_assets_2014",
                                              "unweighted"),
                        fe_spec = if_else(str_detect(model, "fixed"),
                                          "twfe",
                                          "noabsorb"),
                        dependent_variable = variable,
                        label = label
                        ) %>% 
                 select(one_of(c("dependent_variable",
                                         "label",
                                 "ceros",
                                 "design",
                                 "treatment_spec",
                                 "sample_spec",
                                 "weight_spec",
                                 "fe_spec",
                                 "term",
                                 "estimate",
                                 "std.error",
                                 "p.value",
                                 "r.squared",
                                 "logLik",
                                 "uniques",
                                 "nobs"))) 
                 )   %>%
           reduce(inner_join, by = c("dependent_variable", "label", "design", "treatment_spec", "sample_spec", "weight_spec", "fe_spec")) %>% 
           rename(p_value = p.value.x ) %>% 
           select(-p.value.y)

     }}) %>% 
  reduce(bind_rows)

procesamiento %>% 
  count(dependent_variable)

write_csv(procesamiento,"data/model_table_20250318.csv")

# anterior <- read_csv("data/model_table_20241009.csv")


procesamiento %>% 
  bind_rows(anterior) %>%
  write_csv("data/model_table_20241014.csv")


