# Instalar paquetes si no están instalados
if (!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if (!require(glue)) install.packages("glue", dependencies = TRUE)

# Cargar librerías
library(tidyverse)
library(glue)

# Función para generar la tabla en LaTeX
generar_tabla_latex <- function(df) {
  
  # Extraer las variables de la tabla
  terminos <- unique(df$term)
  modelos <- unique(df$design) # Supongo que el diseño identifica cada columna del LaTeX
  
  lab <- unique(df$label)
  
  browser()
  # Construir la cabecera de LaTeX con los títulos
  encabezado <- glue("
  \\begin{{tabular}}{{l|cc|cc|cc|cc|cc}} \\hline\\hline
  & \\multicolumn{{2}}{{c}}{{lab}} & \\multicolumn{{2}}{{c}}{{Log taxable profit}} & \\multicolumn{{2}}{{c}}{{Any taxable profit}} & \\multicolumn{{2}}{{c}}{{Log CIT liability}} & \\multicolumn{{2}}{{c}}{{Any CIT liability}} \\\\ \\hline
  & {paste0('(', 1:length(modelos), ')', collapse = ' & ')} \\\\ \\hline")
  
  # Construir las filas de valores de las regresiones
  filas <- terminos %>%
    map_chr(~ {
      coeficientes <- df %>%
        filter(term == .x) %>%
        arrange(match(design, modelos)) %>%
        pull(estimate)
      
      errores <- df %>%
        filter(term == .x) %>%
        arrange(match(design, modelos)) %>%
        pull(std.error)
      
      glue("{.x} & {paste(coeficientes, collapse = ' & ')} \\\\ \n & ({paste(errores, collapse = ' & ')}) \\\\")
    }) %>%
    paste(collapse = "\n")
  
  # Agregar las filas fijas (fixed effects, N, R²)
  filas_fijas <- glue("
  \\hline
  Firm fixed effects & {paste(rep('N & Y', length.out = length(modelos)), collapse = ' & ')} \\\\
  Weight (2014 log assets) & {paste(rep('N', length(modelos)), collapse = ' & ')} \\\\
  N & {paste(df$nobs[1:length(modelos)], collapse = ' & ')} \\\\
  Adjusted R2 & {paste(df$r.squared[1:length(modelos)], collapse = ' & ')} \\\\ \\hline\\hline
  \\end{{tabular}}
  ")
  
  # Juntar todo en una sola tabla
  latex_table <- glue("{encabezado}\n{filas}\n{filas_fijas}")
  
  return(latex_table)
}

# Función para guardar la tabla en un archivo LaTeX
guardar_tabla_latex <- function(df, file_name = "resultados.tex") {
  tabla_latex <- generar_tabla_latex(df)
  writeLines(tabla_latex, file_name)
}

tabla <- read.csv("data/model_table_20250318.csv") %>% 
  tibble()

diccionario <- tabla %>% 
  distinct(dependent_variable, label)

tabla_pre_post <- read.csv("20250318/4_time_series_grouped_post.txt") %>% 
  tibble() %>% 
  # round all numeric to 3 digit with across
  mutate(across(where(is.numeric), ~round(., 3))) 
  

dicc_1 <- diccionario %>% 
  filter(dependent_variable %in% c("total_revenues",	"total_sales",	"total_cost_expenses",	"labor_cost",	"total_assets",	"tangible_assets"))

means_1 <- tabla_pre_post %>% 
  select(post, group_assign,one_of(c(c("total_revenues",	"total_sales",	"total_cost_expenses",	"labor_cost",	"total_assets",	"tangible_assets"))), sample) %>% 
  filter(group_assign %in% c("T-Maj","C-Maj"),
         sample == "all") %>% 
  rename_with(.cols = all_of(dicc_1$dependent_variable), ~dicc_1$label) %>% 
  select(-sample) %>% 
  rename_with(.cols = 3:8, .fn = ~str_c(.x,"unweighted",sep = "__")) %>% 
  rename(term = post, values = group_assign) 

tabla_did <- tabla %>% 
  filter(sample_spec  == "all",
         treatment_spec == "_majors",
         design == "did",
         # weight_spec == "unweighted",
         fe_spec == "twfe",
         dependent_variable %in% c("total_revenues",	"total_sales",	"total_cost_expenses",	"labor_cost",	"total_assets",	"tangible_assets"))  
  # mutate(term = str_remove(term, ".+\\(.+\\)"),
  #        term = str_remove(term, ".+::"),
  #        term = str_remove(term, ":.+"),
  #        term = as.numeric(term)
  # ) 

tabla_1 <- tabla_did %>% 
  # filter(str_detect(term, "Inter",negate = T)) %>%
  select(label,weight_spec, term, estimate, std.error, nobs, uniques,ceros) %>%
  pivot_longer(cols = c(estimate,
                        std.error,
                        nobs, uniques, ceros), 
               names_to = "values") %>% 
  mutate(value = round(value, 3)) %>% 
  pivot_wider(names_from = label, values_from = value) %>% 
  split(.$weight_spec) %>% 
  map(select, -weight_spec) %>% 
  imap(~{
    lan <- .y
    .x %>% 
      rename_with(.cols = 3:8, ~str_c(.x, lan ,sep= "__")) 
    
  }) %>% 
  reduce(full_join) %>% 
  select(term, values, sort(tidyselect::peek_vars())) %>% 
  bind_rows(means_1)
  


dicc_2 <- diccionario %>% 
  filter(dependent_variable %in% c("log_investments","bin_investments",
                                   "log_inversiones_no_corrientes",
                                   "bin_inversiones_no_corrientes",	"log_current_investment",	"bin_current_investment",	"log_uti_reinvertir_cpz_3580",	"bin_uti_reinvertir_cpz_3580"))

means_2 <- tabla_pre_post %>% 
  select(post, group_assign,one_of(c(c("log_investments","bin_investments",
                                       "log_inversiones_no_corrientes",
                                       "bin_inversiones_no_corrientes",	"log_current_investment",	"bin_current_investment",	"log_uti_reinvertir_cpz_3580",	"bin_uti_reinvertir_cpz_3580"))), sample) %>% 
  filter(group_assign %in% c("T-Maj","C-Maj"),
         sample == "all") %>% 
  rename_with(.cols = all_of(dicc_2$dependent_variable), ~dicc_2$label) %>% 
  select(-sample) %>% 
  rename_with(.cols = 3:10, .fn = ~str_c(.x,"unweighted",sep = "__")) %>% 
  rename(term = post, values = group_assign) 

tabla_did <- tabla %>% 
  filter(sample_spec  == "all",
         treatment_spec == "_majors",
         design == "did",
         # weight_spec == "unweighted",
         fe_spec == "twfe",
         
         #bin_investments	log_inversiones_no_corrientes	bin_inversiones_no_corrientes	log_current_investment	bin_current_investment	log_uti_reinvertir_cpz_3580	bin_uti_reinvertir_cpz_3580
         
         dependent_variable %in% c("log_investments","bin_investments",
                                   "log_inversiones_no_corrientes",
                                   "bin_inversiones_no_corrientes",	"log_current_investment",	"bin_current_investment",	"log_uti_reinvertir_cpz_3580",	"bin_uti_reinvertir_cpz_3580"))  


tabla_2 <- tabla_did %>% 
  select(label,weight_spec, term, estimate, std.error, nobs, ceros,uniques) %>%
  mutate(nobs = 27738) %>% 
  pivot_longer(cols = c(estimate,
                        std.error,
                        nobs, uniques,ceros), 
               names_to = "values") %>% 
  mutate(value = round(value, 3)) %>% 
  pivot_wider(names_from = label, values_from = value) %>% 
  split(.$weight_spec) %>% 
  map(select, -weight_spec) %>% 
  imap(~{
    lan <- .y
    .x %>% 
      rename_with(.cols = 3:10, ~str_c(.x, lan ,sep= "__")) 
    
  }) %>% 
  reduce(full_join) %>% 
  select(term, values, sort(tidyselect::peek_vars())) %>% 
  bind_rows(means_2)


dicc_3 <- diccionario %>% 
  filter(dependent_variable %in% c("log_interest_payments","bin_interest_payments",
                                   "log_interest_payments_related",
                                   "bin_interest_payments_related",	"log_interest_payments_unrelated",
                                   "bin_interest_payments_unrelated",	"log_interest_payments_local",	"bin_interest_payments_local"))

means_3 <- tabla_pre_post %>% 
  select(post, group_assign,one_of(c(c("log_interest_payments","bin_interest_payments",
                                       "log_interest_payments_related",
                                       "bin_interest_payments_related",	"log_interest_payments_unrelated",
                                       "bin_interest_payments_unrelated",	"log_interest_payments_local",	"bin_interest_payments_local"))), sample) %>% 
  filter(group_assign %in% c("T-Maj","C-Maj"),
         sample == "all") %>% 
  rename_with(.cols = all_of(dicc_3$dependent_variable), ~dicc_3$label) %>% 
  select(-sample) %>% 
  rename_with(.cols = 3:10, .fn = ~str_c(.x,"unweighted",sep = "__")) %>% 
  rename(term = post, values = group_assign) 

tabla_did <- tabla %>% 
  filter(sample_spec  == "all",
         treatment_spec == "_majors",
         design == "did",
         # weight_spec == "unweighted",
         fe_spec == "twfe",
         
         # log_interest_payments	bin_interest_payments	log_interest_payments_related	bin_interest_payments_related	log_interest_payments_unrelated	bin_interest_payments_unrelated	log_interest_payments_local	bin_interest_payments_local

         dependent_variable %in% c("log_interest_payments","bin_interest_payments",
                                   "log_interest_payments_related",
                                   "bin_interest_payments_related",	"log_interest_payments_unrelated",
                                   "bin_interest_payments_unrelated",	"log_interest_payments_local",	"bin_interest_payments_local"))  
# mutate(term = str_remove(term, ".+\\(.+\\)"),
#        term = str_remove(term, ".+::"),
#        term = str_remove(term, ":.+"),
#        term = as.numeric(term)
# ) 

tabla_3 <- tabla_did %>% 
  # filter(str_detect(term, "Inter",negate = T)) %>%
  select(label,weight_spec, term, estimate, std.error, nobs, uniques, ceros) %>%
  pivot_longer(cols = c(estimate,
                        std.error,
                        nobs, uniques, ceros), 
               names_to = "values") %>% 
  mutate(value = round(value, 3)) %>% 
  pivot_wider(names_from = label, values_from = value) %>% 
  split(.$weight_spec) %>% 
  map(select, -weight_spec) %>% 
  imap(~{
    lan <- .y
    .x %>% 
      rename_with(.cols = 3:10, ~str_c(.x, lan ,sep= "__")) 
    
  }) %>% 
  reduce(full_join) %>% 
  select(term, values, sort(tidyselect::peek_vars())) %>% 
  bind_rows(means_3)


# con openxlsx guardar tabla_1, 2 y 3 en diferentes hojas de excel
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Panel a")
addWorksheet(wb, "Panel b")
addWorksheet(wb, "Panel c")
writeData(wb, "Panel a", tabla_1)
writeData(wb, "Panel b", tabla_2)
writeData(wb, "Panel c", tabla_3)
saveWorkbook(wb, "tabla_did_19032025.xlsx", overwrite = TRUE)


tabla_did %>% 
  write_csv("tabla_did_1.csv")
generar_tabla_latex(tabla_did)

# Ejecutar la función con tu data frame
guardar_tabla_latex(df_resultados)
