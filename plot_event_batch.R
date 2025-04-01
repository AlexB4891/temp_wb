

# Librerias ---------------------------------------------------------------

library(tidyverse)
library(cowplot)
# Para generar tablas de latex
library(xtable)
library(ggrepel)
library(furrr)

tabla <- read.csv("data/model_table_20250325.csv") %>% 
  tibble()

# Crear directorios a partir de los valores en la columna sample_spec

tabla %>% 
  pull(sample_spec) %>%
  unique() %>%
  walk(~dir.create(paste0("20250325/graphs/", .x)))



# Crear graficos para cada directorio

tabla_es <- tabla %>% 
  filter(sample_spec  == "all",
         # treatment_spec == "_majors",
         design == "event_study",
         weight_spec == "unweighted",
         fe_spec == "twfe",
         dependent_variable == "average_ultimate",
         str_detect(term, ".+treat"))  %>% 
  mutate(term = str_remove(term, ".+\\(.+\\)"),
         term = str_remove(term, ".+::"),
         term = str_remove(term, ":.+"),
         term = as.numeric(term)
         ) 

# Modificar labels de las variables realcionadas a los modelos

format_labels <- function(tabla_es){

tabla_es  %>% 
    filter(str_detect(term, ".+treat")) %>% 
    mutate(term = str_remove(term, ".+\\(.+\\)"),
           term = str_remove(term, ".+::"),
           term = str_remove(term, ":.+"),
           term = as.numeric(term)
    ) %>% 
  mutate(
    treatment_spec = case_when(
      treatment_spec == "_majors" ~ "Majors",
      treatment_spec == "_minors" ~ "Minors",
      treatment_spec == "_joint" ~ "Joint"
    ),
    # design is did and es es event study
    design = case_when(
      design == "did" ~ "DID",
      design == "event_study" ~ "Event Study"
    ),
    # weight_spec is unweighted and weighted
    weight_spec = case_when(
      weight_spec == "unweighted" ~ "Unweighted",
      weight_spec == "log_assets_2014" ~ "Weighted"
    ),
    # fe_spec is twfe and noabsorb
    fe_spec = case_when(
      fe_spec == "twfe" ~ "TWFE",
      fe_spec == "noabsorb" ~ "No Absorb"
    ),
    # sample spec is: 
    sample_spec = case_when(
      sample_spec == "all" ~ "All firms",
      sample_spec == "all_rev" ~ "Active firms",
      sample_spec == "alt" ~ "Alternative treatment",
      sample_spec == "treat_a" ~ "Treat A",
      sample_spec == "treat_b" ~ "Treat B",
      sample_spec == "treat_c" ~ "Treat C"
    )
  )

}

tabla_es <- format_labels(tabla_es)

plot_slides <- function(tabla, 
                        title,
                        subtitle,
                        # weight, fe, sample, depen, treat,
                        label){
  
  
  # browser()
  
  samples <- tabla %>% 
    distinct(treatment_spec) %>% 
    pull()
  
  tabla_grafico <- tabla %>% 
    bind_rows(tibble(term = rep(2014,length(samples)),
                     estimate = rep(0,length(samples)),
                     conf.low = rep(0,length(samples)),
                     conf.high = rep(0,length(samples)),
                     treatment_spec  = samples)) %>% 
    tidyr::fill(everything(),.direction = "downup") %>% 
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      color = case_when(
        treatment_spec == "Majors" ~ "#3B079E",
        TRUE ~ "lightgray"
      )
    )   
  
  tabla_end <- tabla_grafico %>% 
    filter(term == 2019) %>% 
    mutate(etiqueta = case_when(
      treatment_spec == "Joint" ~ "Tratamiento conjunto",
      treatment_spec == "Majors" ~ "Majors >= 50%",
      treatment_spec == "Minors" ~ "Minors < 50%",
    ),
    color = case_when(
      treatment_spec == "_majors" ~ "#3B079E",
      TRUE ~ "lightgray"
    ))
  
  
  tabla_grafico %>% 
    ggplot(aes(x = term, 
               y = estimate,
               color = color,
               group = treatment_spec)) +
    geom_point(size = 4)  +
    geom_linerange(aes(ymin = conf.low,
                       ymax = conf.high)) +
    geom_line() +
    geom_text_repel(data = tabla_end,
                    aes(label = etiqueta,
                        x = term,
                        y = estimate,
                        color = color,
                        group = treatment_spec),
                    hjust = -0.3,
                    segment.color = NA) +
    # geom_hline(aes(yintercept = 0),linetype = "dashed") +
    geom_segment(x = 2012, xend = 2019, y = 0, yend = 0, color = "black") +
    geom_vline(aes(xintercept = 2014.5),linetype = "dashed") +
    scale_x_continuous(breaks = 2007:2019,
                       labels = c(2007:2019))  +
    scale_color_manual(values = c("#3B079E","lightgray")) +
    theme_minimal(16) +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.title.x = element_blank()
    ) +
    labs(title = title,
         subtitle = subtitle,
         y = label) +
    expand_limits(x = c(2012,2021)) 
  
}

plan(multisession, workers = 8)

 tabla %>% 
  filter(design == "event_study") %>% 
  split(list(.$sample_spec, .$dependent_variable, .$weight_spec, .$fe_spec)) %>% 
  future_walk(~{
    
    var_lab <- .x %>% pull(label) %>% unique()
    # browser()
    file <- .x %>% distinct(dependent_variable, weight_spec, fe_spec) %>% unlist() %>% str_c(collapse = "_")
    
    tabla <- format_labels(.x)
    
    sub <- tabla %>% distinct(sample_spec, weight_spec, fe_spec) %>% unlist() %>% str_c(collapse = ", ")
    
    plot_slides(tabla,
                title = var_lab,
                subtitle = sub,
                label = var_lab
           
    ) %>%
      ggsave(plot = .,filename = paste0("20250325/graphs/", .x$sample_spec[1], "/event_study_", file, ".png"), width = 10, height = 6, 
             create.dir = T)
  })

 plan(sequential)