

funcao_demanda_oferta <- 
  function(tempo1, tempo2, 
           perc_direto1, perc_direto2) 
    {
        professional_services_2 <- 
          phc_services %>%
          left_join(qtt_professional, 
                    by = "codigo_sigtap", 
                    suffix = c("_phc", "_qtt")) %>%
          mutate(qtt_cadre = quantidade / n) %>%
          left_join(procedures_professional, 
                    by = "codigo_sigtap", 
                    suffix = c("_phc", "_procedures")) %>%
          filter(categoria == "Enfermeiro" | 
                 categoria == "Médico") %>%
          filter(tipo_procedimento == "Consultas ou Visitas" |
                 tipo_procedimento == "Ações Educacionais") %>%
          mutate(ano = year(mês_procedimento_realizado)) %>%
          filter(ano == 2024) %>%
          mutate(tempo = 
                   if_else(
                    tipo_procedimento == "Consultas ou Visitas",
                    qtt_cadre * tempo1, # 20 minutos
                    qtt_cadre * tempo2
                          )
                 )
  
  demanda_2 <- 
    professional_services_2 %>%
    select(ano, mês_procedimento_realizado, 
           ibge, CBO, categoria, tempo) %>%
    mutate(fte40 = tempo / 126) %>%
    group_by(ano, mês_procedimento_realizado, 
             ibge, CBO, categoria) %>%
    summarise(
      tempo_total = sum(tempo),
      fte40_demanda = sum(fte40),
      fte40_demanda = round(fte40_demanda, 2)
    )
  
  oferta_2 <- 
    oferta_GO %>%
    mutate(cod_regsaud = as.character(cod_regsaud)) %>%
    mutate(ano_mes = ym(COMPETEN)) %>%
    mutate(horas = HORAOUTR + HORAHOSP + HORA_AMB) %>%
    mutate(prof = if_else(substr(CBO, 1, 4) == "2235", 
                          "Enfermeiro", "Médico")) %>%
    group_by(uf, cod_regsaud, regiao_saude, 
             prof, ano_mes) %>%
    summarise(horas = 4.345 * sum(horas)) %>%
    left_join(producao_SISAB, 
              by = c("cod_regsaud" = "Cod_Regiao_Saude")) %>%
    mutate(fte40 = horas / 126) %>%
    mutate(direto = if_else(prof == "Enfermeiro",
                            fte40 * perc_direto1,
                            fte40 * perc_direto2)) %>%
    mutate(liquido = direto * Porcentagem) %>%
    mutate(ano_mes_corrigido = ano_mes + years(2)) %>%
    select(-ano_mes) %>%
    mutate(
      Porcentagem = round(Porcentagem, 2),
      fte40 = round(fte40, 2),
      direto = round(direto, 2),
      liquido = round(liquido, 2)
    )
  
  demanda_2$ibge <- as.character(demanda_2$ibge)
  
  demanda_oferta_2 <- demanda_2 %>%
    left_join(oferta_2, 
              by = c("ibge" = "cod_regsaud", 
                     "categoria" = "prof", 
                     "mês_procedimento_realizado" =
                     "ano_mes_corrigido"),
              suffix = c("_demanda", "_oferta")) %>%
    filter(uf != "NA") %>%
    mutate(ano = year(mês_procedimento_realizado)) %>%
    mutate(resultado = liquido - fte40_demanda) %>%
    group_by(ibge, ano, categoria, 
             uf, regiao_saude) %>%
    summarise(
      resultado = sum(resultado),
      demanda = sum(fte40_demanda),
      oferta = sum(liquido)
    )

  return(demanda_oferta_2)
}

###############

library(geojsonio)
library(broom)
library(jsonlite)
library(RODBC)
library(patchwork)
library(geobr)

cenario_5 <- funcao_demanda_oferta(0.50, 1.00, 
                                   0.60, 0.69) |>  
  mutate(percentage = (oferta * 100)/demanda,
         percentage = round(percentage, 2)) |> 
  mutate(id = as.integer(ibge)) |> 
  select(id, regiao_saude, categoria, 
         resultado, percentage)

DT::datatable(cenario_5)


spdf <- geojson_read("shape file regioes saude.json",  what = "sp")

cenario_5$id <- as.integer(cenario_5$id)

spdf_region <- spdf[ spdf@data$est_id == "52" , ]


spdf_fortified <- sf::st_as_sf(spdf_region)

x <- spdf_fortified |>
  left_join(cenario_5, by = c("reg_id"="id")) |>
  mutate(categoria = if_else(categoria == "Médico","Physician","Nurse")) |> 
  ggplot() +
  geom_sf(aes(fill = percentage)) +
  geom_sf_text(aes(label = regiao_saude), size = 3.5) +
  theme_minimal() +
  scale_fill_gradient(low = "#F8766D", high = "#00BA38", n.breaks = 10) +
  facet_wrap(~categoria, nrow = 1) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) + ggtitle("Scenario 5")

x



## APP backup


library(tidyverse)
library(shiny)
library(bslib)
library(readxl)
library(geojsonio)




# Reading Data ------------------------------------------------------------

phc_services <- read_excel("bases/input_shiny.xlsx")
procedures_professional <- read_excel("bases/procedures_prof_shiny.xlsx")
qtt_professional <- read_excel("bases/qtt_prof_shiny.xlsx")
oferta_GO <- read_excel("bases/supply_shiny.xlsx")
producao_SISAB <- read_excel("bases/sisab_shiny.xlsx")

# Preparing APP -----------------------------------------------------------


# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    # Application title
    titlePanel("Health workforce planning - sensitivy analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("private",
                      "Percentage of SUS dependent population which consume private services",
                      min = 0,
                      max = 0.30,
                      value = 0.10),  
          sliderInput("time1",
                      "Appointment time (in hours):",
                      min = 0.30,
                      max = 0.75,
                      value = 0.50),
            sliderInput("time2",
                        "Educative action (in hours):",
                        min = 0.50,
                        max = 1.50,
                        value = 1.00),
            sliderInput("awt",
                      "Available working time",
                      min = 1350,
                      max = 1600,
                      value = 1512),
            sliderInput("perc_nurse",
                        "Percentage of time in direct activities (Nurse):",
                        min = 0.30,
                        max = 1.00,
                        value = 0.60),
            sliderInput("perc_physician",
                        "Percentage of time in direct activities (Physician):",
                        min = 0.30,
                        max = 1.00,
                        value = 0.50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- 
     
       renderPlot({
          
         professional_services_2 <- 
            
           phc_services |>
            
           left_join(qtt_professional, by = "codigo_sigtap", 
                      suffix = c("_phc", "_qtt")) |>
           
           mutate(quantidade = quantidade - (input$private * quantidade)) |> 
            
           mutate(qtt_cadre = quantidade / n) |>
           
           left_join(procedures_professional, by = "codigo_sigtap", 
                      suffix = c("_phc", "_procedures")) |>
           
           filter(categoria == "Enfermeiro" | 
                   categoria == "Médico") |>
           
           filter(tipo_procedimento == "Consultas ou Visitas" |
                   tipo_procedimento == "Ações Educacionais") |>
            
           mutate(ano = year(mês_procedimento_realizado)) |>
           
           filter(ano == 2024) |>
           mutate(tempo = 
                     if_else(
                       tipo_procedimento == "Consultas ou Visitas",
                       qtt_cadre * input$time1, # 20 minutos
                       qtt_cadre * input$time2)
                  )
         
        demanda_2 <- 
            
            professional_services_2 |>
            
            select(ano, mês_procedimento_realizado, 
                  ibge, CBO, categoria, tempo) |>
            
            mutate(fte40 = tempo / (input$awt/12)) |>
            
            group_by(ano, mês_procedimento_realizado, 
                    ibge, CBO, categoria) |>
            summarise(
                 tempo_total = sum(tempo),
                 fte40_demanda = sum(fte40),
                 fte40_demanda = round(fte40_demanda, 2)
            )
        
        oferta_2 <- 
          
          oferta_GO |>
          
          mutate(cod_regsaud = as.character(cod_regsaud)) |>
          mutate(ano_mes = ym(COMPETEN)) |>
          mutate(horas = HORAOUTR + HORAHOSP + HORA_AMB) |>
          mutate(prof = if_else(substr(CBO, 1, 4) == "2235", 
                                "Enfermeiro", 
                                "Médico")) |>
          
          group_by(uf, cod_regsaud, regiao_saude, 
                   prof, ano_mes) |>
          summarise(horas = 4.345 * sum(horas)) |>
          
          left_join(producao_SISAB, 
                    by = c("cod_regsaud" = "Cod_Regiao_Saude")) |>
          
          mutate(fte40 = horas / (input$awt/12)) |>
          mutate(direto = if_else(prof == "Enfermeiro",
                                  fte40 * input$perc_nurse,
                                  fte40 * input$perc_physician)) |>
          mutate(liquido = direto * Porcentagem) |>
          mutate(ano_mes_corrigido = ano_mes + years(2)) |>
          
          select(-ano_mes) |>
          mutate(
            Porcentagem = round(Porcentagem, 2),
            fte40 = round(fte40, 2),
            direto = round(direto, 2),
            liquido = round(liquido, 2)
          )
        
        demanda_2$ibge <- as.character(demanda_2$ibge)
        
        demanda_oferta_2 <- demanda_2 |>
          left_join(oferta_2, 
                    by = c("ibge" = "cod_regsaud", 
                           "categoria" = "prof", 
                           "mês_procedimento_realizado" =
                           "ano_mes_corrigido"),
                    suffix = c("_demanda", "_oferta")) |>
          filter(uf != "NA") |>
          
          mutate(ano = year(mês_procedimento_realizado)) |>
          mutate(resultado = liquido - fte40_demanda) |>
          
          group_by(ibge, ano, categoria, 
                   uf, regiao_saude) |>
          summarise(
            resultado = sum(resultado),
            demanda = sum(fte40_demanda),
            oferta = sum(liquido)
          )
        
        cenario <- demanda_oferta_2 |>  
          mutate(percentage = (oferta * 100)/demanda,
                 percentage = round(percentage, 2)) |> 
          mutate(id = as.integer(ibge)) |> 
          select(id, regiao_saude, categoria, 
                 resultado, percentage)
      
       cenario |> 
          ggplot(aes(x = regiao_saude, y = percentage)) +
          geom_label(aes(label = percentage)) +
          geom_col() + facet_wrap(~categoria, nrow = 2) + 
          theme_minimal()
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)














#### app completo 


library(tidyverse)
library(shiny)
library(bslib)
library(readxl)
library(geojsonio)
library(renv)
renv::init()




# Reading Data ------------------------------------------------------------

phc_services <- read_excel("bases/input_shiny.xlsx")
procedures_professional <- read_excel("bases/procedures_prof_shiny.xlsx")
qtt_professional <- read_excel("bases/qtt_prof_shiny.xlsx")
oferta_GO <- read_excel("bases/supply_shiny.xlsx")
producao_SISAB <- read_excel("bases/sisab_shiny.xlsx")

# Preparing APP -----------------------------------------------------------


# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    # Application title
    titlePanel("Health workforce planning - sensitivy analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("private",
                      "Percentage of SUS dependent population which consume private services",
                      min = 0,
                      max = 0.30,
                      value = 0.10),  
          sliderInput("time1",
                      "Appointment time (in hours):",
                      min = 0.30,
                      max = 0.75,
                      value = 0.50),
            sliderInput("time2",
                        "Educative action (in hours):",
                        min = 0.50,
                        max = 1.50,
                        value = 1.00),
            sliderInput("awt",
                      "Available working time",
                      min = 1350,
                      max = 1600,
                      value = 1512),
            sliderInput("perc_nurse",
                        "Percentage of time in direct activities (Nurse):",
                        min = 0.30,
                        max = 1.00,
                        value = 0.60),
            sliderInput("perc_physician",
                        "Percentage of time in direct activities (Physician):",
                        min = 0.30,
                        max = 1.00,
                        value = 0.50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot", width = "100%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- 
     
       renderPlot({
          
         professional_services_2 <- 
            
           phc_services |>
            
           left_join(qtt_professional, by = "codigo_sigtap", 
                      suffix = c("_phc", "_qtt")) |>
           
           mutate(quantidade = quantidade - (input$private * quantidade)) |> 
            
           mutate(qtt_cadre = quantidade / n) |>
           
           left_join(procedures_professional, by = "codigo_sigtap", 
                      suffix = c("_phc", "_procedures")) |>
           
           filter(categoria == "Enfermeiro" | 
                  categoria == "Médico") |>
           
           filter(tipo_procedimento == "Consultas ou Visitas" |
                   tipo_procedimento == "Ações Educacionais") |>
            
           mutate(ano = year(mês_procedimento_realizado)) |>
           
           filter(ano == 2024) |>
           mutate(tempo = 
                     if_else(
                       tipo_procedimento == "Consultas ou Visitas",
                       qtt_cadre * input$time1, # 20 minutos
                       qtt_cadre * input$time2)
                  )
         
        demanda_2 <- 
            
            professional_services_2 |>
            
            select(ano, mês_procedimento_realizado, 
                  ibge, CBO, categoria, tempo) |>
            
            mutate(fte40 = tempo / (input$awt/12)) |>
            
            group_by(ano, mês_procedimento_realizado, 
                    ibge, CBO, categoria) |>
            summarise(
                 tempo_total = sum(tempo),
                 fte40_demanda = sum(fte40),
                 fte40_demanda = round(fte40_demanda, 2)
            )
        
        oferta_2 <- 
          
          oferta_GO |>
          
          mutate(cod_regsaud = as.character(cod_regsaud)) |>
          mutate(ano_mes = ym(COMPETEN)) |>
          mutate(horas = HORAOUTR + HORAHOSP + HORA_AMB) |>
          mutate(prof = if_else(substr(CBO, 1, 4) == "2235", 
                                "Enfermeiro", 
                                "Médico")) |>
          
          group_by(uf, cod_regsaud, regiao_saude, 
                   prof, ano_mes) |>
          summarise(horas = 4.345 * sum(horas)) |>
          
          left_join(producao_SISAB, 
                    by = c("cod_regsaud" = "Cod_Regiao_Saude")) |>
          
          mutate(fte40 = horas / (input$awt/12)) |>
          mutate(direto = if_else(prof == "Enfermeiro",
                                  fte40 * input$perc_nurse,
                                  fte40 * input$perc_physician)) |>
          mutate(liquido = direto * Porcentagem) |>
          mutate(ano_mes_corrigido = ano_mes + years(2)) |>
          
          select(-ano_mes) |>
          mutate(
            Porcentagem = round(Porcentagem, 2),
            fte40 = round(fte40, 2),
            direto = round(direto, 2),
            liquido = round(liquido, 2)
          )
        
        demanda_2$ibge <- as.character(demanda_2$ibge)
        
        demanda_oferta_2 <- demanda_2 |>
          left_join(oferta_2, 
                    by = c("ibge" = "cod_regsaud", 
                           "categoria" = "prof", 
                           "mês_procedimento_realizado" =
                           "ano_mes_corrigido"),
                    suffix = c("_demanda", "_oferta")) |>
          filter(uf != "NA") |>
          
          mutate(ano = year(mês_procedimento_realizado)) |>
          mutate(resultado = liquido - fte40_demanda) |>
          
          group_by(ibge, ano, categoria, 
                   uf, regiao_saude) |>
          summarise(
            resultado = sum(resultado),
            demanda = sum(fte40_demanda),
            oferta = sum(liquido)
          )
        
        cenario <- demanda_oferta_2 |>  
          mutate(percentage = (oferta * 100)/demanda,
                 percentage = round(percentage, 2)) |> 
          mutate(id = as.integer(ibge)) |> 
          select(id, regiao_saude, categoria, 
                 resultado, percentage)
      
        demanda_oferta_2 |> 
          gather(key = "component", value = "valor", 7:8) |> 
          mutate(valor = round(valor)) |> 
          mutate(component = if_else(component == "demanda",
                                     "demand","supply")) |> 
          ggplot(aes(x = regiao_saude, y = valor, fill = component)) + 
          geom_col(position = "dodge") + theme_minimal() + 
          geom_text(aes(label = valor, angle = 45), position = position_dodge(width = .9)) +
          facet_wrap(~categoria, nrow = 2) + xlab("health region") +
          ylab("Total of professionals (FTE)") + ggtitle("Demand vs Supply by health region") +
          theme(text = element_text(size = 20),
                axis.text.x = element_text(angle = 45, hjust=1),
                axis.title.x = element_blank())  
          
          
        

    }, height = 650, width = 1200 
)
}

# Run the application 
shinyApp(ui = ui, server = server)
