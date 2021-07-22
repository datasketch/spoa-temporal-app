#webshot::install_phantomjs()
library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shinydisconnect)
library(shinybusy)
library(dsmodules)
library(lfltmagic)
library(hgchmagic)


style <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans&display=swap');


@media screen and (max-width: 768px) { 
.layout-container{
  min-height: 600px !important;} 
}

.orientation-notice {
 display: none !important;
}

.panel {
 font-size: 0.85rem;
 height: 100%; /*90%;*/
}

.panel-body {
 padding-bottom: 2rem;
}


.shinybusy, .shinybusy-ready {
 top: 30% !important;
 right: 50% !important;
}

.text-malibu {
  color: #0a4a83;
}

.top-malibu {
  border-top: 1.5px solid #0a4a83;
}

.text-chardonnay {
  color: #0a4a83;
}

.top-chardonnay {
    border-top: 1.5px solid #0a4a83;
}

.btn-default {
 font-family: IBM Plex Sans;
 background: #ffffff !important;
 border: 2px solid #197e93;
 color: #197e93;
 float: left;
 font-size: 0.82rem;
 font-weight: 400;
 text-transform: initial;
 letter-spacing: 0;
 margin-bottom: 3px;
}

.basic_active {
  color: #ffffff !important;
  background-color: #197e93 !important;
  border-color: #197e93 !important;
}


.needed {
 margin-bottom: 9px;
 max-width: 95%;
 text-align: left;
 white-space: normal;
 word-wrap: break-word;
}

.dropdown-action-trigger {
 background-color: #197e93 !important;
}

.style_section {
 color: #197e93;
 font-size: 0.85rem !important;
 margin-bottom: 0rem;
 padding-top: 0rem;
}

input[type='radio']:checked:after {
        width: 15px;
        height: 15px;
        border-radius: 15px;
        top: -1px;
        left: -1px;
        position: relative;
        background-color: #0a4a83;
        content: '';
        display: inline-block;
        visibility: visible;
}
    
#tipo_censo {
 margin-top: 3%;
}

#relacion {
 margin-bottom: 9%;
}

#variables_principales {
 margin-top: -3%;
 margin-bottom: 7%;
}


#ss-connect-dialog a::before {
background: #0a4a83 !important;
}

.irs-bar {
    border-top: 1px solid #197e93 !important;
    border-bottom: 1px solid #197e93 !important;
    background: #197e93 !important;
}

.irs-from, .irs-to, .irs-single {
    color: #197e93 !important
}

.title-data-select {
    color: #197e93 !important;
}


.buttons-group .button-style.active-btn {
    background-color: #197e93 !important;
}


.buttons-group .button-style.active-btn:hover {
    background-color: #197e93 !important;
}

.button-checkmark {
 display: none;
}

.leaflet-control-attribution {
 display: none;
}
"

datos_siscrimel <- readRDS("data/all_spoa_data.rds")
dp <- read_csv("data/deptos_conf.csv")
dp <- dp[-32,]
ui <- panelsPage(
  disconnectMessage(
    text = "Tu sesión ha finalizado, por favor haz click aquí para recargar vista",
    refresh = "RECARGAR",
    background = "#ffffff",
    colour = "#197e93",
    size = 14,
    overlayColour = "#2a2e30",
    overlayOpacity = 0.85,
    refreshColour = "#ffffff",
    css = "padding: 4.8em 3.5em !important; box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important;"
  ),
  tags$head(tags$head(
    includeScript("js/siscrimel.js")
  )),
  styles = style,
  panel(title = "Preguntas",
        id = "azul",
        width = 300,
        body = div(
          uiOutput("base"),
          uiOutput("basicos"),
          uiOutput("controls")
        )
  ),
  panel(title = "Visualización",
        id = "naranja",
        header_right = downloadImageUI("download", dropdownLabel = "Descarga", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown"),
        can_collapse = FALSE,
        color = "chardonnay", #div(add_busy_spinner(spin = "fading-circle"),uiOutput("final_viz"))
        body = uiOutput("final_viz"), #verbatimTextOutput("aver"),#  
        footer =  div(class = "panel-header",
                      uiOutput("viz_icons"), 
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src='ds_logo.png', align = "right", width = 150, height = 110)))
  ),
  panel(title = "Detalle", 
        width = 300,
        collapsed = TRUE,
        body = uiOutput("text_info")
        
        
        ##"info click"
  )
)

server <- function(input, output, session) {
  
  output$base <- renderUI({
    var_base <- c("Delitos", "Irregularidades")
    radioButtons("variables_principales",
                 label = HTML("<div class='style_section'>Visualizar</div>"), 
                 setNames(tolower(var_base), var_base))
  })
  
  
  questions_list <- reactive({
    
    if (is.null(input$variables_principales)) return()
    
    basicos <- data.frame(id = c("general", "delito", "relacion"),
                          preguntas = c(paste0("¿Cuáles son las zonas más afectadas por ", input$variables_principales," electorales?"),
                                        paste0("¿Cuáles son los tipos de  ", input$variables_principales," electorales más comunes?"),
                                        paste0("¿Cómo se relacionan   ", input$variables_principales," electorales con otras variables?"))
                          
    )
    
    l <- purrr::map(1:nrow(basicos), function(z){
      actionButton(inputId = basicos[z,]$id, label = basicos[z,]$preguntas, class = "needed")
    })
    l
  })
  
  
  quest_choose <- reactive({
    last_btn <- input$last_click
    if (is.null(last_btn)) last_btn <- "general"
    last_btn
  })
  
  observe({
    req(questions_list())
    l <- questions_list()
    last_btn <- quest_choose()
    button_id <- which(c("general", "delito", "relacion") %in% last_btn)
    l[[button_id]] <- gsub("needed", "needed basic_active", l[[button_id]])
    l[[button_id]] <- HTML(paste0(paste(l[[button_id]], collapse = '')))
    output$basicos <- renderUI({
      l
    })
  })
  
  
  
  
  
  
  possible_viz <- reactive({
    p <- quest_choose()
    if (is.null(p)) return()
    if (p == "relacion") {
      v <- c("map", "scatter")
    } else {
      v <-  c("map", "bar", "treemap", "line")
    }
    v
  })
  
  
  actual_but <- reactiveValues(active = NULL)
  
  observe({
    if (is.null(possible_viz())) return()
    viz_rec <- possible_viz()
    if (is.null(input$viz_selection)) return()
    
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
  })
  
  
  output$viz_icons <- renderUI({
    req(possible_viz())
    suppressWarnings(
      buttonImageInput('viz_selection',
                       div(class="title-data-select", "Selecciona el tipo de gráfica"),
                       images = possible_viz(),
                       path = 'icons/',
                       active = actual_but$active)
    )
  })
  
  
  id_viz <- reactive({
    id_viz <- input$viz_selection
    if (is.null(id_viz)) id_viz <- "map"
    id_viz
  })
  
  pregunta <- reactive({
    p <- quest_choose()
    p == "relacion"
  })
  
  non_delito <- reactive({
    p <- quest_choose()
    p != "delito"
  })
  
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())
  
  
  
  core_data <- reactive({
    var_int <- input$variables_principales
    if (is.null(var_int)) return()
    id_data <- non_delito()
    
    if (var_int == "delitos") {
      c_d <- "spoa"
    } else {
      c_d <- "uriel"
    }
    if (!id_data) c_d <- paste0(c_d, "_", var_int) 
    
    c_d
  })
  
  
  rate_type <- reactive({
    tc <- input$tipo_censo
    id_data <- non_delito()
    var_int <- input$variables_principales
    if (is.null(tc)) return()
    if (is.null(id_data)) return()
    if (is.null(var_int)) return()
    
    if (id_data) {
      if (tc == "poblacion") {
        rt <- "tasa_denuncias_poblacion"
      } else {
        rt <- "tasa_denuncias_electoral"
      }
      if (var_int == "irregularidades") rt <- paste0(rt, "_uriel")
    } else {
      rt <- var_int
    }
    rt
  })
  
  
  territorio_select <- reactive({
    if(is.null(input$nivel_territorial)) return()
    input$nivel_territorial
  })
  
  data_select <- reactive({
    id_core <- core_data()
    if (is.null(id_core)) return()
    d_s <- datos_siscrimel[[id_core]]
    d_s
  })
  
  
  depto_opts <- reactive({
    req(data_select())
    viz <- id_viz()
    data_select <- data.frame(depto = unique(data_select()$depto))
    data_select <- dp %>% left_join(data_select)
    c_d <- setNames(data_select$id, data_select$label)
    if (viz == 'map') c_d <- c("Todos", c_d)
    c_d
  })
  
  data_filter <- reactive({
    req(data_select()) 
    df <- data_select()
    id_rate <- rate_type()
    id_viz <- id_viz()
    id_nivel <- input$nivel_territorial
    
    if (id_viz != "map") {
      id_nivel <- ifelse(id_nivel == "code_mun", "mcpio", "depto")
    }
    
    id_anios <- input$fechas
    if (is.null(id_anios)) return()
    first_anio <- as.numeric(id_anios[1])
    last_anio <- as.numeric(id_anios[2])
    
    df <- df %>% filter(anio %in% first_anio:last_anio)
    var_select <- c(id_nivel, "anio", id_rate)
    if (id_viz != "line") var_select <- setdiff(var_select, "anio")
    df <- df[var_select]
    df
    
  })
  
  
  output$aver <- renderPrint({
    data_filter()
  })
  
  tooltip_info <- reactive({
    #"{name}"
    "Tooltip info"
  })
  
  
  output$map_info <- renderLeaflet({
    if (is.null(id_viz())) return()
    if (is.null(input$nivel_territorial)) return()
    if (id_viz() != "map") return()
    id_depto <- input$departamentos
    if (is.null(id_depto)) id_depto <- "Todos"
    
    id_map_name <-  "col_departments"
    if (input$nivel_territorial == "code_mun") id_map_name <- "col_municipalities"
    num_zoom <- 5
    if (id_depto != "Todos") {
      id_map_name <- paste0("col_depto_", id_depto)
      num_zoom <- 7
    }
    delito <- !non_delito()
    dd <- data_filter()
    
    opts_l <- list(map_name = id_map_name,
                   map_tiles = "Esri.WorldStreetMap",
                   #map_color_scale = "Bins",
                   agg = "mean",
                   #tooltip = tooltip_info(),
                   legend_position = "bottomleft",
                   map_min_zoom = num_zoom
                   )
    if (delito) {
      dd <- dd %>% 
        group_by_all() %>% 
        summarise(total = n()) %>% 
        filter(total == max(total))

      opts_l <- modifyList(opts_l, list(
                                        color_by = names(dd)[2],
                                        map_color_scale = "Category",
                                        palette_colors_sequential = c("#0a4a83", "#0ebabe", "#eb5d0b", "#f4b72f", "#27a864")
      ))
      viz <- "lflt_choropleth_GcdCatNum"
    } else {
      viz <- "lflt_choropleth_GcdNum"
      opts_l <- modifyList(opts_l, list(palette_colors =  c("#73fbf3", "#60dbdf", "#4ebdcc", "#3d9fb9", "#2b82a7", "#1a6695", "#0a4a83")
      ))
    }
    
    opts_l <- modifyList(opts_l, list(data = dd))
    lf <- do.call(viz, opts_l)
    # if (input$casos_M != "lideres_asesinados") {
    #   lf <- lf %>% addLegendCustom(colors = c("#1549FF", "#1549FF", "#1549FF"), labels = c("<6", "6-15", ">15"), sizes = c(10, 20, 35))
    # }
    lf
  })
  
  
  output$general_viz <- renderHighchart({
    if (is.null(id_viz())) return()
    #if (is.null(input$nivel_territorial)) return()
    if (id_viz() == "map") return()
    
    viz <- paste0("hgch_", id_viz(),  "_CatNum")
    if (id_viz() == "line") viz <- gsub("CatNum", "CatYeaNum", viz)
    do.call(viz, list(data = data_filter(),
                      orientation = "hor",
                      agg = "mean",
                      ver_label = " ",
                      hor_title = " ", #reactivo porque depende de los datos
                      palette_colors = c("#0a4a83", "#0ebabe", "#eb5d0b", "#f4b72f", "#27a864")))
    
  })
  
  
  output$final_viz <- renderUI({
    if (is.null(id_viz())) return()
    if (id_viz() == "map") {
      v <- leafletOutput("map_info")
    } else {
      v <- highchartOutput("general_viz")
    }
    v
  })
  
  output$text_info <- renderUI({
    "Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
  })
  
}

shinyApp(ui, server)