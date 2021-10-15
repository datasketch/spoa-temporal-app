webshot::install_phantomjs()
library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shinydisconnect)
library(shinybusy)
library(dsmodules)
library(lfltmagic)
library(hgchmagic)
library(DT)

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
 width: 95%;
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

#ss-connect-dialog {
  color: #197e93 !important;
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

all_data <- readRDS("data/mecanismos_data.rds")
dptos_id <- read_csv("data/deptos_conf.csv")
dptos_id <- dptos_id[-32,]
ui <- panelsPage(
  disconnectMessage(
    text = "Tu sesión ha finalizado, por favor haz click aquí para recargar vista",
    refresh = "RECARGAR",
    background = "#ffffff",
    colour = "#da1c95",
    size = 14,
    overlayColour = "#2a2e30",
    overlayOpacity = 0.85,
    refreshColour = "#ffffff",
    css = "padding: 4.8em 3.5em !important; box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important;"
  ),
  tags$head(tags$head(
    includeScript("js/selectorClick.js")
  )),
  styles = style,
  panel(title = "Filtros",
        id = "azul",
        width = 300,
        body = div(
          div(style = "max-height: 300px !important; overflow: auto; margin-bottom: 5%;",
              uiOutput("preguntas")
          ),
          
          uiOutput("controls")
          #
        )
  ),
  panel(title = "Visualización",
        id = "naranja",
        header_right = uiOutput("descargas"),
        can_collapse = FALSE,
        color = "chardonnay",
        body = div(add_busy_spinner(spin = "fading-circle"),
                   #verbatimTextOutput("aver")
                   uiOutput("viz")##
        ),
        footer =  div(class = "panel-header",
                      uiOutput("viz_icons"), 
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src='ds_logo.png', align = "right", width = 150, height = 110)))
  ),
  panel(title = "Detalle", 
        width = 300,
        body =  uiOutput("info_final")#verbatimTextOutput("aver")#u
  )
)

server <- function(input, output, session) {
  
  # viz according question
  
  possible_viz <- reactive({
    v <- c("map", "bar", "pie",  "table")
    v
  })
  
  
  actual_but <- reactiveValues(active = NULL)
  
  observe({
    if (is.null(input$viz_selection)) return()
    viz_rec <- possible_viz()
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
  })
  
  # print viz
  output$viz_icons <- renderUI({
    req(possible_viz())
    suppressWarnings(
      buttonImageInput('viz_selection',
                       div(class="title-data-select", "Selecciona el tipo de gráfica"),
                       images = possible_viz(),
                       path = 'icons/',
                       active = actual_but$active
      )
    )
  })
  
  # questions
  output$preguntas <- renderUI({
    basicos <- data.frame(id = c("referendo_id", "plebiscito", "cabildo", "reconcocatoria", "consulta_pop"),
                          preguntas = c("Referendo", 
                                        "Plebiscito 2016",
                                        "Cabildo abierto",
                                        "Revocatoria de mandato",
                                        "Consulta popular"))
    
    l <- purrr::map(1:nrow(basicos), function(z){
      actionButton(inputId = basicos[z,]$id, label = basicos[z,]$preguntas, class = "needed")
    })
    l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
    l[[1]] <- HTML(paste0(paste(l[[1]], collapse = '')))
    
    l
  })
  
  # chosen question
  
  quest_choose <- reactive({
    last_btn <- input$last_click
    if (is.null(last_btn)) last_btn <- "referendo_id"
    last_btn
  })
  
  
  referendo_view <- reactive({
    if (is.null(quest_choose())) return()
    quest_choose() == "referendo_id"
  })
  
  referendo_opts  <- reactive({
    if (is.null(quest_choose())) return()
    if (!referendo_view()) return()
    setNames(c("referendo", "referendo_2003"), c("Referendos históricos", "Referendo de 2003"))
  })
  
  
  data_select <- reactive({
    if (is.null(quest_choose())) return()
    bd <- quest_choose()
    if (quest_choose() == "referendo_id") {
      if (is.null(input$referendo_ly)) return()
      bd <- input$referendo_ly
    }
    df <- all_data[[bd]]
    df
  })
  
  
  depto_opts <- reactive({
    req(data_select())
    c("TODOS", unique(data_select()$Departamento))
  })
  
  distinc_ref2003_view <- reactive({
    if (is.null(quest_choose())) return()
    q <- TRUE
    if (quest_choose() == "referendo_id") {
      req(input$referendo_ly)
      if (input$referendo_ly == "referendo_2003") q <- FALSE
    }
    q
  })
  
  mcpios_opts <- reactive({
    req(distinc_ref2003_view())
    if (distinc_ref2003_view()) {
      req(data_select())
      if (is.null(input$departamentos)) return()
      if (input$departamentos == "TODOS") {
        "TODOS"
      } else {
        df <- data_select()$Municipio[grep(input$departamentos, data_select()$Departamento)]
        c("TODOS", unique(df))
      }
    } else {
      return()
    }
  })
  
  referendoHist_view <- reactive({
    if (is.null(quest_choose())) return()
    q <- FALSE
    if (quest_choose() == "referendo_id") {
      q <- TRUE
      req(input$referendo_ly)
      if (input$referendo_ly == "referendo_2003") q <- FALSE
    }
    q
  })
  
  cabildo_view <- reactive({
    if (is.null(quest_choose())) return()
    quest_choose() == "cabildo"
  })
  
  recon_view <- reactive({
    if (is.null(quest_choose())) return()
    quest_choose() == "reconcocatoria"
  })
  
  consulta_view <- reactive({
    if (is.null(quest_choose())) return()
    quest_choose() == "consulta_pop"
  })
  
  options_view <- reactive({
    if (is.null(quest_choose())) return()
    q <- TRUE
    if (quest_choose() == "cabildo") q <- FALSE
    if (quest_choose() == "referendo_id") {
      req(input$referendo_ly)
      if (input$referendo_ly == "referendo") q <- FALSE
    }
    q
  })
  
  tipo_voto_ops <- reactive({
    if (is.null(quest_choose())) return()
    if (!options_view()) return()
    req(data_select())
    c("TODOS", unique(data_select()$`Tipo de votación`))
  })
  
  estado_ops <- reactive({
    if (is.null(quest_choose())) return()
    if (!recon_view()) return()
    req(data_select())
    c("TODOS", unique(data_select()$Estado))
  })
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())
  
  
  
  data_filter <- reactive({
    
    req(quest_choose())
    req(data_select())
    
    df <- data_select()
    
    req(input$departamentos)
    if (input$departamentos != "TODOS") {
      df <- df %>% dplyr::filter(Departamento %in% input$departamentos)
    }
    
    if (quest_choose() == "referendo_id") {
      if (is.null(input$referendo_ly)) return()
      if (input$referendo_ly == "referendo") {
        if (is.null(input$municipios)) return()
        if (input$municipios != "TODOS") df <- df %>% dplyr::filter(Municipio %in% input$municipios) 
        if (is.null(input$anio)) return()
        anios <- input$anio
        if (length(input$anio) > 1) anios <- input$anio[1]:input$anio[2]
        df <- df[grep(paste0(anios, collapse = "|"), df$`Fecha De Realizacion Del Mecanismo`),]
      } else {
        if (is.null(input$tipo_voto)) return()
        if (input$tipo_voto != "TODOS") {
          df <- df %>% dplyr::filter(`Tipo de votación` %in% input$tipo_voto)
        }
      } 
    } else {
      if (input$municipios != "TODOS") df <- df %>% dplyr::filter(Municipio %in% input$municipios)
      if (options_view()) {
        if (input$tipo_voto != "TODOS") {
          df <- df %>% dplyr::filter(`Tipo de votación` %in% input$tipo_voto)
        }
      }
    }
    
    if (quest_choose() %in% "cabildo") {
      anios <- input$anio_cabildo
      if (length(input$anio_cabildo) > 1) anios <- input$anio_cabildo[1]:input$anio_cabildo[2]
      df <- df[grep(paste0(anios, collapse = "|"), df$`Fecha De Realizacion Del Mecanismo`),]
    }
    
    if (quest_choose() %in% "reconcocatoria") {
      anios <- input$anio_recon
      if (length(input$anio_recon) > 1) anios <- input$anio_recon[1]:input$anio_recon[2]
      df <- df[grep(paste0(anios, collapse = "|"), df$Fecha),]
      if (is.null(input$estado)) return()
      if (input$estado != "TODOS") df <- df %>% dplyr::filter(Estado %in% input$estado)
    }
    
    if (quest_choose() %in% "consulta_pop") {
      anios <- input$anio_cons
      if (length(input$anio_cons) > 1) anios <- input$anio_cons[1]:input$anio_cons[2]
      df <- df[grep(paste0(anios, collapse = "|"), df$Fecha),]
    }
    
    
    df
    
  })
  
  
  data_viz <- reactive({
    req(data_filter())
    req(quest_choose())
    if (is.null(actual_but$active)) return()
    df <- data_filter() 
    
    if (quest_choose() %in% c("referendo_id")) {
      req(input$referendo_ly)
      if (input$referendo_ly == "referendo") {
        dv <- df %>% 
          dplyr::group_by(Departamento) %>%
          dplyr::summarise(Total = n())
        if (input$departamentos != "TODOS") {
          dv <- df %>% 
            dplyr::group_by(Municipio) %>%
            dplyr::summarise(Total = n()) 
        }
      } else {
        dv <- df %>% 
          dplyr::group_by(Departamento) %>%
          dplyr::summarise(Total = sum(Total, na.rm = T))
      }
    } else if (quest_choose() == "cabildo") {
      dv <- df %>% 
        dplyr::group_by(Departamento) %>%
        dplyr::summarise(Total = n())
      if (input$departamentos != "TODOS") {
        dv <- df %>% 
          dplyr::group_by(Municipio) %>%
          dplyr::summarise(Total = n())
      }
    } else {
      dv <- df %>% 
        dplyr::group_by(Departamento) %>%
        dplyr::summarise(Total = sum(Total, na.rm = TRUE))
      if (input$departamentos != "TODOS") {
        dv <- df %>% 
          dplyr::group_by(Municipio) %>%
          dplyr::summarise(Total = sum(Total, na.rm = T)) 
      }
    }
    
    if (actual_but$active %in% c("bar", "pie"))
      if (!is.null(input$tipo_voto)) {
        if (input$tipo_voto == "TODOS") {
          dv <- df %>% group_by(`Tipo de votación`) %>% summarise(Total = sum(Total, na.rm = T))
        }
      }
    
    dv
    
    
  })
  
  viz_type <- reactive({
    if (is.null(actual_but$active)) return()
    viz <- actual_but$active
    vt <- paste0("hgch_", viz, "_CatNum")
    if (actual_but$active == "map") vt <- "lflt_choropleth_GnmNum"
    if (actual_but$active == "table") vt <- "table"
    vt
  })
  
  
  viz_end <- reactive({
    req(viz_type())
    req(data_viz())
    if (is.null(data_viz())) return()
    if (is.null(actual_but$active)) return()
    if (actual_but$active == "table") return()
    df <- data_viz()
    
    if (actual_but$active == "map") {
      
      geo_select <- "col_departments"
      input_depto <- iconv(tolower(input$departamentos), to = "ASCII//TRANSLIT")
      dptos_id$label <- iconv(tolower(dptos_id$label), to = "ASCII//TRANSLIT")
      
      if (input_depto != "todos") {
        geo_select <- paste0("col_depto_", dptos_id$id[grep(input_depto,dptos_id$label)])
      }
      
      print(geo_select)
      print(df)
      do.call(viz_type(), list(data = df, 
                               map_name = geo_select,
                               map_tiles = "Esri.WorldStreetMap",
                               legend_position = "bottomleft",
                               palette_colors =  c("#73fbf3", "#60dbdf", "#4ebdcc", "#3d9fb9", "#2b82a7", "#1a6695", "#0a4a83"),
                               map_min_zoom = 5))
    } else {
      if (input$departamentos != "todos") {
        } 
      #func_chart <- "function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}"
      func_chart <- "function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}"
      
      opts_l <- list(data = df,
                     orientation = "hor",
                     label_wrap = 100,
                     hor_title = " ",
                     ver_title = " ",
                     sort = "desc",
                     palette_colors = c("#0a4a83", "#0ebabe", "#eb5d0b", "#f4b72f", "#27a864"),
                     clickFunction = JS(func_chart)
      )
      
      if (options_view()) {
        if (input$tipo_voto == "TODOS") {
          opts_l <- modifyList(opts_l, list(
            color_by = names(df)[1],
            map_color_scale = "Category",
            palette_colors_sequential = c("#0a4a83", "#0ebabe", "#eb5d0b", "#f4b72f", "#27a864")
          ))
        }
      }
      
      do.call(viz_type(), opts_l)  
    }
    
  })
  
  
  output$table_view <- renderDataTable({
    req(data_filter())
    df <- data_filter()
    DT::datatable(df,
                  rownames = F,
                  options = list(
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    lengthChange = F,
                    pageLength = 5,
                    scrollX = T,
                    scrollY = T,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#0a4a83', 'color': '#fff'});",
                      "}")
                  )) %>% 
      formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
  })
  
  output$basic_viz <- renderHighchart({
    if (is.null(actual_but$active)) return()
    if (actual_but$active == "table") return()
    if (actual_but$active == "map") return()
    viz_end()
  })
  
  
  output$map_viz <- renderLeaflet({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "map") return()
    viz_end()
  })
  
  output$viz <- renderUI({
    if (is.null(data_viz())) return("Sin registros para los filtros en selección")
    if (is.null(actual_but$active)) return()
    if (actual_but$active == "map") {
      leafletOutput("map_viz")
    } else if (actual_but$active == "table") {
      dataTableOutput("table_view", width = 600)
    } else {
      highchartOutput("basic_viz", height = 380)
    }
  })
  
  
  
  
  id_click_viz <- reactiveValues(value = NULL)
  
  observeEvent(input$map_viz_shape_click, {
    id_click_viz$value <- input$map_viz_shape_click$id
  })
  
  
  #
  observeEvent(input$hcClicked, {
    id_click_viz$value <- input$hcClicked$id
  })
  
  
  click_table <- reactive({
    
    id_sel <- input$table_view_rows_selected
    if (is.null(id_sel)) return()
    df <- data_filter()[id_sel,]
    cs <- df$Departamento
    if (options_view()) {
      if (input$tipo_voto != "TODOS") {
        cs <- df$`Tipo de votación` 
      }
    } 
    cs
  })
  
  observeEvent(input$table_view_rows_selected,{
    id_click_viz$value <- click_table()
  })
  
  observeEvent(input$last_click,{
    id_click_viz$value <- NULL
  })
  
  
  output$tabla_refe <- renderUI({
    downloadTableUI("dropdown_refepais", dropdownLabel = "Descarga", formats = c("csv", "xlsx", "json"), display = "dropdown")
  })
  
  data_refepais <- reactive({
    all_data$referendo_paises
  })
  
  downloadTableServer("dropdown_refepais", element = data_refepais(), formats = c("csv", "xlsx", "json"))
  
  data_info <- reactive({
    if (is.null(data_filter())) return()
    if (is.null(id_click_viz$value)) return()
    df <- data_filter()
    #"referendo_id", "plebiscito", "cabildo", "reconcocatoria", "consulta_pop"
    
    # if (options_view()) {
    #   if (input$tipo_voto != "TODOS") {
    #     df <- df %>% filter(`Tipo de votación` %in% id_click_viz$value)
    #   }
    # }
    
    if (quest_choose() == "referendo_id") {
      req(input$referendo_ly)
      if (input$referendo_ly == "referendo") {
        tx <- 
          map(id_click_viz$value, function(d){
            dp <- df %>% filter(Departamento %in% d)
            HTML("<h2>", id_click_viz$value, "</h2>",
                 map(1:nrow(dp), function(i){
                   HTML(paste0("<br/><b>Fecha de realización del mecanismo: </b>", dp$`Fecha De Realizacion Del Mecanismo`[i],
                               "<br/><b>Objeto: </b>", dp$Objeto[i],
                               "<br/><b>Votación obtenida: </b>", dp$`Votacion Obtenida`[i]))
                 }) %>% paste0(collapse = "<br/>")
            )
          })
      } else {
        tx <- div(HTML("<p style='margin-top:15px;margin-bottom: 21px;'>Descarga los resultados por países <p/>"),
                  uiOutput("tabla_refe"))
      }
    }
    
    if (quest_choose() == "plebiscito") {
        tx <- map(id_click_viz$value, function(d){
          dp <- df %>% filter(Departamento %in% d) %>% 
            group_by(Municipio, `Tipo de votación`) %>% 
            summarise(Total = sum(Total, na.rm =  TRUE)) 
          dp$votos_info <- paste0(dp$`Tipo de votación`, ": ", dp$Total)
          dp <- dp  %>% group_by(Municipio) %>% 
            summarise(votos_info = paste0(votos_info, collapse = "</br>"))
          HTML("<h2>", id_click_viz$value, "</h2>",
               map(1:nrow(dp), function(i){
                 HTML(paste0("<br/><b>Municipio: </b>", dp$Municipio[i],
                             "<br/><b>Votos: </b><br/>", dp$votos_info[i]))
               }) %>% paste0(collapse = "<br/>")
          )
        })
    }
    
    if (quest_choose() == "cabildo") {
      tx <- 
        map(id_click_viz$value, function(d){
          dp <- df %>% filter(Departamento %in% d)
          HTML("<h2>", id_click_viz$value, "</h2>",
               map(1:nrow(dp), function(i){
                 HTML(paste0("<br/><b>Fecha de realización del mecanismo: </b>", dp$`Fecha De Realizacion Del Mecanismo`[i],
                             "<br/><b>Objeto: </b>", dp$Objeto[i],
                             "<br/><b>Votación obtenida: </b>", dp$`Votacion Obtenida`[i]))
               }) %>% paste0(collapse = "<br/>")
          )
        })
    }
    #Listado de cabildos realizados con “FECHA, DEPARTAMENTO, MUNICIPIO, POTENCIAL, UMBRAL, TOTAL VOTACIÓN, ABSTENCIÓN (%), SI, NO, VOTO EN BLANCO, NULOS, NO MARCADOS”
    if (quest_choose() == "reconcocatoria") {
      tx <- map(id_click_viz$value, function(d){
        dp <- df %>% filter(Departamento %in% d) %>% 
          group_by(Municipio, `Tipo de votación`) %>% 
          summarise(Total = sum(Total, na.rm =  TRUE), Potencial = Potencial, Umbral= Umbral, `Abstención` = `Abstención`) 
        dp$votos_info <- paste0(dp$`Tipo de votación`, ": ", dp$Total)
        dp <- dp  %>% group_by(Municipio, Potencial, Umbral, `Abstención`) %>% 
          summarise(votos_info = paste0(votos_info, collapse = "</br>"))
        HTML("<h2>", id_click_viz$value, "</h2>",
             map(1:nrow(dp), function(i){
               HTML(paste0("<br/><h3>", dp$Municipio[i], "</h3><br/>Potencial: ", dp$Potencial[i],
                           "<br/>Umbral: ",dp$Umbral[i],
                           "<br/>Abstención: ",dp$`Abstención`[i],
                           "<br/><br/><b>Votos: </b><br/>", dp$votos_info[i]))
             }) %>% paste0(collapse = "<br/>")
        )
      })
    } 
    
    if (quest_choose() == "consulta_pop") {
      tx <- map(id_click_viz$value, function(d){
        dp <- df %>% filter(Departamento %in% d) %>% 
          group_by(Descripción, Municipio, Fecha, `Tipo de votación`) %>% 
          summarise(Total = sum(Total, na.rm =  TRUE), `Abstención` = `Abstención`) 
        dp$votos_info <- paste0(dp$`Tipo de votación`, ": ", dp$Total)
        dp <- dp  %>% group_by(Descripción, Fecha, Municipio, `Abstención`) %>% 
          summarise(votos_info = paste0(votos_info, collapse = "</br>"))
        HTML("<h2>", id_click_viz$value, "</h2>",
             map(1:nrow(dp), function(i){
               HTML(paste0("<br/><h3>", dp$Municipio[i], "</h3>
                            <br/>Descripción", dp$Descripción[i],
                           "<br/>Fecha: ",dp$Fecha[i],
                           "<br/>Abstención: ",dp$`Abstención`[i],
                           "<br/><br/><b>Votos: </b><br/>", dp$votos_info[i]))
             }) %>% paste0(collapse = "<br/>")
        )
      })
    }
    
    
    
    
    tx
  })
  
  
  output$text_click <- renderUI({
    p <- quest_choose()
    if (is.null(p)) return()
    data_info()
  })
  
  output$aver <- renderPrint({
    data_info()
  })
  
  output$textos_informativos <- renderUI({
    if (!is.null(id_click_viz$value)) return()
    if (!is.null(data_info())) return()
    
    tx <- HTML(
      '<div class = "indicacion"><img src="click/click.svg" style="width: 50px; display:block;margin-left: 40%;"/>
   <br/><p><b>1.</b>Selecciona el tipo de datos que quieres explorar, puedes elegir: Referendo, Plebiscito 2016,
        Cabildo abierto, Revocatoria de mandato o Consulta popular.</p><br/>
   <p><b>2.</b>Selecciona el tipo de gráfico que quieres ver.</p><br/>
   <p><b>3.</b>Da clic en cada barra, color o elemento de la gráfica para obtener más información.</p><br/>
    </div>')
    
    
    if (quest_choose() == "referendo_id") {
      req(input$referendo_ly)
      if (input$referendo_ly != "referendo") {
        tx <- div(HTML("<p style='margin-top:3px;margin-bottom: 21px;'>Descarga los resultados por países <p/>"),
                  uiOutput("tabla_refe"))
      }
    }
    
    tx
    
  })
  

  output$info_final <- renderUI({
    if (is.null(id_click_viz$value)) {
      uiOutput("textos_informativos")
    } else {
      uiOutput("text_click")
    }
  })
  
  
  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      downloadImageUI("download_viz", dropdownLabel = "Descarga", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown")
    } else {
      downloadTableUI("dropdown_table", dropdownLabel = "Descarga", formats = c("csv", "xlsx", "json"), display = "dropdown")
    }
  })
  
  downloadTableServer("dropdown_table", element = data_filter(), formats = c("csv", "xlsx", "json"))
  downloadImageServer("download_viz", element = viz_end(), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  
}

shinyApp(ui, server)