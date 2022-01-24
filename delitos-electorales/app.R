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

.control-label {
  margin-top: 10px;
}
"

datos_siscrimel <- readRDS("data/all_spoa_data.rds")
mcpio_centroids <- read_rds("data/mcpio_centroids.rds")
depto_centroids <- read_rds("data/deptos_centroids.rds")
dp <- read_csv("data/deptos_conf.csv")
add_labs <- read_csv("data/label_other_data.csv")

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.8, position ="bottomleft", title = "Líderes asesinados"){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px;border-radius: 50%;")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, position = position, title = title))
}

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
        header_right = uiOutput("descargas"),
        can_collapse = FALSE,
        color = "chardonnay", #div(add_busy_spinner(spin = "fading-circle"),uiOutput("final_viz"))
        body = uiOutput("final_viz"), # verbatimTextOutput("aver"),# 
        footer =  div(class = "panel-header",
                      uiOutput("viz_icons"), 
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src='ds_logo.png', align = "right", width = 150, height = 110)))
  )#,
  # panel(title = "Detalle", 
  #       width = 300,
  #       body =  uiOutput("click_text")#verbatimTextOutput("aver")#u
  # )
)

server <- function(input, output, session) {
  
  output$base <- renderUI({
    var_base <- c("Delitos electorales", "Irregularidades electorales")
    radioButtons("variables_principales",
                 label = HTML("<div class='style_section'>Variables</div>"), 
                 setNames(c("delitos", "irregularidades"), var_base))
  })
  
  
  questions_list <- reactive({
    
    if (is.null(input$variables_principales)) return()
    
    basicos <- data.frame(id = c( "delito", "general", "relacion"),
                          preguntas = c(paste0("¿Cuáles son los tipos de  ", input$variables_principales," electorales más comunes?"),
                                        paste0("¿Cuáles son las zonas más afectadas por ", input$variables_principales," electorales?"),
                                        paste0("¿Cómo se relacionan   ", input$variables_principales," electorales con otras variables?"))
                          
    )
    
    l <- purrr::map(1:nrow(basicos), function(z){
      actionButton(inputId = basicos[z,]$id, label = basicos[z,]$preguntas, class = "needed")
    })
    l
  })
  
  
  quest_choose <- reactive({
    last_btn <- input$last_click
    if (is.null(last_btn)) last_btn <- "delito"
    last_btn
  })
  
  observe({
    if(is.null(questions_list())) return()
    l <- questions_list()
    last_btn <- quest_choose()
    button_id <- which(c("delito","general", "relacion") %in% last_btn)
    l[[button_id]] <- gsub("needed", "needed basic_active", l[[button_id]])
    l[[button_id]] <- HTML(paste0(paste(l[[button_id]], collapse = '')))
    output$basicos <- renderUI({
      l
    })
  })
  
  
  possible_viz <- reactive({
    p <- quest_choose()
    if (is.null(p)) return()
    v <-  c("map", "bar", "line", "table")
    if (p == "relacion") {
      v <- c("map" , "table") #"scatter",
    }
    if (p == "delito") {
      v <- c("bar", "map", "line", "table")
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
    id_viz <- actual_but$active
    if (is.null(id_viz)) id_viz <- "map"
    id_viz
  })
  
  
  pregunta <- reactive({
    req(quest_choose())
    if (is.null(input$variables_principales)) return()
    q <- quest_choose()
    p <- q == "relacion"
    p
  })
  
  cultivo_view <- reactive({
    if (is.null(input$variables_adicionales)) return(FALSE)
    if (is.null(quest_choose())) return()
    cv <- TRUE
    if (quest_choose() != "relacion") cv <- FALSE
    if (input$variables_adicionales != "cultivos_ilicitos") cv <- FALSE
    cv
  })
  
  delitos_show <- reactive({
    if (is.null(quest_choose())) return()
    p <- quest_choose()
    p == "delito"
  })
  
  delitos_opts <- reactive({
    req(data_select())
    req(input$variables_principales)
    vp <- input$variables_principales
    if (vp == "delitos") vp <- "delito"
    c("Todos",unique(data_select()[[vp]]))
  })
  
  non_delito <- reactive({
    if (is.null(quest_choose())) return()
    p <- quest_choose()
    p != "delito"
  })
  
  territorio_select <- reactive({
    if(is.null(input$nivel_territorial)) return()
    input$nivel_territorial
  })
  
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
    
    id_data <- non_delito()
    var_int <- input$variables_principales
    
    if (is.null(id_data)) return()
    if (is.null(var_int)) return()
    
    rt <- "tasa_denuncias_electoral"
    
    if (id_data) {
      if (var_int == "irregularidades") rt <- paste0(rt, "_uriel")
    } else {
      rt <- "total"
    }
    rt
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
    #if (viz == 'map') 
    c_d <- c("Todos", c_d)
    c_d
  })
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())
  
  
  
  data_filter <- reactive({
    req(data_select())
    req(rate_type())
    df <- data_select()
    
    if (is.null(input$departamentos)) return()
    if (input$departamentos != "Todos") {
      depto_o <- dp$depto[dp$id == input$departamentos]
      df$depto <- toupper(df$depto)
      df <- df %>% filter(depto %in% depto_o)
    }
    
    if (is.null(input$fechas)) return()
    anios <- input$fechas
    if (length(input$fechas) > 1) anios <- input$fechas[1]:input$fechas[2]
    df <- df[grep(paste0(anios, collapse = "|"), df$anio),]
    
    
    if (quest_choose() == "relacion") {
      if (actual_but$active == "table") {
        if (is.null(input$variables_adicionales)) return()
        df <- datos_siscrimel[[input$variables_adicionales]]
      }
    }
    
    if (quest_choose() == "delito") {
      if (actual_but$active != "bar") {
        if (is.null(input$tipos_delito)) return()
        if (input$tipos_delito != "Todos") {
          if (is.null(input$variables_principales)) return()
          if (input$variables_principales %in% "delitos") {
            df <- df %>% dplyr::filter(delito %in% input$tipos_delito)
          } else if (input$variables_principales %in% "irregularidades"){
            df <- df %>% dplyr::filter(irregularidades %in% input$tipos_delito)
          } else {
            return()
          }
        } else {
          df <- df
        }
      }
    }
    #print( df)
    df
    
  })
  
  data_viz <- reactive({
    req(data_filter())
    req(rate_type())
    df <- data_filter()
    #print(quest_choose())
    if (is.null(input$nivel_territorial)) return()
    
    var_sel <-  c("mcpio", "depto", "anio", rate_type()) 
    if (actual_but$active == "map") {
      if (quest_choose() != "delito") {
        var_sel <- c(var_sel, "code_mun_dane", "code_depto_dane")
      }
    }
    if (actual_but$active != 'line') var_sel <- setdiff(var_sel, "anio")
    
    if (is.null(input$departamentos)) return()
    if (input$departamentos != "Todos") {
      var_sel <- setdiff(var_sel, "depto")
    }
    
    if (input$nivel_territorial == "code_depto_dane") {
      var_sel <- setdiff(var_sel, "mcpio")
    } 
    
    if (actual_but$active == "map") {
      if (quest_choose() == "delito") {
        var_sel <- c(var_sel, "delito")
      }
    }
    
    if (actual_but$active != "map") {
      if (quest_choose() == "delito") {
        var_p <- input$variables_principales
        if (var_p == "delitos") var_p <- "delito"
        var_sel <- c(var_p, "total")
      }
    }
    
    if (actual_but$active == "line") {
      if (quest_choose() == "delito") {
        var_p <- input$variables_principales
        if (var_p == "delitos") var_p <- "delito"
        var_sel <- c(var_p, "anio","total")
      }
    }
    
    df <- df[var_sel]
    
    
    if (actual_but$active == "map") {
      if (quest_choose() == "delito") {
        df <- df %>% 
          dplyr::group_by(depto) %>%
          dplyr::summarise(total = sum(total),
                           delitos = paste0(unique(delito), collapse = "<br/>")) 
      }
    }
    
    df
  })
  
  
  tooltip_info <- reactive({
    req(quest_choose())
    if (actual_but$active != "map") return()
    if (quest_choose() != "delito")  {
      
      if (is.null(input$nivel_territorial)) return()
      if (input$nivel_territorial == "code_mun_dane") {
        tpi <- HTML(paste0("Code_mun_dane: {code_mun_dane} <br/> Nombre: {mcpio} <br/>", rate_type(),": {",rate_type(),"}"))
      } else {
        tpi <- HTML(paste0("Code_depto_dane: {code_depto_dane}
                           <br/> Nombre: {depto} <br/>",
                           rate_type(),": {",rate_type(),"} <br/>"))
      }  
    } else {
      if (input$nivel_territorial == "code_mun_dane") {
        tpi <- HTML("Nombre: {mcpio}  <br/> Total de delitos: {total}")
      } else {
        tpi <- "Nombre: {depto} <br/> Total de delitos: {total} <br/> {delitos}"
      } 
      
    }
    tpi
  })
  
  gen_viz <- reactive({
    if (nrow(data_viz()) == 0) return()
    if (is.null(input$departamentos)) return()
    if (actual_but$active == "map") {
      num_zoom <- 5
      
      id_map_name <-  "col_departments"
      
      if (input$nivel_territorial == "code_mun_dane") {
        id_map_name <- "col_municipalities"
      }
      
      if (input$departamentos != "Todos") {
        id_depto <- input$departamentos
        id_map_name <- paste0("col_depto_", id_depto)
        num_zoom <- 7
      }
      
      agg_opt <- "mean"
      if ("total" %in% names(data_viz())) agg_opt <- "sum"
      #print(data_viz())
      lf <- do.call("lflt_choropleth_GnmNum", 
                    list(
                      data = data_viz(),
                      map_name = id_map_name,
                      palette_colors =  c("#f9c74f","#ee9b00", "#ca6702", "#bb3e03", "#ae2012", "#9b2226"),
                      map_tiles = "Esri.WorldStreetMap",
                      agg = agg_opt,
                      tooltip = tooltip_info(),
                      legend_position = "bottomleft",
                      map_min_zoom = num_zoom
                    ))
    } else {
      if (is.null(input$departamentos)) return()
      if (actual_but$active == "map") return()
      viz_type <- "hgch_bar_CatNum"
      order_year <- NULL
      if (actual_but$active == "line") {
        viz_type <- "hgch_line_CatYeaNum"
        order_year <- sort(unique(data_viz()[[2]]))
      }
      agg_opt <- "mean"
      if ("total" %in% names(data_viz())) agg_opt <- "sum"
      
      do.call(viz_type, list(data = data_viz(),
                             orientation = "hor",
                             agg = agg_opt,
                             label_wrap = 100,
                             ver_title = " ",
                             sort = "desc",
                             order = order_year,
                             hor_title = " ", #reactivo porque depende de los datos
                             palette_colors = c("#0a4a83", "#0ebabe", "#eb5d0b", "#f4b72f", "#27a864")))
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
                    pageLength = 15,
                    scrollX = T,
                    scrollY = T,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#0a4a83', 'color': '#fff'});",
                      "}")
                  )) %>% 
      DT::formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
  })
  
  output$map_lflt <- renderLeaflet({
    if (is.null(id_viz())) return()
    if (is.null(input$nivel_territorial)) return()
    if (id_viz() != "map") return()
    gen_viz()
  })
  
  output$other_hgch <- renderHighchart({
    if (is.null(id_viz())) return()
    if (id_viz() == "map") return()
    if (id_viz() == "table") return()
    gen_viz()
  })
  
  output$final_viz <- renderUI({
    if (is.null(id_viz())) return()
    if (id_viz() == "map") {
      v <- leafletOutput("map_lflt")
    } else if (id_viz() == "table") {
      v <- dataTableOutput("table_view", width = 950)
    } else {
      v <- highchartOutput("other_hgch")
    }
    v
  })
  
  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      downloadImageUI("download_viz", dropdownLabel = "Descarga", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown")
    } else {
      downloadTableUI("dropdown_table", dropdownLabel = "Descarga", formats = c("csv", "xlsx", "json"), display = "dropdown")
    }
  })
  
  downloadTableServer("dropdown_table", element = reactive(data_filter()), formats = c("csv", "xlsx", "json"))
  downloadImageServer("download_viz", element = reactive(gen_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  
  delitos_seguridad <- reactive({
    if (quest_choose() != "relacion") return(FALSE)
    if (is.null(input$variables_adicionales)) return()
    input$variables_adicionales == "base_seguridad"
  })
  
  data_relacion <- reactive({
    if (quest_choose() != "relacion") return()
    if (is.null(input$variables_adicionales)) return()
    
    df <- datos_siscrimel[[input$variables_adicionales]]
    
    if (input$variables_adicionales == "base_seguridad") {
      if (is.null(input$tipo_delito)) return()
      df <- df %>% filter(tipo %in% input$tipo_delito)
    }
    
    if (is.null(input$fechas)) return()
    anios <- input$fechas
    if (length(input$fechas) > 1) anios <- input$fechas[1]:input$fechas[2]
    df <- df[grep(paste0(anios, collapse = "|"), df$anio),]
    df
  })
  
  acciones_sub <- reactive({
    q <- FALSE
    if (quest_choose() != "relacion") return(FALSE)
    if (is.null(input$variables_adicionales)) return(FALSE)
    if (input$variables_adicionales != "base_seguridad")  return(FALSE)
    if (is.null(input$tipo_delito)) return(FALSE)
    if (input$tipo_delito == "Acciones subversivas") q <- TRUE
    q
  })
  
  condutas_opts <- reactive({
    req(data_relacion())
    if (is.null(input$tipo_delito)) return()
    if (input$variables_adicionales != "base_seguridad") return()
    if (input$tipo_delito == "Acciones subversivas") {
      c("TODAS", unique(data_relacion()$conducta))
    } else {
      return()
    }
  })
  
  
  data_relacion_filter <- reactive({
    if (quest_choose() != "relacion") return()
    if (is.null(input$variables_adicionales)) return()
    req(data_relacion())
    df <- data_relacion()
    if (input$variables_adicionales == "base_seguridad") {
      if (is.null(input$conduta_delito)) return()
      if (input$conduta_delito != "TODAS") {
        df <- data_relacion() %>% filter(conducta %in% input$conduta_delito)
      }
    }
    if (input$variables_adicionales ==  "cultivos_ilicitos") {
      var_c <- input$tipo_cultivo
      if (is.null(var_c)) return()
      df <- df[,-grep(var_c, names(df))]
      names(df) <- gsub("amapola|coca", "total", names(df))
    }
    
    if (is.null(input$nivel_territorial)) return()
    
    if (input$nivel_territorial == "code_depto_dane") {
      df <- df %>% select(depto, total)
      if (input$variables_adicionales %in% c("base_seguridad", "delitos_ambiente", "hechos_victimizantes", "minas")) {
        df <- df %>% group_by(name = depto) %>% summarise(total = sum(total, na.rm = TRUE))
      } else {
        df <- df %>% group_by(name = depto) %>% summarise(total = mean(total, na.rm = TRUE))
      }
      df <- df %>% drop_na(total)
      df$name <- stringi::stri_trans_general(str = df$name, id = "Latin-ASCII")
      df$name[df$name == "NARINO"] <- "NARIÑO"
      df$name[df$name == "ARCHIPIELAGO DE SAN ANDRES"] <- "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA"
      df$name[df$name == "BOGOTA D.C."] <- "BOGOTA, D.C."
      df <- df %>% left_join(depto_centroids)
    } else {
      df <-  df %>% select(mcpio, depto, total)
      if (input$variables_adicionales %in% c("base_seguridad", "delitos_ambiente", "hechos_victimizantes", "minas")) {
        df <- df %>% group_by(name = mcpio, depto) %>% summarise(total = sum(total, na.rm = TRUE))
      } else {
        df <- df %>% group_by(name = mcpio, depto) %>% summarise(total = mean(total, na.rm = TRUE))
      }
      df <- df %>% drop_na(total)
      df$name <- stringi::stri_trans_general(str = df$name, id = "Latin-ASCII")
      df$depto <- stringi::stri_trans_general(str = df$depto, id = "Latin-ASCII")
      df$depto[df$depto == "NARINO"] <- "NARIÑO"
      df$depto[df$depto == "ARCHIPIELAGO DE SAN ANDRES"] <- "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA"
      df$depto[df$depto == "BOGOTA D.C."] <- "BOGOTA, D.C."
      df <- df %>% left_join(mcpio_centroids)
    }
    
    df$lat <- as.numeric(df$lat)
    df$lon <- as.numeric(df$lon)
    df$radius <- scales::rescale(df$total, c(3, 11))
    df$labels <- paste0(add_labs$Label[add_labs$Id == input$variables_adicionales], ": <br/>",
                        format(df$total, digits = 2, big.mark = ",",  scientific = FALSE))  %>% lapply(htmltools::HTML)
    df
    #df %>% drop_na(lat, lon)
    
  })
  
  
  
  
  
  observe({
    if (is.null(data_relacion_filter())) return()
    df <- data_relacion_filter()
    #print(df)
    leafletProxy("map_lflt", data = df) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~radius,
        label = ~labels,
        color = "#1549FF",
        fillOpacity = 1,
        labelOptions = labelOptions(style = list(`font-weight` = "normal",
                                                 padding = "3px 8px"),
                                    textsize = "13px", direction = "auto"))
  })
  
  
  output$click_text <- renderUI({
    tx <- HTML(
      '<div class = "indicacion"><img src="click/click.svg" style="width: 50px; display:block;margin-left: 40%;"/>
   <br/>
   <p>Da clic en cada barra, color o elemento de la gráfica para obtener más información.</p><br/>
    </div>')
    tx
  })
  
  
  
}

shinyApp(ui, server)