# LDJ Prism · Shiny + ggplot2 MVP
# Datos sintéticos de demostración; esta aplicación no almacena archivos cargados.
library(shiny)
library(ggplot2)
library(svglite)
library(colourpicker)
library(rhandsontable)
library(readxl)
source(if(file.exists("R/analysis.R")) "R/analysis.R" else "apps/ldj-prism/R/analysis.R", local=TRUE)
private_mode <- identical(Sys.getenv("LDJ_PRISM_MODE"), "private")

demo_data <- local({
  d <- expand.grid(
    Concentracion_uM = c(0.01, 0.03, 0.1, 0.3, 1, 3, 10),
    Replica = 1:3,
    Compuesto = c("Ejemplo A", "Ejemplo B"),
    KEEP.OUT.ATTRS = FALSE
  )
  midpoint <- ifelse(d$Compuesto == "Ejemplo A", 1.5, 4.5)
  d$Viabilidad_pct <- pmax(0, pmin(110,
    100 / (1 + (d$Concentracion_uM / midpoint)^1.2) +
      4 * sin(seq_len(nrow(d)) * 1.4)
  ))
  d$control_type <- "treatment"
  d$observation_id <- paste0("t",seq_len(nrow(d)))
  d$experiment_id <- "SYNTHETIC-1"
  d$control_source <- "Synthetic generator"
  d$inference_eligible <- TRUE
  controls <- expand.grid(Replica=1:3,
    control_type=c("untreated","vehicle","infected_untreated"),
    Compuesto=c("Ejemplo A","Ejemplo B"),stringsAsFactors=FALSE)
  controls$Concentracion_uM <- NA_real_
  controls$Viabilidad_pct <- 100+c(-2,0,2)[controls$Replica]
  controls$observation_id <- paste0(controls$control_type,"-",controls$Replica)
  controls$experiment_id <- "SYNTHETIC-1"
  controls$control_source <- "Synthetic shared reference"
  controls$inference_eligible <- TRUE
  rbind(d,controls[,names(d)])
})

palette_ldj <- function(n, palette) {
  if (palette == "Viridis") return(grDevices::hcl.colors(n, "viridis"))
  if (palette == "Grises") return(grDevices::gray.colors(n, start = 0.15, end = 0.7))
  if (palette == "Prism clasico B/N")
    return(rep(c("#000000", "#595959", "#888888", "#BBBBBB", "#E0E0E0"), length.out = n))
  base <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
            "#E69F00", "#56B4E9", "#F0E442", "#000000")
  rep(base, length.out = n)
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f5f7fa; }
    .well { background: white; border: 1px solid #dde3e9; }
    .title-line { margin-bottom: 18px; }
    .help-note { color: #56616c; font-size: 12px; }
  "))),
  div(class = "title-line",
      h2("LDJ Prism", style = "margin-bottom: 3px;"),
      p("Laboratorio de Virología Molecular · Editor de figuras con R y ggplot2",
        class = "help-note")),
  sidebarLayout(
    sidebarPanel(width = 4,
      tabsetPanel(id = "inspector", type = "tabs",
tabPanel("Datos",
p(if(private_mode) "Entorno privado: datos de usuario habilitados." else "Demo sintética: importación de usuario deshabilitada."),
      uiOutput("import_controls"),
      selectInput("sheet", "Hoja del libro", choices=NULL),
      selectInput("decimal", "Separador decimal", c("Punto"=".", "Coma"=",")),
      actionButton("apply_grid", "Aplicar tabla al gráfico"),
      actionButton("reset_grid", "Restaurar original"),
      selectInput("x", "Variable X", choices = names(demo_data), selected="Concentracion_uM"),
      selectInput("y", "Variable Y (numérica)", choices = names(demo_data)[vapply(demo_data,is.numeric,logical(1))], selected="Viabilidad_pct"),
      selectInput("group", "Color / grupo", choices = c("Sin agrupación"=".none",stats::setNames(names(demo_data),names(demo_data))),selected="Compuesto")
),
tabPanel("Gráfica",
selectInput("module", "Análisis", c("Editor universal"="universal","Citotoxicidad MTT"="mtt","Actividad farmacológica"="activity","Potencia y seguridad"="safety")),
textInput("control_label_default", "Etiqueta del control", "No tratado"),
selectInput("control_type_default", "Tipo predeterminado de control cero",
 c("No tratado"="untreated","Vehículo"="vehicle","Control negativo"="negative",
 "Control positivo"="positive","Infectado sin tratamiento"="infected_untreated")),
selectInput("dispersion", "Dispersión", c("SD","SEM","IC95")),
helpText("Para distinguir controles en la tabla: control_type, control_label, observation_id, experiment_id, control_source e inference_eligible. Sin procedencia completa se muestran solo como referencia visual."),
selectInput("threshold_mode", "Definición del umbral", c("Respuesta absoluta"="absolute","Porcentaje de la media del control"="control")),
selectInput("zero_policy", "Dosis cero en el ajuste", c("Excluir del ajuste; conservar control"="exclude","Incluir en ajuste"="include")),
checkboxInput("multipanel", "Figura multipanel: vista general y detalle por serie", FALSE),
textInput("experiment_id", "Identificador del experimento", ""),
textInput("control_experiment", "Experimento del control", ""),
textInput("control_source", "Procedencia del control", ""),
selectInput("model", "Modelo", c("4PL","3PL")),
textInput("model_reason", "Justificación del modelo/restricciones", ""),
checkboxInput("independent", "Confirmo réplicas independientes y control del mismo experimento", FALSE),
numericInput("control", "Concentración del control", 0, min=0),
selectInput("unit", "Unidad de concentración", c("uM","nM","mM","M")),
numericInput("ic50_value", "IC50 para integración", 1, min=0),
numericInput("cc20_value", "CC20 para integración", 2, min=0),
numericInput("cc50_value", "CC50 para integración", 5, min=0),
selectInput("ic_unit", "Unidad IC50", c("uM","nM","mM","M")),
selectInput("cc_unit", "Unidad CC20/CC50", c("uM","nM","mM","M")),
selectInput("kind", "Tipo de gráfica",
                  choices = c("Dispersión" = "scatter",
                              "Curva de medias" = "line",
                              "Boxplot + puntos" = "box", "Barras: media ± SD" = "bar")),
selectInput("preset", "Preset LDJ-FIG v2.0", c("Color accesible"="color", "Blanco y negro"="mono", "Presentación"="slides")),
actionButton("apply_preset", "Aplicar preset"), helpText("Propuestas de estilo; pendientes de comparación con figuras de referencia.")
),
tabPanel("Ejes",
textInput("plot_title", "Título", ""),
      textInput("x_label", "Nombre del eje X", "Concentración (µM)"),
      textInput("y_label", "Nombre del eje Y", "Viabilidad celular (%)"),
      selectInput("x_scale", "Escala X",
                  choices = c("Lineal" = "linear", "Log10" = "log10"), selected = "log10"),
      selectInput("y_scale", "Escala Y",
                  choices = c("Lineal" = "linear", "Log10" = "log10"))
),
tabPanel("Colores",
selectInput("palette", "Paleta",
                  choices = c("LDJ - Okabe-Ito" = "Okabe-Ito",
                              "Prism clasico - blanco y negro" = "Prism clasico B/N",
                              "Viridis" = "Viridis", "Grises" = "Grises",
                              "Elegir colores manualmente" = "Personalizada")),
      conditionalPanel("input.palette == 'Personalizada'",
                       uiOutput("custom_color_inputs"))
),
tabPanel("Símbolos y líneas",
checkboxInput("points", "Mostrar observaciones individuales", TRUE),
      numericInput("point_size", "Tamaño de símbolos", 2, min = 0.5, max = 9, step = 0.5),
      selectInput("point_shape", "Forma de símbolos",
                  choices = c("Círculo" = "16", "Triángulo" = "17", "Cuadrado" = "15",
                              "Diamante" = "18", "Círculo abierto" = "1")),
      sliderInput("alpha", "Opacidad de símbolos", min = 0.1, max = 1, value = 0.8, step = 0.05),
      selectInput("line_type", "Estilo de línea",
                  choices = c("Continua" = "solid", "Guiones" = "dashed",
                              "Puntos" = "dotted", "Punto-guion" = "dotdash")),
      sliderInput("line_width", "Grosor de línea", min = 0.3, max = 2.5, value = 0.9)
),
tabPanel("Leyendas",
selectInput("legend_position", "Posición de leyenda",
                  choices = c("Derecha" = "right", "Abajo" = "bottom",
                              "Arriba" = "top", "Izquierda" = "left", "Oculta" = "none")),
      textInput("legend_title", "Título de la leyenda", "")
),
tabPanel("Anotaciones",
numericInput("font_size", "Tamaño base (pt)", 12, min = 7, max = 24),
      selectInput("font_family", "Familia tipográfica",
                  choices = c("Sans serif" = "sans", "Serif" = "serif", "Monoespaciada" = "mono")),
      checkboxInput("grid_lines", "Mostrar cuadrícula", FALSE),
textInput("caption", "Nota al pie", "Datos sintéticos · sin inferencia estadística")
),
tabPanel("Exportar",
numericInput("width", "Ancho de exportación (pulgadas)", 7, min = 2, max = 20),
      numericInput("height", "Alto de exportación (pulgadas)", 5, min = 2, max = 20),
      selectInput("dpi", "Resolución de imagen (dpi)",
                  choices = c("300" = "300", "600" = "600", "900" = "900"),
                  selected = "600"),
      fluidRow(
        column(4, downloadButton("png", "PNG")),
        column(4, downloadButton("pdf", "PDF")),
        column(4, downloadButton("svg", "SVG"))
      ),
downloadButton("config", "Configuración JSON")
)
      ),
      p("Prototipo visual: aún no estima IC50/CC50 ni realiza inferencia estadística.",
        class = "help-note", style = "margin-top: 12px;")
    ),
    mainPanel(width = 8,
      wellPanel(
        plotOutput("plot", height = "520px"),
        uiOutput("status")
      ),
      h4("Vista previa de datos"),
      rHandsontableOutput("grid"),
      h4("Resultados"), verbatimTextOutput("analysis_status"),
      tableOutput("results"), tableOutput("anova"), tableOutput("diagnostics"), tableOutput("comparisons"),
      p("Piloto con datos sintéticos. Las opciones visuales no modifican la tabla original.",
        class = "help-note")
    )
  )
)

server <- function(input, output, session) {
  source_data <- reactiveVal(demo_data)
  working <- reactiveVal(demo_data)
  dataset <- reactive(working())
  output$import_controls <- renderUI({
    if(!private_mode) return(helpText("Para probar pegado, copie las filas sintéticas de la cuadrícula."))
    tagList(fileInput("upload", "Importar CSV/XLSX", accept=c(".csv",".xlsx")),
      selectInput("separator", "Separador CSV", c("Coma"=",","Punto y coma"=";","Tabulación"="\\t")))
  })
  observeEvent(input$upload, {
    req(private_mode)
    tryCatch({
      ext <- tolower(tools::file_ext(input$upload$name))
      if(ext=="xlsx") {
        sheets <- readxl::excel_sheets(input$upload$datapath)
        updateSelectInput(session,"sheet",choices=sheets,selected=sheets[1])
      } else if(ext=="csv") {
        d <- read.table(input$upload$datapath,header=TRUE,sep=input$separator,
          dec=input$decimal,check.names=FALSE,stringsAsFactors=FALSE)
        source_data(d); working(d)
      } else stop("Use CSV o XLSX.")
    },error=function(e) showNotification(conditionMessage(e),type="error"))
  })
  observeEvent(input$sheet, {
    req(private_mode,input$upload,tolower(tools::file_ext(input$upload$name))=="xlsx",input$sheet)
    tryCatch({
      d <- as.data.frame(readxl::read_excel(input$upload$datapath,sheet=input$sheet))
      source_data(d); working(d)
    },error=function(e) showNotification(conditionMessage(e),type="error"))
  })
  output$grid <- renderRHandsontable({
    rhandsontable(working(), readOnly=!private_mode, rowHeaders=TRUE, stretchH="all")
  })
  observeEvent(input$apply_grid, {
    req(private_mode,input$grid)
    working(hot_to_r(input$grid))
  })
  observeEvent(input$reset_grid, working(source_data()))
  pending_selection <- reactiveVal(NULL)
  selection_ready <- reactive({
    req(input$x,input$y,input$group)
    p <- pending_selection()
    if(!is.null(p)) req(identical(input$x,p$x),identical(input$y,p$y),identical(input$group,p$group))
    d <- dataset()
    shiny::validate(need(input$x%in%names(d),"La columna X seleccionada no existe."),
      need(input$y%in%names(d),"La columna Y seleccionada no existe."),
      need(input$group==".none" || input$group%in%names(d),"La columna de grupo no existe."))
    TRUE
  })
  observeEvent(list(input$x,input$y,input$group), {
    p <- pending_selection()
    if(!is.null(p) && identical(input$x,p$x) && identical(input$y,p$y) &&
       identical(input$group,p$group)) pending_selection(NULL)
  },priority=90)
  physical_data <- reactive({
    req(selection_ready())
    prepare_controls(dataset(),input$x,input$y,
      if(is.null(input$control_label_default)) "No tratado" else input$control_label_default,
      if(is.null(input$control_type_default)) "untreated" else input$control_type_default,
      if(identical(input$group,".none")) NULL else input$group)
  })
  visual_data <- reactive(expand_control_contexts(physical_data(),if(identical(input$group,".none")) NULL else input$group))
  mapped <- reactive({
    req(selection_ready())
    d <- visual_data()
    # Independent fits use only rows with measured concentration; no fabricated zero.
    d <- d[!d$.is_control | is.finite(d[[input$x]]),,drop=FALSE]
    map_data(d,input$x,input$y,input$group,input$decimal)
  })
  inference_data <- reactive({
    req(selection_ready())
    d <- visual_data()
    controls <- d[d$.is_control,,drop=FALSE]
    if(!nrow(controls) || any(!controls$.eligible)) stop("Seleccione controles elegibles con procedencia completa.")
    if(any(!is.finite(controls[[input$x]]))) stop("Control con concentración no informada: la comparación por dosis requiere mapeo explícito.")
    map_data(d,input$x,input$y,input$group,input$decimal)
  })

  analysis <- reactive({
    req(selection_ready())
    if(is.null(input$module) || input$module=="universal") return(NULL)
    if(input$module=="safety") return(list(table=selectivity(input$ic50_value,
      input$cc20_value,input$cc50_value,input$ic_unit,input$cc_unit)))
    if(!nzchar(trimws(input$model_reason))) stop("Documente la justificación del modelo.")
    if(identical(input$threshold_mode,"control")) inference_data()
    d <- mapped()
    groups <- split(d,d$series)
    fits <- lapply(names(groups),function(g) {
      targets <- if(input$module=="mtt") c(50,80) else 50
      rows <- lapply(targets,function(t) {
        z <- fit_response(groups[[g]],input$model,t,input$threshold_mode,input$control,input$zero_policy)$table
        z$series <- g; z$unit <- input$unit; z
      })
      do.call(rbind,rows)
    })
    list(table=do.call(rbind,fits))
  })
  output$results <- renderTable({req(analysis()); analysis()$table},digits=4)
  output$analysis_status <- renderText({
    tryCatch({analysis(); "Resultados calculados. IC: método delta; revise extrapolación y adecuación del modelo."},
      error=function(e) conditionMessage(e))
  })
  output$anova <- renderTable({
    req(input$module %in% c("mtt","activity"),isTRUE(input$independent))
    validate_provenance(input$experiment_id,input$control_experiment,input$control_source,input$independent)
    do.call(rbind,lapply(split(inference_data(),inference_data()$series),function(g) {
      z <- compare_control(g,input$control)$anova
      z$term <- rownames(z); z$series <- g$series[1]; z
    }))
  })
  output$diagnostics <- renderTable({
    req(input$module %in% c("mtt","activity"),isTRUE(input$independent))
    validate_provenance(input$experiment_id,input$control_experiment,input$control_source,input$independent)
    do.call(rbind,lapply(split(inference_data(),inference_data()$series),function(g) {
      z <- compare_control(g,input$control)$diagnostics; z$series <- g$series[1]; z
    }))
  })
  output$comparisons <- renderTable({
    req(input$module %in% c("mtt","activity"),isTRUE(input$independent))
    validate_provenance(input$experiment_id,input$control_experiment,input$control_source,input$independent)
    d <- inference_data()
    do.call(rbind,lapply(split(d,d$series),function(g) {
      z <- compare_control(g,input$control)$dunnett
      z$series <- g$series[1]; z
    }))
  })

  observeEvent(dataset(), {
    d <- dataset(); cols <- names(d); numeric_cols <- cols[vapply(d,is.numeric,logical(1))]
    choose <- function(current,choices,preferred) {
      if(!is.null(current) && current%in%choices) current else
        if(preferred%in%choices) preferred else if(length(choices)) choices[1] else ""
    }
    proposed <- list(x=choose(isolate(input$x),cols,"Concentracion_uM"),
      y=choose(isolate(input$y),numeric_cols,"Viabilidad_pct"),
      group=choose(isolate(input$group),c(".none",cols),"Compuesto"))
    pending_selection(proposed)
    for(id in c("x","y","group")) freezeReactiveValue(input,id)
    updateSelectInput(session,"x",choices=cols,selected=proposed$x)
    updateSelectInput(session,"y",choices=numeric_cols,selected=proposed$y)
    updateSelectInput(session,"group",choices=c("Sin agrupación"=".none",stats::setNames(cols,cols)),selected=proposed$group)
    if(!length(numeric_cols)) showNotification("La tabla no contiene columnas numéricas para Y.",type="error")
  },priority=100)

  output$custom_color_inputs <- renderUI({
    req(input$palette == "Personalizada", input$group)
    d <- dataset()
    if (identical(input$group, ".none") || !(input$group %in% names(d))) {
      groups <- "Datos"
    } else {
      groups <- levels(as.factor(d[[input$group]]))
    }
    tagList(lapply(seq_along(groups), function(i) {
      colourpicker::colourInput(
        inputId = paste0("custom_color_", i),
        label = paste0("Color: ", groups[[i]]),
        value = palette_ldj(length(groups), "Okabe-Ito")[[i]],
        showColour = "both", allowTransparent = FALSE
      )
    }))
  })

  observeEvent(input$apply_preset, {
    preset <- input$preset
    updateSelectInput(session, "palette", selected = if (preset == "mono") "Prism clasico B/N" else "Okabe-Ito")
    updateNumericInput(session, "font_size", value = if (preset == "slides") 18 else 12)
    updateNumericInput(session, "point_size", value = if (preset == "slides") 3 else 2)
    updateSelectInput(session, "font_family", selected = "sans")
    updateSelectInput(session, "legend_position", selected = "right")
    updateCheckboxInput(session, "grid_lines", value = FALSE)
    updateSliderInput(session, "line_width", value = 0.9)
    updateSelectInput(session, "dpi", selected = "600")
  })

  output$config <- downloadHandler(
    filename = function() "LDJ_Prism_config.json",
    content = function(file) {
      ids <- c("x", "y", "group", "kind", "plot_title", "x_label", "y_label",
               "x_scale", "y_scale", "palette", "points", "point_size", "point_shape",
               "alpha", "line_type", "line_width", "font_size", "font_family",
               "legend_position", "legend_title", "grid_lines", "caption", "width",
               "height", "dpi", "preset")
      values <- reactiveValuesToList(input)
      colors <- values[grepl("^custom_color_", names(values))]
      jsonlite::write_json(list(schema_version = 1, data = "synthetic",
        settings = values[intersect(ids, names(values))], colors = colors, analysis=list(threshold_mode=input$threshold_mode,zero_policy=input$zero_policy,model=input$model,model_reason=input$model_reason,experiment=input$experiment_id,control_experiment=input$control_experiment,control_source=input$control_source)),
        file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
  plot_builder <- reactive({
    req(selection_ready())
    d <- visual_data()
    shiny::validate(
      need(nrow(d) > 0, "El archivo está vacío."),
      need(!anyDuplicated(names(d)), "Los nombres de columnas deben ser únicos."),
      need(input$x %in% names(d), "Seleccione un eje X válido."),
      need(input$y %in% names(d), "Seleccione un eje Y válido."),
      need(is.numeric(d[[input$y]]), "La variable Y debe ser numérica.")
    )
    separate_controls <- d$.is_control & !input$kind%in%c("bar","box") & (is.na(d[[input$x]]) |
      (input$x_scale=="log10" & !input$kind%in%c("bar","box")))
    d <- d[!separate_controls & (d$.is_control | !is.na(d[[input$x]])) & is.finite(d[[input$y]]),,drop=FALSE]
    shiny::validate(need(nrow(d) > 0, "No hay observaciones válidas para graficar."))
    x_numeric <- is.numeric(d[[input$x]])
    if (input$x_scale == "log10" && !input$kind%in%c("bar","box")) {
      shiny::validate(need(x_numeric, "Para usar Log10 en X, elija una variable numérica."))
      d <- d[!d$.is_control,,drop=FALSE]
      shiny::validate(need(nrow(d)>0 && all(d[[input$x]] > 0), "Log10 requiere dosis positivas; los controles se muestran por separado."))
    }
    if (input$y_scale == "log10")
      shiny::validate(need(all(d[[input$y]] > 0), "Log10 en Y requiere valores positivos."))

    grouped <- !is.null(input$group) && input$group != ".none" &&
      input$group %in% names(d)
    if (grouped) {
      d$ldj_group <- as.factor(d[[input$group]])
    } else {
      d$ldj_group <- factor("Datos")
    }
    if (input$kind == "bar") {
      p <- ggplot(d,aes(x=.category,y=.data[[input$y]],color=ldj_group,group=ldj_group)) +
        stat_summary(fun=mean,geom="col",position=position_dodge(0.8),fill="white") +
        stat_summary(fun.data=function(y) dispersion_summary(y,if(is.null(input$dispersion)) "SD" else input$dispersion),geom="errorbar",position=position_dodge(0.8),width=.2) +
        geom_point(position=position_jitterdodge(jitter.width=.08,seed=42),
          size=input$point_size,shape=as.integer(input$point_shape),alpha=input$alpha)
    } else if (input$kind == "box") {
      d$ldj_x <- d$.category
      p <- ggplot(d, aes(x = ldj_x, y = .data[[input$y]], color = ldj_group))
      p <- p + geom_boxplot(outlier.shape = NA, linewidth = input$line_width)
      if (isTRUE(input$points))
        p <- p + geom_point(position = position_jitterdodge(jitter.width = 0.12),
                            alpha = input$alpha, size = input$point_size,
                            shape = as.integer(input$point_shape))
    } else {
      p <- ggplot(d, aes(x = .data[[input$x]], y = .data[[input$y]],
                         color = ldj_group, group = ldj_group))
      if (input$kind == "line") {
        shiny::validate(need(x_numeric, "La curva de medias requiere un eje X numérico."))
        p <- p + stat_summary(fun = mean, geom = "line",
                              linewidth = input$line_width, linetype = input$line_type,
                              na.rm = TRUE)
      }
      if (isTRUE(input$points) || input$kind == "scatter")
        p <- p + geom_point(alpha = input$alpha, size = input$point_size,
                            shape = as.integer(input$point_shape))
      if (input$x_scale == "log10") p <- p + scale_x_log10()
    }
    if (input$y_scale == "log10") p <- p + scale_y_log10()
    n_groups <- nlevels(d$ldj_group)
    plot_colors <- palette_ldj(n_groups, input$palette)
    if (identical(input$palette, "Personalizada")) {
      plot_colors <- vapply(seq_len(n_groups), function(i) {
        chosen <- input[[paste0("custom_color_", i)]]
        if (is.null(chosen) ||
            !grepl("^#[0-9A-Fa-f]{6}$", chosen)) {
          return(palette_ldj(n_groups, "Okabe-Ito")[[i]])
        }
        chosen
      }, character(1))
    }
    title_legend <- if (nzchar(input$legend_title)) input$legend_title else
      if (grouped) input$group else NULL
    p <- p + scale_color_manual(values = plot_colors) +
      labs(title = if (nzchar(input$plot_title)) input$plot_title else NULL,
           x = input$x_label, y = input$y_label, color = title_legend, caption = input$caption) +
      theme_classic(base_size = input$font_size, base_family = input$font_family) +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.major = if (isTRUE(input$grid_lines)) element_line(
              linewidth = 0.2, colour = "#DFDFDF") else element_blank(),
            axis.line = element_line(linewidth = 0.5),
            legend.position = if (grouped) input$legend_position else "none")
    p
  })

  control_plot <- reactive({
    d <- visual_data(); d <- d[d$.is_control,,drop=FALSE]
    req(nrow(d)>0)
    if(input$y_scale=="log10" && any(d[[input$y]]<=0))
      stop("Los controles tienen valores no positivos: seleccione Y lineal.")
    d$.series <- if(input$group==".none") "Datos" else as.character(d[[input$group]])
    p <- ggplot(d,aes(.control_label,.data[[input$y]],color=.series)) +
      geom_point(position=position_jitter(width=.07,seed=42),size=input$point_size,shape=as.integer(input$point_shape)) +
      stat_summary(fun=mean,geom="point",shape=18,size=3) +
      stat_summary(fun.data=function(y) dispersion_summary(y,if(is.null(input$dispersion)) "SD" else input$dispersion),geom="errorbar",width=.15) +
      labs(title="Controles experimentales (representación separada)",x=NULL,y=input$y_label,color=input$legend_title) +
      theme_classic(base_size=input$font_size,base_family=input$font_family) +
      theme(legend.position=input$legend_position)
    all_data <- visual_data()
    groups <- if(input$group==".none") "Datos" else levels(factor(all_data[[input$group]]))
    colors <- palette_ldj(length(groups),input$palette)
    if(input$palette=="Personalizada") colors <- vapply(seq_along(groups),function(i) {
      z <- input[[paste0("custom_color_",i)]]
      if(is.null(z)) palette_ldj(length(groups),"Okabe-Ito")[i] else z
    },character(1))
    p <- p + scale_color_manual(values=setNames(colors,groups)) + facet_wrap(~.series)
    if(input$y_scale=="log10") p <- p+scale_y_log10()
    p
  })
  final_plot <- reactive({
    req(selection_ready())
    if(identical(input$module,"safety")) {
      z <- analysis()$table
      safety <- ggplot(data.frame(parameter=c("IC50","CC20","CC50"),
        value=as.numeric(z[1,1:3])),aes(parameter,value)) +
        geom_point(size=input$point_size,color=palette_ldj(1,input$palette)) +
        scale_y_log10() + theme_classic(base_size=input$font_size) +
        labs(title=input$plot_title,x=input$x_label,y="Concentration (M)",caption=input$caption)
      if(any(visual_data()$.is_control)) return(patchwork::wrap_plots(safety,control_plot(),ncol=1))
      return(safety)
    }
    p <- plot_builder()
    if(!is.null(input$module) && input$module %in% c("mtt","activity") && input$kind=="line") {
      analysis()
      curves <- do.call(rbind,lapply(split(mapped(),mapped()$series),function(g) {
        cv <- fit_response(g,input$model,50,input$threshold_mode,input$control,input$zero_policy)$curve
        cv$series <- g$series[1]; cv
      }))
      p <- p + geom_line(data=curves,aes(x=dose,y=response,color=series,group=series),
        inherit.aes=FALSE,linewidth=input$line_width,linetype="dashed")
    }
    controls_separate <- (!input$kind%in%c("bar","box") && any(visual_data()$.is_control & is.na(visual_data()[[input$x]]))) || (input$x_scale=="log10" && !input$kind%in%c("bar","box") && any(visual_data()$.is_control))
    if(isTRUE(input$multipanel)) {
      detail <- p + facet_wrap(~ldj_group) + labs(title="Detalle por serie")
      if(controls_separate) return(patchwork::wrap_plots(p,detail,control_plot(),ncol=1))
      return(patchwork::wrap_plots(p,detail,ncol=1))
    }
    if(controls_separate) return(patchwork::wrap_plots(p,control_plot(),ncol=1))
    p
  })
  output$plot <- renderPlot(final_plot(),res=110)
  output$preview <- renderTable(utils::head(dataset(), 8), rownames = FALSE)
  output$status <- renderUI({
    p(class = "help-note",
      paste(nrow(dataset()), "filas ·", ncol(dataset()),
            "columnas · datos", if(private_mode) "de sesión privada" else "sintéticos"))
  })
  export_plot <- function(file, format) {
    ggplot2::ggsave(
      filename = file, plot = final_plot(), device = format,
      width = input$width, height = input$height,
      units = "in", dpi = as.integer(input$dpi), bg = "white"
    )
  }
  output$png <- downloadHandler(filename = function() "LDJ_Prism.png",
                                content = function(file) export_plot(file, "png"))
  output$pdf <- downloadHandler(filename = function() "LDJ_Prism.pdf",
                                content = function(file) export_plot(file, "pdf"))
  output$svg <- downloadHandler(filename = function() "LDJ_Prism.svg",
                                content = function(file) export_plot(file, "svg"))
}
shinyApp(ui, server)
