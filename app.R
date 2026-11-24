# LDJ Prism · Shiny + ggplot2 MVP
# Datos sintéticos de demostración; esta aplicación no almacena archivos cargados.
library(shiny)
library(ggplot2)
library(svglite)
library(colourpicker)
library(rhandsontable)
library(readxl)
source(if(file.exists("R/analysis.R")) "R/analysis.R" else "apps/ldj-prism/R/analysis.R", local=TRUE)
for(module_file in c("mtt_visualization_v4_LDJ_Prism.R","mtt_data_entry.R","mtt_shiny.R","pharmacology.R"))
  source(file.path(if(dir.exists("R")) "R" else "apps/ldj-prism/R",module_file),local=TRUE)
private_mode <- identical(Sys.getenv("LDJ_PRISM_MODE"), "private")
data_entry_enabled <- TRUE

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

# Formula-generated teaching examples. No experimental input is used.
editorial_example <- function(module) {
  activity <- identical(module,"activity")
  d <- expand.grid(Concentracion_uM=c(.025,.08,.25,.8,2.5,8,25),
    Replica=1:3,Compuesto=paste("Serie sintética",LETTERS[1:4]),
    KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  midpoint <- c(.55,1.8,4.4,7)[match(d$Compuesto,unique(d$Compuesto))]
  d$Viabilidad_pct <- 5+95/(1+(d$Concentracion_uM/midpoint)^1.35)+
    c(-1.8,.2,1.6)[d$Replica]*rep(c(.7,1.1,1.8),length.out=nrow(d))
  d$control_type <- "treatment"
  d$observation_id <- paste0(module,"-T",seq_len(nrow(d)))
  d$independent_unit_id <- d$observation_id
  d$experiment_id <- paste0("SYNTHETIC-",toupper(module))
  d$control_source <- "Generador sintético LDJ"
  d$inference_eligible <- TRUE
  if(activity) {
    ctrl <- d[seq_len(12),]
    ctrl$Compuesto <- rep(unique(d$Compuesto),each=3)
    ctrl$Concentracion_uM <- NA_real_
    ctrl$Viabilidad_pct <- rep(c(96,99,101),4)
    ctrl$control_type <- "infected_untreated"
    ctrl$observation_id <- paste0("activity-C",seq_len(nrow(ctrl)))
    ctrl$independent_unit_id <- ctrl$observation_id
    d <- rbind(d,ctrl)
    return(d)
  }
  z <- d[seq_len(12),]
  z$Compuesto <- rep(unique(d$Compuesto),each=3)
  z$parameter <- rep(c("IC50","CC20","CC50"),4)
  z$unit <- rep(c("uM","uM","nM","uM"),each=3)
  z$estimate <- c(.035,.32,.57, .38,.78,1.8, 105,1720,4430, 5,2,7.2)
  z$lower <- z$upper <- NA_real_
  z$Concentracion_uM <- z$Viabilidad_pct <- NA_real_
  d$parameter <- d$unit <- NA_character_
  d$estimate <- d$lower <- d$upper <- NA_real_
  rbind(d,z)
}

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
    .mtt-preview { overflow: auto; background: white; }
    @media print { .sidebar-panel, .title-line, details { display:none; }
      .main-panel { width:100%; } .mtt-preview { overflow:visible; } }
  "))),
  div(class = "title-line",
      h2("LDJ Prism", style = "margin-bottom: 3px;"),
      p("Laboratorio de Virología Molecular · Editor de figuras con R y ggplot2",
        class = "help-note")),
  selectInput("module", "Módulo", c("Editor universal"="universal","Citotoxicidad MTT"="mtt",
    "Actividad farmacológica"="activity","Potencia y seguridad"="safety")),
  sidebarLayout(
    sidebarPanel(width = 3, class="sidebar-panel",
      tabsetPanel(id = "inspector", type = "tabs", selected="Gráfica",
tabPanel("Datos",
p("Los datos enviados se procesan en la sesión del servidor. La demo pública no garantiza confidencialidad: utilice únicamente datos no confidenciales; reserve datos internos para alojamiento autenticado."),
checkboxInput("has_header","El archivo incluye encabezados",TRUE),
textInput("column_names","Nombres de columnas separados por coma",""),
actionButton("rename_columns","Renombrar columnas"),
actionButton("add_row","Añadir fila"),actionButton("add_column","Añadir columna"),
p(if(private_mode) "Entorno privado configurado." else "Demo pública: utilice únicamente tablas no confidenciales."),
      uiOutput("import_controls"),
      tags$details(tags$summary("Pegar tabla desde Excel (bloque tabulado)"),
        textAreaInput("paste_tsv","Pegue filas y columnas tabuladas",rows=5),
        actionButton("apply_paste","Reemplazar tabla con datos pegados")),
      selectInput("sheet", "Hoja del libro", choices=NULL),
      selectInput("decimal", "Separador decimal", c("Punto"=".", "Coma"=",")),
      actionButton("apply_grid", "Aplicar tabla al gráfico"),
      actionButton("reset_grid", "Restaurar original"),
      selectInput("x", "Variable X", choices = names(demo_data), selected="Concentracion_uM"),
      selectInput("y", "Variable Y (numérica)", choices = names(demo_data)[vapply(demo_data,is.numeric,logical(1))], selected="Viabilidad_pct"),
      selectInput("group", "Color / grupo", choices = c("Sin agrupación"=".none",stats::setNames(names(demo_data),names(demo_data))),selected="Compuesto")
),
tabPanel("Análisis",
conditionalPanel("input.module === 'mtt'",
  selectInput("mtt_view","Vista MTT",c("Editorial 4 × 2"="editorial","Vista general"="general"))),
conditionalPanel("input.module === 'mtt' && input.mtt_view !== 'general'",
selectInput("mtt_source","Fuente MTT",if(private_mode) c("Demostración sintética"="synthetic","Bundle auditado privado (LDJ_MTT_ROOT)"="private") else c("Demostración sintética"="synthetic")),
checkboxInput("mtt_user_data","Usar tabla seleccionada",FALSE),
selectInput("mtt_display_unit","Unidad de representación",c("uM","nM","mM","M")),
selectInput("mtt_series_1","Serie A/B",choices=NULL),
selectInput("mtt_series_2","Serie C/D",choices=NULL),
selectInput("mtt_series_3","Serie E/F",choices=NULL),
selectInput("mtt_series_4","Serie G/H",choices=NULL),
tags$details(tags$summary("Asignar controles por serie y experimento"),
  helpText("Las referencias visuales no duplican observaciones físicas ni habilitan contrastes inferenciales."),
  rHandsontableOutput("mtt_control_grid"),
  actionButton("mtt_control_apply","Guardar asignaciones de controles"),
  textOutput("mtt_control_status")),
actionButton("mtt_run","Ejecutar análisis de la tabla"),
textOutput("mtt_run_status"),
selectInput("mtt_inference","Análisis estadístico",c("ANOVA de una vía + Dunnett contra control"="anova","Solo análisis descriptivo"="descriptive")),
selectInput("mtt_reference","Control de referencia",c("No tratado"="untreated","Vehículo"="vehicle","Negativo"="negative","Positivo"="positive","Infectado sin tratamiento"="infected_untreated")),
h4("ANOVA MTT"),tableOutput("mtt_anova_table"),
h4("Dunnett MTT"),tableOutput("mtt_dunnett_table"),
tags$details(tags$summary("Diagnósticos y trazabilidad"),verbatimTextOutput("mtt_inference_report")),
downloadButton("mtt_statistics","Descargar resultados MTT (JSON)")),
actionButton("run_analysis","Ejecutar análisis"),
textOutput("workflow_status"),
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
conditionalPanel("input.module === 'safety'",
selectInput("safety_display","Vista de potencia y seguridad",c("Resumen transversal"="summary","Editorial 4×2"="editorial")),
conditionalPanel("input.module === 'safety' && input.safety_display === 'editorial'",
  selectInput("cc20_source","Fuente de CC20",c("Calculado por regresión"="regression",
    "Proporcionado en tabla"="table","Mostrar ambas estimaciones"="both"),selected="table"),
  numericInput("cc20_tolerance","Tolerancia (% del CC20 del ajuste)",10,min=0,step=1),
  checkboxInput("safety_show_fit","Superponer la curva ajustada",FALSE),
  textInput("safety_viability_label","Etiqueta de la franja de viabilidad","80–100% viable")),
helpText("Editorial: combine en una tabla cuatro grupos, filas de parámetros con parameter/estimate/unit y filas de viabilidad con parameter vacío y columnas X/Y. X debe usar la unidad de concentración seleccionada."),

checkboxInput("safety_table","Leer parámetros de la tabla (parameter, estimate, unit; lower/upper opcionales)",FALSE),

selectInput("safety_unit","Unidad de presentación de parámetros",c("nM","uM","mM","M"),selected="uM"),
numericInput("ic50_value", "IC50 para integración", NA_real_, min=0),
numericInput("cc20_value", "CC20 para integración", NA_real_, min=0),
numericInput("cc50_value", "CC50 para integración", NA_real_, min=0),
selectInput("ic_unit", "Unidad IC50", c("uM","nM","mM","M")),
selectInput("cc_unit", "Unidad CC20/CC50", c("uM","nM","mM","M"))),
conditionalPanel("input.module === 'activity'",
selectInput("activity_control_type","Control inferencial de actividad",c("Infectado sin tratamiento"="infected_untreated","Células sanas sin tratamiento"="untreated","Vehículo"="vehicle","Negativo"="negative","Positivo"="positive")),
checkboxInput("activity_editorial","Actividad: pares editoriales por serie (hasta 4)",TRUE),
checkboxInput("activity_other_controls","Mostrar otros controles experimentales",TRUE)),
),
tabPanel("Gráfica",
selectInput("kind", "Tipo de gráfica",
                  choices = c("Dispersión" = "scatter",
                              "Curva de medias" = "line",
                              "Boxplot + puntos" = "box", "Barras: media ± SD" = "bar")),
selectInput("preset", "Preset LDJ-FIG v2.0", c("Color accesible"="color", "Blanco y negro"="mono", "Presentación"="slides")),
actionButton("apply_preset", "Aplicar preset"), helpText("Propuestas de estilo; pendientes de comparación con figuras de referencia.")
),
tabPanel("Editor de paneles",
conditionalPanel("input.module !== 'universal'", uiOutput("editor_context")),
p("Las opciones gráficas no recalculan modelos ni comparaciones. La demo usa resultados calculados exclusivamente de datos sintéticos."),
selectInput("mtt_target","Aplicar estilo a",c("Todos los paneles"="all",LETTERS[1:8])),
checkboxInput("mtt_significance","Mostrar significancia",TRUE),
checkboxInput("mtt_anova_summary","Mostrar resumen ANOVA",TRUE),
checkboxInput("mtt_show_ns","Mostrar ns",TRUE),
numericInput("mtt_mark_size","Tamaño de significancia",3,min=1,max=8,step=.5),
numericInput("mtt_mark_offset","Separación de significancia",.07,min=.02,max=.25,step=.01),
textInput("mtt_title","Título del panel (vacío: automático)",""),
textInput("mtt_x","Eje X (vacío: global)",""),
textInput("mtt_y","Eje Y (vacío: global)",""),
textInput("mtt_hex","Color HEX (vacío: paleta)",""),
textInput("mtt_note","Anotación del panel",""),
textInput("mtt_ylim","Límites Y: mínimo,máximo",""),
textInput("mtt_xlim","Límites X de curva: mínimo,máximo",""),
selectInput("mtt_dispersion","Dispersión del panel",c("Global"="global","SD","SEM","IC95")),
textInput("mtt_size","Tamaño de símbolo (vacío: global)",""),
textInput("mtt_font","Tamaño de fuente (vacío: global)",""),
selectInput("mtt_shape","Símbolo del panel",c("Global"="global","Círculo"="21","Triángulo"="24","Cuadrado"="22","Diamante"="23")),
selectInput("mtt_scale","Escala X de curva",c("Global"="global","Log10"="log10","Lineal"="linear")),
selectInput("mtt_points","Réplicas del panel de barras",c("Global"="global","Mostrar"="show","Ocultar"="hide")),
actionButton("mtt_apply","Aplicar estilo"),actionButton("mtt_reset","Restablecer estilos por panel"),
sliderInput("preview_zoom","Zoom de vista previa",min=60,max=150,value=100,step=10),
actionButton("print_figure","Modo impresión",onclick="window.print()"),
verbatimTextOutput("mtt_audit")
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
tabPanel("Colores",ldj_color_ui()),
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
      p("Las estimaciones dependen del modelo y de sus diagnósticos. MTT v4 visualiza resultados conservados sin reajustarlos.",
        class = "help-note", style = "margin-top: 12px;")
    ),
    mainPanel(width = 9, class="main-panel",
      conditionalPanel("input.inspector === 'Datos'",
        wellPanel(h3("1. Datos"),selectInput("entry_type","Tipo de entrada",
          c("Observaciones individuales"="observations","Parámetros resumidos"="parameters")),
          actionButton("blank_table","Nueva tabla en blanco"),
          actionButton("load_example","Cargar ejemplo"),
          helpText("Edite celdas, pegue rangos o use el menú contextual para eliminar filas y columnas."),
          rHandsontableOutput("grid"),textOutput("table_state"),
          actionButton("confirm_table","Confirmar tabla y pasar a Análisis"))),
      conditionalPanel("input.inspector !== 'Datos' && input.inspector !== 'Análisis'",wellPanel(
        h3("3. Figura"), div(class="mtt-preview",uiOutput("plot_container")),
        uiOutput("status")
      )),
      conditionalPanel("input.inspector === 'Análisis'",
        h3("2. Análisis"),
        h4("Resultados"), verbatimTextOutput("analysis_status"),
        conditionalPanel("input.module === 'mtt' || input.module === 'activity'",
          tags$details(tags$summary("Conservar en el registro de esta sesión"),
            textInput("registry_assay","Identificador del ensayo"),
            textInput("registry_conditions","Línea celular, tiempo, normalización y condiciones"),
            actionButton("registry_save","Guardar resultados actuales"))),
        conditionalPanel("input.module === 'safety'",
          tags$details(tags$summary("Integrar resultados de MTT y Actividad farmacológica"),
            selectInput("registry_mtt","Ensayo MTT",choices=NULL),
            selectInput("registry_activity","Ensayo de actividad",choices=NULL),
            checkboxInput("registry_confirm","Confirmo la identidad de los compuestos y he revisado la compatibilidad de las condiciones",FALSE),
            actionButton("registry_integrate","Integrar resultados"))),
        downloadButton("registry_export","Exportar registro de sesión (JSON)"),
        tableOutput("results"), conditionalPanel("input.module == 'activity'",verbatimTextOutput("activity_audit")), tableOutput("anova"), tableOutput("diagnostics"), tableOutput("comparisons")),
      p("Piloto con datos sintéticos. Las opciones visuales no modifican la tabla original.",
        class = "help-note")
    )
  )
)

server <- function(input, output, session) {
  figure_template <- reactive(if (identical(input$module,"mtt") &&
    !identical(input$mtt_view,"general")) "mtt4x2" else "universal")
  manual <- reactiveVal(FALSE)
  manual_snapshot <- reactiveVal(NULL)
  draft_pending <- reactiveVal(FALSE)
  run_signature <- reactive(serialize(list(analysis_signature(),input$module,
    input$activity_control_type,input$safety_table,input$safety_display,input$decimal,
    input$ic50_value,input$cc20_value,input$cc50_value,input$ic_unit,input$cc_unit),NULL))
  analysis_ready <- reactive({
    req(!manual() || (!draft_pending() && identical(manual_snapshot(), run_signature())))
  })
  color_series <- reactive({
    if(identical(figure_template(),"mtt4x2") && !isTRUE(input$mtt_user_data))
      return(ldj_mtt_defaults()$order)
    if(is.null(input$group)||input$group==".none"||!input$group%in%names(dataset())) return("Datos")
    z <- as.character(dataset()[[input$group]]);sort(unique(z[!is.na(z)&nzchar(z)]))
  })
  appearance <- ldj_color_server(input,output,session,color_series)
  table_result <- reactiveVal(NULL)
  table_snapshot <- reactiveVal(NULL)
  mtt_control_map <- reactiveVal(NULL)
  analysis_signature <- reactive(serialize(list(data=dataset(),control_map=mtt_control_map(),x=input$x,y=input$y,
    group=input$group,series=lapply(1:4,function(i) input[[paste0("mtt_series_",i)]]),
    unit=input$unit,model=input$model,reason=input$model_reason,
    mtt_inference=input$mtt_inference,mtt_reference=input$mtt_reference,
    threshold=input$threshold_mode,zero=input$zero_policy,
    control=input$control,control_type=input$control_type_default,
    experiment=input$experiment_id,control_experiment=input$control_experiment,
    source=input$control_source,independent=input$independent),NULL))
  observeEvent(list(dataset(),input$group), {
    req(input$group,input$group%in%names(dataset()))
    values <- unique(as.character(observations()[[input$group]])); values <- values[!is.na(values)&nzchar(values)]
    for(i in 1:4) {
      old <- isolate(input[[paste0("mtt_series_",i)]])
      selected <- if(!is.null(old)&&old%in%values) old else if(length(values)>=i) values[i] else ""
      updateSelectInput(session,paste0("mtt_series_",i),choices=c("Seleccionar"="",values),selected=selected)
    }
  })
  run_mtt <- function(incoming=observations()) {
    tryCatch({
      req(selection_ready(),input$group!=".none")
      if (!identical(input$threshold_mode,"absolute"))
        stop("Esta plantilla sólo admite ajustes absolutos; el análisis relativo al control no está validado.")
      series <- vapply(1:4,function(i) input[[paste0("mtt_series_",i)]],character(1))
      incoming <- ldj_mtt_normalize_numeric(incoming,
        if(is.null(input$decimal)) "." else input$decimal)
      result <- ldj_mtt_analyze_table(incoming,input$x,input$y,input$group,
        series,input$unit,input$model,control_assignments=mtt_control_map(),
        inference=identical(input$mtt_inference,"anova"),reference=input$mtt_reference %||% "untreated",
        independent=input$independent)
      table_result(result)
      table_snapshot(analysis_signature())
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=NULL))
  }
  observeEvent(input$mtt_run,run_mtt())
  output$mtt_run_status <- renderText({
    if(is.null(table_result())) "Seleccione columnas, unidad y series; ejecute el análisis."
    else if(!identical(table_snapshot(),analysis_signature())) "Datos o configuración modificados: vuelva a ejecutar el análisis antes de exportar."
    else "Análisis vigente; los cambios visuales conservan los ajustes."
  })
  mtt_styles <- reactiveVal(list())
  mtt_bundle <- reactive({
    req(figure_template()=="mtt4x2",input$mtt_source)
    if(identical(input$mtt_source,"synthetic")) return(ldj_mtt_synthetic_bundle())
    req(private_mode)
    root <- Sys.getenv("LDJ_MTT_ROOT")
    validate(need(nzchar(root),"Configure LDJ_MTT_ROOT en el entorno privado autorizado."))
    validate(need(identical(Sys.getenv("LDJ_MTT_UNITS_VERIFIED"),"true"),
      "Verifique primero unidades de entrada: la referencia LET está en nM y los campos v4 se llaman conc_uM. LDJ_MTT_UNITS_VERIFIED=true requiere un bundle convertido íntegramente a µM, incluidos modelos e IC."))
    ldj_mtt_read_bundle(root)
  })
  observeEvent(input$mtt_apply,{
    tryCatch({
      cfg <- list()
      for(pair in list(c("title","mtt_title"),c("x_label","mtt_x"),c("y_label","mtt_y"),c("color","mtt_hex"),c("annotation","mtt_note"))) {
        v <- trimws(input[[pair[2]]]); if(nzchar(v)) cfg[[pair[1]]] <- v
      }
      if(!is.null(cfg$color) && !grepl("^#[[:xdigit:]]{6}$",cfg$color)) stop("Use un color HEX de seis dígitos.")
      for(pair in list(c("x_limits","mtt_xlim"),c("y_limits","mtt_ylim"))) {
        v <- trimws(input[[pair[2]]])
        if(nzchar(v)) {
          lim <- suppressWarnings(as.numeric(strsplit(v,",",fixed=TRUE)[[1]]))
          if(length(lim)!=2 || any(!is.finite(lim)) || diff(lim)<=0) stop("Use límites mínimo,máximo válidos.")
          if(pair[1]=="x_limits" && min(lim)<=0) stop("Las curvas requieren límites X positivos.")
          cfg[[pair[1]]] <- lim
        }
      }
      if(input$mtt_dispersion!="global") cfg$summary_measure <- input$mtt_dispersion
      for(pair in list(c("point_size","mtt_size"),c("font_size","mtt_font"))) {
        v <- trimws(input[[pair[2]]])
        if(nzchar(v)) {
          n <- suppressWarnings(as.numeric(v))
          if(length(n)!=1 || !is.finite(n) || n<=0 || n>40) stop("Use un tamaño positivo hasta 40.")
          cfg[[pair[1]]] <- n
        }
      }
      if(input$mtt_shape!="global") cfg$point_shape <- as.integer(input$mtt_shape)
      if(input$mtt_scale!="global") cfg$x_scale <- input$mtt_scale
      if(input$mtt_points!="global") cfg$show_points <- input$mtt_points=="show"
      styles <- mtt_styles()
      targets <- if(input$mtt_target=="all") LETTERS[1:8] else input$mtt_target
      for(tag in targets) styles[[tag]] <- cfg
      mtt_styles(styles)
    },error=function(e) showNotification(conditionMessage(e),type="error"))
  })
  observeEvent(input$mtt_reset,mtt_styles(list()))
  observeEvent(list(input$palette_apply,input$colors_reset),{
    styles <- mtt_styles()
    for(tag in names(styles)) styles[[tag]]$color <- NULL
    mtt_styles(styles)
  },ignoreInit=TRUE)
  editorial_module <- reactive({
    if(identical(figure_template(),"mtt4x2")) return("mtt")
    if(identical(input$module,"activity") && isTRUE(input$activity_editorial)) return("activity")
    if(identical(input$module,"safety") && identical(input$safety_display,"editorial")) return("safety")
    NULL
  })
  observeEvent(list(editorial_module(), input$module),{
    key <- editorial_module()
    dimensions <- if(!is.null(key)) ldj_editorial_dimensions(key) else
      if(identical(input$module,"safety")) c(width=10,height=8) else c(width=7,height=5)
    updateNumericInput(session,"width",value=dimensions[["width"]])
    updateNumericInput(session,"height",value=dimensions[["height"]])
  })
  output$editor_context <- renderUI({
    key <- editorial_module()
    if(is.null(key)) return(NULL)
    p(switch(key,mtt="Citotoxicidad MTT",activity="Actividad farmacológica",safety="Potencia y seguridad"))
  })
  mtt_result <- reactive({
    req(figure_template()=="mtt4x2",input$font_size,input$palette,input$dispersion)
    b <- if(isTRUE(input$mtt_user_data)) list(synthetic=FALSE) else mtt_bundle(); pal <- ldj_mtt_defaults()$palette
    color_keys <- if(isTRUE(input$mtt_user_data)) vapply(1:4,function(i) input[[paste0("mtt_series_",i)]] %||% "",character(1)) else ldj_mtt_defaults()$order
    colors <- unname(ldj_series_colors(appearance(),color_keys)[color_keys])
    if(input$palette=="Personalizada") for(i in 1:4) {
      v <- input[[paste0("custom_color_",i)]]
      if(!is.null(v) && grepl("^#[[:xdigit:]]{6}$",v)) colors[i] <- v
    }
    pal[ldj_mtt_defaults()$order] <- colors
    cfg <- list(palette=pal,figure_title=input$plot_title,x_label=input$x_label,y_label=input$y_label,
      control_label=input$control_label_default,
      font_size=input$font_size,font_family=input$font_family,show_points=input$points,
      point_shape=as.integer(input$point_shape),point_size=input$point_size,
      summary_measure=input$dispersion,x_scale=input$x_scale,panels=mtt_styles(),
      plot_width=input$width,plot_height=input$height,dpi=as.integer(input$dpi),appearance=appearance(),
      show_significance=input$mtt_significance,show_anova=input$mtt_anova_summary,show_ns=input$mtt_show_ns,
      significance_offset=input$mtt_mark_offset,significance_size=input$mtt_mark_size)
    if(isTRUE(b$synthetic)) {
      cfg$short <- stats::setNames(paste("Ejemplo",LETTERS[1:4]),ldj_mtt_defaults()$order)
      cfg$label_significance <- FALSE
      cfg$plot_caption <- paste("DEMO · datos sintéticos. Medias ±",input$dispersion,
        "; bandas locales: dispersión observada, no IC del ajuste. Sin contrastes Dunnett inventados.",
        "Seis controles físicos compartidos entre dos contextos gráficos.",input$caption)
    }
    if(isTRUE(input$mtt_user_data)) {
      validate(need(!is.null(table_result()),"Ejecute el análisis de la tabla."),
        need(identical(table_snapshot(),analysis_signature()),"Datos o configuración modificados: vuelva a ejecutar el análisis."))
      cfg$x_label <- if(identical(input$x_label,"Concentración (µM)"))
         paste0("Concentración (",if(input$mtt_display_unit=="uM") "µM" else input$mtt_display_unit,")") else input$x_label
      return(ldj_mtt_table_figure(ldj_mtt_display_unit(table_result(),input$mtt_display_unit),cfg))
    }
    ldj_mtt_editor_build(b,cfg)
  })
  current_mtt_report <- reactive({
    req(table_result(),identical(table_snapshot(),analysis_signature()))
    ldj_mtt_inference_report(table_result())
  })
  output$mtt_anova_table <- renderTable({
    report <- current_mtt_report()
    do.call(rbind,lapply(names(report),function(series) {
      z <- report[[series]]
      data.frame(Serie=series,Experimento=z$audit$experiment %||% "ND",
        F=z$anova[1,"F value"] %||% NA_real_,GL=z$anova[1,"Df"] %||% NA_real_,
        GL_residual=z$anova[2,"Df"] %||% NA_real_,p=format.pval(z$anova[1,"Pr(>F)"] %||% NA_real_,digits=4),
        Estado=z$status)
    }))
  },na="ND")
  output$mtt_dunnett_table <- renderTable({
    report <- current_mtt_report()
    do.call(rbind,lapply(names(report),function(series) {
      z <- report[[series]]
      if(!is.null(z$dunnett)) data.frame(Serie=series,Control=z$audit$control_type,
        Contraste=z$dunnett$contrast,Diferencia=z$dunnett$difference,
        p_ajustado=format.pval(z$dunnett$p_adjusted,digits=4),Significancia=z$dunnett$significance)
    }))
  },na="ND")
  output$mtt_inference_report <- renderPrint(print(current_mtt_report()))
  output$mtt_statistics <- downloadHandler(filename=function() "LDJ-Prism-MTT-estadistica.json",
    content=function(file) jsonlite::write_json(current_mtt_report(),file,pretty=TRUE,auto_unbox=TRUE,digits=NA))
  output$mtt_audit <- renderPrint({
    req(figure_template()=="mtt4x2")
    z <- mtt_result()$audit
    print(list(synthetic=z$synthetic,physical_shared_controls=z$shared_physical_n,
      visual_context_observations=z$shared_visual_n,inference=z$inference))
  })
  output$mtt_data <- renderTable({req(figure_template()=="mtt4x2");
    if(isTRUE(input$mtt_user_data)) {req(table_result()); return(head(do.call(rbind,lapply(table_result()$rows,`[[`,"data")),60))}
    head(mtt_bundle()$orig,60)})
  output$plot_container <- renderUI({
    key <- editorial_module()
    if(is.null(key)) return(plotOutput("plot",height="520px"))
    zoom <- (input$preview_zoom %||% 100)/100
    rows <- if(key=="activity") max(1,min(4,length(color_series()))) else 4L
    dimensions <- ldj_editorial_dimensions(key, rows)
    preview_width <- round(820*zoom)
    preview_max <- paste0(round(100*zoom),"%")
    tags$div(style="overflow-x:auto;",
      tags$div(style=sprintf("width:%dpx;max-width:%s;aspect-ratio:%.6f;position:relative;",
        preview_width,preview_max,dimensions[["width"]]/dimensions[["height"]]),
        plotOutput("plot",height="100%",width="100%")))
  })

  example_module <- reactiveVal(NULL)
  source_data <- reactiveVal(demo_data)
  working <- reactiveVal(demo_data)
  dataset <- reactive(working())
  # Only the explicitly synthetic bundled fixture may auto-document an example fit.
  # Imported/manual data still require an independent model justification.
  demo_reason <- "Demostración sintética: ajuste 4PL para ilustrar la interfaz; revise la adecuación del modelo antes de usar datos experimentales."
  observeEvent(list(input$module, dataset()), {
    reason <- isolate(input$model_reason) %||% ""
    bundled <- identical(dataset(), demo_data) ||
      (identical(example_module(),"activity") && identical(dataset(),editorial_example("activity")))
    if (identical(input$module, "activity") && bundled && !nzchar(trimws(reason))) {
      updateTextInput(session, "model_reason", value=demo_reason)
    } else if (!bundled && identical(reason, demo_reason)) {
      updateTextInput(session, "model_reason", value="")
    }
  },ignoreInit=TRUE)
  observations <- reactive({
    d <- dataset()
    if ("parameter" %in% names(d))
      d <- d[is.na(d$parameter) | !nzchar(trimws(d$parameter)), , drop=FALSE]
    d
  })
  grid_data <- function() {
    if(is.null(input$grid)) working() else hot_to_r(input$grid)
  }
  observeEvent(input$blank_table, {
    d <- if(identical(input$entry_type,"parameters"))
      data.frame(group="",parameter="",estimate="",lower="",upper="",unit="")
    else data.frame(concentration="",response="",group="",replicate="",control_type="",
      observation_id="",experiment_id="",control_source="",inference_eligible="",independent_unit_id="")
    source_data(d)
    working(d)
    manual(TRUE)
    draft_pending(FALSE)
    manual_snapshot(NULL)
    updateCheckboxInput(session,"safety_table",value=identical(input$entry_type,"parameters"))
    updateCheckboxInput(session,"mtt_user_data",value=TRUE)
    if(identical(input$entry_type,"parameters")) {
      updateSelectInput(session,"module",selected="safety")
      updateSelectInput(session,"safety_display",selected="summary")
    }
  })
  observeEvent(input$load_example, {
    key <- isolate(input$module)
    specialized <- key %in% c("activity","safety")
    d <- if(specialized) editorial_example(key) else demo_data
    example_module(if(specialized) key else NULL)
    source_data(d)
    working(d)
    manual(FALSE)
    draft_pending(FALSE)
    manual_snapshot(NULL)
    table_result(NULL)
    table_snapshot(NULL)
    updateSelectInput(session,"entry_type",selected="observations")
    updateSelectInput(session,"x",selected="Concentracion_uM")
    updateSelectInput(session,"y",selected="Viabilidad_pct")
    updateSelectInput(session,"group",selected="Compuesto")
    updateSelectInput(session,"unit",selected="uM")
    updateSelectInput(session,"mtt_source",selected="synthetic")
    updateCheckboxInput(session,"mtt_user_data",value=FALSE)
    updateCheckboxInput(session,"safety_table",value=identical(key,"safety"))
    if(identical(key,"safety")) {
      updateSelectInput(session,"safety_display",selected="editorial")
      updateTextInput(session,"y_label",value="Viabilidad celular (%)")
    }
    if(identical(key,"activity")) {
      updateCheckboxInput(session,"activity_editorial",value=TRUE)
      updateSelectInput(session,"activity_control_type",selected="infected_untreated")
      updateTextInput(session,"y_label",value="Células infectadas (%)")
      updateTextInput(session,"model_reason",value=demo_reason)
    }
  })
  confirm_grid <- function() {
    d <- ldj_mtt_normalize_numeric(grid_data(),input$decimal %||% ".")
    if(!nrow(d) || !ncol(d) || !any(vapply(d,function(x)
      any(!is.na(x) & nzchar(trimws(as.character(x)))),logical(1))))
      stop("Introduzca al menos un valor antes de confirmar.")
    populated <- Reduce(`|`,lapply(d,function(x) !is.na(x) & nzchar(trimws(as.character(x)))))
    d <- d[populated,,drop=FALSE]
    if("control_type"%in%names(d)) {
      missing_type <- is.na(d$control_type) | !nzchar(trimws(d$control_type))
      dose_col <- if("concentration"%in%names(d)) "concentration" else input$x
      dose <- if(length(dose_col) && dose_col%in%names(d)) d[[dose_col]] else rep(NA_real_,nrow(d))
      default_type <- ifelse(!is.na(dose) & dose==0,input$control_type_default %||% "untreated","treatment")
      d$control_type[missing_type] <- default_type[missing_type]
    }
    if("parameter"%in%names(d)) {
      rows <- !is.na(d$parameter) & nzchar(trimws(d$parameter))
      if(!any(rows)) stop("Indique un parámetro IC50, CC20 o CC50.")
      ldj_safety_results(d[rows,,drop=FALSE],"group",input$decimal %||% ".")
    }
    working(d)
    draft_pending(FALSE)
  }
  observeEvent(input$grid, {
    if(!manual()) return()
    d <- tryCatch(ldj_mtt_normalize_numeric(grid_data(),input$decimal %||% "."),
      error=function(e) NULL)
    if(!is.null(d)) {
      current <- working()
      canonical <- function(table) lapply(table,function(x) {
        x <- as.character(x)
        x[is.na(x)] <- ""
        x
      })
      draft_pending(!identical(canonical(d),canonical(current)))
    }
  },ignoreInit=TRUE)
  observeEvent(input$confirm_table, {
    tryCatch({
      confirm_grid()
      updateTabsetPanel(session,"inspector",selected="Análisis")
    },error=function(e) showNotification(conditionMessage(e),type="error"))
  })
  observeEvent(input$run_analysis, {
    if(manual() && draft_pending()) {
      showNotification("Confirme los cambios de la tabla en Datos.",type="warning")
      return()
    }
    if(identical(figure_template(),"mtt4x2") && isTRUE(input$mtt_user_data)) {
      run_mtt()
      if(!identical(table_snapshot(),analysis_signature())) return()
    }
    manual_snapshot(run_signature())
    tryCatch({
      if(identical(figure_template(),"universal") && !identical(input$module,"universal")) analysis()
      updateTabsetPanel(session,"inspector",selected="Gráfica")
    },error=function(e) {
      manual_snapshot(NULL)
      showNotification(conditionMessage(e),type="error")
    })
  })
  output$table_state <- renderText(if(manual()) "Tabla manual. Confirme los cambios antes de analizar." else
    "Tabla de sesión. Puede editarla o crear una tabla independiente.")
  output$workflow_status <- renderText(if(manual() && (draft_pending() || !identical(manual_snapshot(),run_signature())))
    "Datos o ajustes pendientes de analizar." else "La figura usa los datos confirmados.")
  output$import_controls <- renderUI({
    tagList(fileInput("upload", "Importar CSV/TSV/XLS/XLSX no confidencial", accept=c(".csv",".tsv",".xls",".xlsx")),
      selectInput("separator", "Separador CSV", c("Coma"=",","Punto y coma"=";","Tabulación"="\\t")))
  })
  observeEvent(input$upload, {
    req(data_entry_enabled)
    tryCatch({
      ext <- tolower(tools::file_ext(input$upload$name))
      if(ext %in% c("xls","xlsx")) {
        sheets <- readxl::excel_sheets(input$upload$datapath)
        updateSelectInput(session,"sheet",choices=sheets,selected=sheets[1])
      } else {
        d <- ldj_mtt_import_table(input$upload$datapath,input$upload$name,
          header=!identical(input$has_header,FALSE),
          decimal=if(is.null(input$decimal)) "." else input$decimal,
          separator=if(is.null(input$separator)) "," else if(identical(input$separator,"\\t")) "\t" else input$separator)
        source_data(d); working(d)
      }
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=NULL))
  })
  observeEvent(input$sheet, {
    req(data_entry_enabled,input$upload,
      tolower(tools::file_ext(input$upload$name)) %in% c("xls","xlsx"),input$sheet)
    tryCatch({
      d <- ldj_mtt_import_table(input$upload$datapath,input$upload$name,
        sheet=input$sheet,header=!identical(input$has_header,FALSE),
        decimal=if(is.null(input$decimal)) "." else input$decimal)
      source_data(d); working(d)
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=NULL))
  })
  observeEvent(input$apply_paste, {
    tryCatch({
      d <- ldj_mtt_paste_table(input$paste_tsv,
        header=!identical(input$has_header,FALSE),
        decimal=if(is.null(input$decimal)) "." else input$decimal)
      source_data(d); working(d)
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=NULL))
  })
  observeEvent(list(dataset(),input$x,input$y,input$group), {
    req(input$x,input$y,input$group,input$group %in% names(dataset()))
    tryCatch({
      mtt_control_map(ldj_mtt_default_control_map(observations(),input$x,input$y,input$group))
    }, error=function(e) {
      mtt_control_map(NULL)
      showNotification(paste("Registro de controles:",conditionMessage(e)),type="warning")
    })
  }, priority=95)
  output$mtt_control_grid <- renderRHandsontable({
    req(mtt_control_map())
    if(!nrow(mtt_control_map())) return(NULL)
    rhandsontable::rhandsontable(mtt_control_map(),readOnly=FALSE,rowHeaders=TRUE,
      stretchH="all",contextMenu=TRUE)
  })
  observeEvent(input$mtt_control_apply, {
    tryCatch({
      req(input$mtt_control_grid)
      map <- rhandsontable::hot_to_r(input$mtt_control_grid)
      if (!all(c("experiment_id","observation_id","series") %in% names(map)))
        stop("Conserve los encabezados del registro de controles.")
      mtt_control_map(map)
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=NULL))
  })
  output$mtt_control_status <- renderText({
    m <- mtt_control_map()
    if(is.null(m)||!nrow(m))
      "No hay asignaciones editables: incluya observation_id y experiment_id para mapear controles."
    else paste(nrow(m),"asignaciones visuales; los controles físicos permanecen únicos.")
  })
  output$grid <- renderRHandsontable({
    rhandsontable(working(), readOnly=FALSE, rowHeaders=TRUE, stretchH="all",contextMenu=TRUE,useTypes=FALSE)
  })
  observeEvent(input$apply_grid, {
    req(input$grid)
    tryCatch(confirm_grid(),error=function(e) showNotification(conditionMessage(e),type="error"))
  })
  observeEvent(input$reset_grid, working(source_data()))
  observeEvent(input$add_row, {d <- grid_data(); d[nrow(d)+1L,] <- NA; working(d)})
  observeEvent(input$add_column, {d <- grid_data(); d[[make.unique(c(names(d),"Variable"))[ncol(d)+1L]]] <- NA_real_; working(d)})
  observeEvent(input$rename_columns, {
    n <- trimws(strsplit(input$column_names,",",fixed=TRUE)[[1]])
    d <- grid_data()
    if(length(n)!=ncol(d) || any(!nzchar(n)) || anyDuplicated(n)) {
      showNotification("Indique un nombre único no vacío por columna.",type="error")
    } else {names(d) <- n; working(d)}
  })
  pending_selection <- reactiveVal(NULL)
  selection_ready <- reactive({
    req(input$x,input$y,input$group)
    p <- pending_selection()
    d <- dataset()
    if(!is.null(p)) req(input$x%in%names(d),input$y%in%names(d),
      is.numeric(d[[input$y]]),input$group==".none" || input$group%in%names(d))
    shiny::validate(need(input$x%in%names(d),"La columna X seleccionada no existe."),
      need(input$y%in%names(d),"La columna Y seleccionada no existe."),
      need(is.numeric(d[[input$y]]),"La columna Y debe ser numérica."),
      need(input$group==".none" || input$group%in%names(d),"La columna de grupo no existe."))
    TRUE
  })
  observeEvent(list(input$x,input$y,input$group), {
    p <- pending_selection()
    d <- isolate(dataset())
    if(!is.null(p) && !is.null(input$x) && !is.null(input$y) && !is.null(input$group) &&
       input$x%in%names(d) && input$y%in%names(d) && is.numeric(d[[input$y]]) &&
       (input$group==".none" || input$group%in%names(d))) pending_selection(NULL)
  },priority=90)
  physical_data <- reactive({
    req(selection_ready())
    prepare_controls(observations(),input$x,input$y,
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
    analysis_ready()
    if(is.null(input$module) || input$module=="universal") return(NULL)
    if(input$module=="safety") {
      d <- if(isTRUE(input$safety_table)) dataset() else data.frame(
        parameter=c("IC50","CC20","CC50"),estimate=c(input$ic50_value %||% NA_real_,input$cc20_value %||% NA_real_,input$cc50_value %||% NA_real_),
        unit=c(input$ic_unit,rep(input$cc_unit,2)))
      if("parameter"%in%names(d)) d <- d[!is.na(d$parameter)&nzchar(trimws(d$parameter)),,drop=FALSE]
      return(list(table=ldj_safety_results(d,input$group %||% ".none",input$decimal)))
    }
    req(selection_ready())
    reason <- trimws(input$model_reason %||% "")
    teaching_example <- identical(example_module(),"activity") && identical(dataset(),editorial_example("activity"))
    if(!nzchar(reason) || (identical(reason,demo_reason) && !identical(dataset(),demo_data) && !teaching_example))
      shiny::validate(shiny::need(FALSE,
        "Antes de ajustar la curva, abra Análisis, justifique el modelo 3PL/4PL y pulse Ejecutar análisis."))
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
  output$results <- renderTable({
    req(analysis())
    result <- analysis()$table
    if (identical(input$module, "safety")) {
      concentration <- result$parameter != "SI"
      for (column in c("estimate", "lower", "upper")) {
        values <- result[[column]]
        display <- ifelse(is.finite(values), formatC(values, format = "fg", digits = 5), NA_character_)
        small <- concentration & is.finite(values) & abs(values) < 1e-4 & values != 0
        display[small] <- formatC(values[small], format = "e", digits = 4)
        result[[column]] <- display
      }
    }
    result
  },digits=4,na="ND")
  output$analysis_status <- renderText({
    tryCatch({analysis(); if(identical(input$module,"safety")) return("Estimaciones e IC en M, SI sin unidades. Los IC se muestran únicamente si se proporcionan límites válidos."); "Resultados calculados. IC: método delta; revise extrapolación y adecuación del modelo."},
      error=function(e) conditionMessage(e))
  })
  output$anova <- renderTable({
    req(identical(input$module,"mtt"),isTRUE(input$independent))
    validate_provenance(input$experiment_id,input$control_experiment,input$control_source,input$independent)
    do.call(rbind,lapply(split(inference_data(),inference_data()$series),function(g) {
      z <- compare_control(g,input$control)$anova
      z$term <- rownames(z); z$series <- g$series[1]; z
    }))
  })
  output$diagnostics <- renderTable({
    req(identical(input$module,"mtt"),isTRUE(input$independent))
    validate_provenance(input$experiment_id,input$control_experiment,input$control_source,input$independent)
    do.call(rbind,lapply(split(inference_data(),inference_data()$series),function(g) {
      z <- compare_control(g,input$control)$diagnostics; z$series <- g$series[1]; z
    }))
  })
  output$comparisons <- renderTable({
    req(identical(input$module,"mtt"),isTRUE(input$independent))
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
    old_x <- isolate(input$x)
    if(manual() && "concentration"%in%numeric_cols && !isTRUE(old_x%in%numeric_cols))
      old_x <- "concentration"
    proposed <- list(x=choose(old_x,cols,if("concentration"%in%cols) "concentration" else "Concentracion_uM"),
      y=choose(isolate(input$y),numeric_cols,if("response"%in%numeric_cols) "response" else "Viabilidad_pct"),
      group=choose(isolate(input$group),c(".none",cols),if("group"%in%cols) "group" else "Compuesto"))
    pending_selection(proposed)
    updateSelectInput(session,"x",choices=cols,selected=proposed$x)
    updateSelectInput(session,"y",choices=numeric_cols,selected=proposed$y)
    updateSelectInput(session,"group",choices=c("Sin agrupación"=".none",stats::setNames(cols,cols)),selected=proposed$group)
    if(!length(numeric_cols) && !manual()) showNotification("La tabla no contiene columnas numéricas para Y.",type="error")
  },priority=100)

  output$custom_color_inputs <- renderUI({
    req(input$palette == "Personalizada", input$group)
    d <- dataset()
    if(identical(figure_template(),"mtt4x2")) {
      groups <- if(identical(input$mtt_source,"private")) ldj_mtt_defaults()$order else paste("Ejemplo",LETTERS[1:4])
    } else if (identical(input$group, ".none") || !(input$group %in% names(d))) {
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
        settings = values[intersect(ids, names(values))], colors = colors,appearance=appearance(),
        mtt=list(template=figure_template(),source=input$mtt_source,panels=mtt_styles()),
        analysis=list(threshold_mode=input$threshold_mode,zero_policy=input$zero_policy,model=input$model,model_reason=input$model_reason,experiment=input$experiment_id,control_experiment=input$control_experiment,control_source=input$control_source)),
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
    plot_colors <- ldj_series_colors(appearance(),levels(d$ldj_group))
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
    s <- ldj_element_style(appearance())
    p <- ldj_style_universal(p,appearance())+
      ldj_plot_theme(s,input$font_size,input$font_family,isTRUE(input$grid_lines))+
      theme(legend.position=if(grouped) input$legend_position else "none")
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
    colors <- ldj_series_colors(appearance(),groups)
    if(input$palette=="Personalizada") colors <- vapply(seq_along(groups),function(i) {
      z <- input[[paste0("custom_color_",i)]]
      if(is.null(z)) palette_ldj(length(groups),"Okabe-Ito")[i] else z
    },character(1))
    p <- p + scale_color_manual(values=setNames(colors,groups)) + facet_wrap(~.series)
    if(input$y_scale=="log10") p <- p+scale_y_log10()
    s <- ldj_element_style(appearance())
    p <- ldj_style_universal(p,appearance(),".series")+ldj_plot_theme(s,input$font_size,input$font_family)
    p
  })
  # Inferential reactive cache is independent of all presentation inputs.
  fitted_curves <- reactive({
    analysis()
    do.call(rbind,lapply(split(mapped(),mapped()$series),function(g) {
      cv <- fit_response(g,input$model,50,input$threshold_mode,input$control,input$zero_policy)$curve
      cv$series <- g$series[1]; cv
    }))
  })
  activity_result <- reactive({
    estimates <- analysis()$table
    curves <- fitted_curves()
    d <- visual_data()
    d$.series_key <- if(input$group==".none") "Datos" else as.character(d[[input$group]])
    keys <- unique(as.character(estimates$series))
    validate(need(length(keys)<=4,"El diseño editorial admite hasta cuatro series. Use la vista universal para más series."))
    teaching_example <- identical(example_module(),"activity") && identical(dataset(),editorial_example("activity"))
    experiment <- if(teaching_example) "SYNTHETIC-ACTIVITY" else input$experiment_id
    confirmed <- teaching_example || isTRUE(input$independent)
    rows <- lapply(keys,function(key) {
      z <- d[!is.na(d$.series_key)&d$.series_key==key & is.finite(d[[input$y]]),,drop=FALSE]
      inf <- ldj_activity_inference(d[!is.na(d$.series_key)&d$.series_key==key,,drop=FALSE],
        input$x,input$y,input$activity_control_type,experiment,confirmed)
      list(series=key,data=z,inference=inf,fit=list(table=estimates[estimates$series==key,,drop=FALSE],
        curve=curves[curves$series==key,,drop=FALSE]))
    })
    list(rows=rows,x=input$x,y=input$y,unit=input$unit,metric="IC50")
  })
  safety_editorial_result <- reactive({
    validate(need(isTRUE(input$safety_table),"Active Leer parámetros de la tabla."))
    req(selection_ready())
    d <- dataset()
    validate(need(input$group %in% names(d),"Seleccione la columna de grupo."),
      need(is.numeric(d[[input$x]]),"Seleccione una concentración numérica."))
    d <- d[is.na(d$parameter)|!nzchar(trimws(d$parameter)),,drop=FALSE]
    ldj_safety_editorial(analysis()$table,d,input$x,input$y,input$group,input$unit,"M",input$model, NULL)
  })
  final_plot <- reactive({
    analysis_ready()
    if(identical(figure_template(),"mtt4x2")) return(mtt_result()$figure)
    if(identical(input$module,"safety") && identical(input$safety_display,"editorial"))
      return(ldj_safety_editorial_plot(safety_editorial_result(),appearance(),input$safety_unit,input$dispersion,
        input$plot_title,y_label=input$y_label,cc20_source=input$cc20_source %||% "table",
        cc20_tolerance=input$cc20_tolerance %||% 10,
        show_fit=isTRUE(input$safety_show_fit),viability_label=input$safety_viability_label))
    if(identical(input$module,"safety")) return(ldj_safety_plot(analysis()$table,appearance(),
      input$safety_unit %||% "uM",input$plot_title,input$font_size,input$font_family,isTRUE(input$grid_lines)))
    req(selection_ready())
    if(identical(input$module,"activity") && isTRUE(input$activity_editorial)) {
      result <- activity_result()
      return(ldj_mtt_table_figure(result,list(appearance=appearance(),font_size=input$font_size,
        font_family=input$font_family,summary_measure=input$dispersion,show_points=input$points,
        point_size=input$point_size,point_shape=as.integer(input$point_shape),x_scale=input$x_scale,
        reference_control=input$activity_control_type,show_other_controls=isTRUE(input$activity_other_controls),
        show_significance=isTRUE(input$mtt_significance),show_anova=isTRUE(input$mtt_anova_summary),
        show_ns=isTRUE(input$mtt_show_ns),significance_size=input$mtt_mark_size,
        significance_offset=input$mtt_mark_offset,
        x_label=if(identical(input$x_label,"Concentración (µM)")) paste0("Concentración (",input$unit,")") else input$x_label,
        y_label=input$y_label,figure_title=input$plot_title,panels=mtt_styles(),grid=isTRUE(input$grid_lines),
        y_limits=NULL))$figure)
    }
    p <- plot_builder()
    if(!is.null(input$module) && input$module %in% c("mtt","activity") && input$kind=="line") {
      curves <- fitted_curves()
      for(key in unique(curves$series)) {
        s <- ldj_element_style(appearance(),as.character(key))
        p <- p + geom_line(data=curves[curves$series==key,,drop=FALSE],aes(x=dose,y=response),
          inherit.aes=FALSE,color=s$curve,linewidth=s$curve_linewidth %||% input$line_width,
          linetype=s$curve_linetype %||% "dashed",alpha=s$curve_alpha %||% s$alpha)
      }
      if(appearance()$preset=="editorial") {
        parameters <- analysis()$table
        parameters <- parameters[parameters$requested_target==50 & is.finite(parameters$estimate) & parameters$estimate>0,,drop=FALSE]
        descriptions <- character()
        for(key in unique(parameters$series)) {
          z <- parameters[parameters$series==key,,drop=FALSE]
          s <- ldj_element_style(appearance(),as.character(key))
          z$ldj_group <- key
          p <- p+geom_hline(data=z,aes(yintercept=target),color=s$reference,linetype="dashed",
            linewidth=s$reference_linewidth %||% .45)+
            geom_vline(data=z,aes(xintercept=estimate),color=s$reference,linetype="dashed",
              linewidth=s$reference_linewidth %||% .45)
          descriptions <- c(descriptions,paste0(key,": ",if(input$module=="activity") "IC50" else "CC50",
            " = ",signif(z$estimate,4)," ",z$unit,"; IC95 ",signif(z$lower,4),"–",signif(z$upper,4),
            if(isTRUE(z$extrapolated)) " (extrapolado)" else ""))
        }
        p <- p+labs(caption=paste(c(input$caption,descriptions),collapse="\n"))
      }
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
  analytical_registry <- reactiveVal(list())
  observeEvent(input$registry_save,{
    tryCatch({
      analysis_ready()
      module <- input$module
      assay <- trimws(input$registry_assay %||% "")
      conditions <- trimws(input$registry_conditions %||% "")
      if(!nzchar(assay) || !nzchar(conditions)) stop("Identifique el ensayo y documente las condiciones.")
      if(identical(figure_template(),"mtt4x2")) {
        if(!isTRUE(input$mtt_user_data) || is.null(table_result()) ||
           !identical(table_snapshot(),analysis_signature())) stop("Ejecute primero el análisis de la tabla MTT actual.")
        z <- table_result()
        estimates <- do.call(rbind,lapply(z$rows,function(row)
          transform(row$fit$table,series=row$series,unit=z$unit,parameter="CC50")))
        observations <- do.call(rbind,lapply(z$rows,function(row) {
          d <- row$data[!row$data$.is_control,,drop=FALSE]
          data.frame(group=row$series,concentration=ldj_convert_concentration(d[[z$x]],z$unit,"M"),
            response=d[[z$y]])
        }))
      } else {
        estimates <- analysis()$table
        if(identical(module,"mtt") && !identical(input$threshold_mode,"absolute"))
          stop("La integración editorial requiere parámetros de viabilidad absolutos.")
        estimates$parameter <- if(module=="activity") "IC50" else
          ifelse(estimates$requested_target==80,"CC20","CC50")
        observations <- if(module=="mtt") {
          d <- mapped()
          data.frame(group=d$series,concentration=ldj_convert_concentration(d$dose,input$unit,"M"),
            response=d$response)
        } else NULL
      }
      if(is.null(estimates) || !nrow(estimates) || !any(is.finite(estimates$estimate)))
        stop("No hay una estimación válida para conservar.")
      registry <- analytical_registry()
      id <- paste0(module,"-",length(registry)+1L)
      registry[[id]] <- list(module=module,assay=assay,conditions=conditions,model=input$model,
        original_unit=input$unit,source="Calculado en LDJ Prism",estimates=estimates,observations=observations)
      analytical_registry(registry)
      showNotification(paste("Guardado:",assay),type="message")
    },error=function(e) showNotification(conditionMessage(e),type="error"))
  })
  observeEvent(analytical_registry(),{
    registry <- analytical_registry()
    for(module in c("mtt","activity")) {
      items <- Filter(function(entry) identical(entry$module,module),registry)
      labels <- vapply(items,function(entry) paste(entry$assay,"·",entry$conditions),character(1))
      updateSelectInput(session,paste0("registry_",module),choices=c("Seleccionar"="",stats::setNames(names(items),labels)))
    }
  })
  observeEvent(input$registry_integrate,{
    tryCatch({
      registry <- analytical_registry()
      mtt <- registry[[input$registry_mtt %||% ""]]
      activity <- registry[[input$registry_activity %||% ""]]
      if(is.null(mtt) || is.null(activity) || !isTRUE(input$registry_confirm))
        stop("Seleccione los dos ensayos y confirme identidad y compatibilidad.")
      viable <- sort(unique(as.character(mtt$estimates$series[is.finite(mtt$estimates$estimate) & mtt$estimates$parameter=="CC50"])))
      active <- sort(unique(as.character(activity$estimates$series[is.finite(activity$estimates$estimate)])))
      if(!length(viable) || !identical(viable,active)) stop("Los identificadores de compuesto con estimaciones válidas deben coincidir en ambos ensayos.")
      result_rows <- function(entry) {
        z <- entry$estimates[entry$estimates$series %in% viable,,drop=FALSE]
        data.frame(group=z$series,concentration=NA_real_,response=NA_real_,
          parameter=z$parameter,estimate=z$estimate,lower=z$lower,upper=z$upper,unit=z$unit,
          provenance=paste(entry$source,entry$assay))
      }
      data <- mtt$observations[mtt$observations$group %in% viable,,drop=FALSE]
      data$parameter <- data$unit <- NA_character_
      data$estimate <- data$lower <- data$upper <- NA_real_
      data$provenance <- mtt$assay
      data <- rbind(data,result_rows(mtt),result_rows(activity))
      source_data(data)
      working(data)
      example_module(NULL)
      manual(FALSE)
      draft_pending(FALSE)
      manual_snapshot(NULL)
      updateSelectInput(session,"x",selected="concentration")
      updateSelectInput(session,"y",selected="response")
      updateSelectInput(session,"group",selected="group")
      updateSelectInput(session,"unit",selected="M")
      updateCheckboxInput(session,"safety_table",value=TRUE)
      updateSelectInput(session,"safety_display",selected=if(length(viable)<=4L) "editorial" else "summary")
      updateSelectInput(session,"cc20_source",selected=if(any(data$parameter=="CC20",na.rm=TRUE)) "table" else "regression")
      updateCheckboxInput(session,"registry_confirm",value=FALSE)
      updateTabsetPanel(session,"inspector",selected="Gráfica")
    },error=function(e) showNotification(conditionMessage(e),type="error"))
  })
  output$registry_export <- downloadHandler(filename=function() "LDJ-Prism-registro-analitico.json",
    content=function(file) jsonlite::write_json(analytical_registry(),file,pretty=TRUE,auto_unbox=TRUE,digits=NA))
  output$activity_audit <- renderPrint({
    req(identical(input$module,"activity"))
    d <- visual_data()
    groups <- if(input$group==".none") list(Datos=d) else split(d,d[[input$group]])
    rows <- if(isTRUE(input$activity_editorial)) activity_result()$rows else lapply(names(groups),function(key)
      list(series=key,inference=ldj_activity_inference(groups[[key]],input$x,input$y,input$activity_control_type,input$experiment_id,input$independent)))
    for(row in rows) {cat(row$series,"·",row$inference$status,"\n"); print(row$inference$audit); if(!is.null(row$inference$diagnostics)) print(row$inference$diagnostics); if(!is.null(row$inference$dunnett)) print(row$inference$dunnett)}
  })
  output$plot <- renderPlot(final_plot(),res=110)
  output$preview <- renderTable(utils::head(dataset(), 8), rownames = FALSE)
  output$status <- renderUI({
    if(identical(input$module,"safety") && !isTRUE(input$safety_table)) {
      supplied <- c(input$ic50_value,input$cc20_value,input$cc50_value)
      if(!any(is.finite(supplied) & supplied>0)) return(p(class="help-note",
        "Todavía no hay parámetros de potencia y seguridad. Introduzca IC50, CC20 y CC50 en Análisis o importe una tabla de parámetros. ND significa no disponible, no cero."))
    }
    if(identical(figure_template(),"mtt4x2")) return(p(class="help-note",
      paste("MTT · ocho paneles A–H ·",if(isTRUE(mtt_bundle()$synthetic)) "DEMO sintética" else "bundle privado",
        "· los cambios de estilo conservan estimaciones y controles.")))
    p(class = "help-note",
      paste(nrow(dataset()), "filas ·", ncol(dataset()),
            "columnas · datos", if(private_mode) "de sesión privada" else "sintéticos"))
  })
  export_plot <- function(file, format) {
    analysis_ready()
    if(identical(figure_template(),"mtt4x2")) {
      # Shiny temporary download paths may not carry a file extension.
      target <- tempfile(fileext=paste0(".",format))
      on.exit(unlink(target),add=TRUE)
      ldj_mtt_editor_export(mtt_result(),target,input$width,input$height,as.integer(input$dpi))
      file.copy(target,file,overwrite=TRUE)
      return(invisible(file))
    }
    ggplot2::ggsave(
      filename = file, plot = final_plot(), device = format,
      width = input$width, height = input$height,
      units = "in", dpi = as.integer(input$dpi), bg = appearance()$global$background
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
