# LDJ PharmaStudio · Shiny + ggplot2
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
  stopifnot(module %in% c("mtt","activity","safety"))
  if(module=="safety") {
    viability <- editorial_example("mtt")
    activity <- editorial_example("activity")
    parameters <- lapply(unique(viability$Compuesto),function(key) {
      fits <- lapply(list(CC20=list(viability,80),CC50=list(viability,50),IC50=list(activity,50)),function(spec) {
        z <- spec[[1]]
        z <- z[z$Compuesto==key & z$control_type=="treatment",]
        fit_response(data.frame(dose=z$Concentracion_uM,response=z$Viabilidad_pct),target=spec[[2]])$table
      })
      z <- do.call(rbind,fits)
      unit <- if(key=="Compuesto C") "nM" else "uM"
      data.frame(Compuesto=key,parameter=rownames(z),unit=unit,
        estimate=ldj_convert_concentration(z$estimate,"uM",unit),
        lower=ldj_convert_concentration(z$lower,"uM",unit),
        upper=ldj_convert_concentration(z$upper,"uM",unit),
        provenance=ifelse(rownames(z)=="IC50","Ajuste 4PL · actividad sintética","Ajuste 4PL · MTT sintético"))
    })
    params <- do.call(rbind,parameters)
    for(name in setdiff(names(params),names(viability))) viability[[name]] <- NA
    for(name in setdiff(names(viability),names(params))) params[[name]] <- NA
    return(rbind(viability,params[,names(viability)]))
  }
  activity <- identical(module,"activity")
  compounds <- paste("Compuesto",LETTERS[1:4])
  d <- expand.grid(Concentracion_uM=c(.005,.025,.125,.625,3.125,15.625,78.125),
    Replica=1:4,Compuesto=compounds,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  series <- match(d$Compuesto,compounds)
  midpoint <- if(activity) c(.09,.85,2.6,9)[series] else c(3.1,5.5,12,2.2)[series]
  spread <- c(1.1,1.8,2.4,1.5)[series] * (1+.35*cos(log(d$Concentracion_uM)))
  d$Viabilidad_pct <- 4+96/(1+(d$Concentracion_uM/midpoint)^1.25)+
    c(-1.35,-.4,.55,1.2)[d$Replica]*spread
  d$control_type <- "treatment"
  controls <- expand.grid(Concentracion_uM=NA_real_,Replica=1:4,Compuesto=compounds,
    control_type=if(activity) c("infected_untreated","vehicle") else "untreated",
    KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  controls$Viabilidad_pct <- 100+c(-2,-.5,.7,1.8)[controls$Replica]
  d <- rbind(d,controls[,names(d)])
  d$observation_id <- paste0(module,"-",seq_len(nrow(d)))
  d$independent_unit_id <- d$observation_id
  d$experiment_id <- paste0("SYNTHETIC-",toupper(module))
  d$control_source <- "Generador sintético LDJ"
  d$inference_eligible <- d$control_type!="vehicle"
  d
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
  title = "LDJ PharmaStudio",
  tags$head(tags$style(HTML("
    body { background: #f5f7fa; }
    .well { background: white; border: 1px solid #dde3e9; }
    .title-line { margin-bottom: 18px; }
    .help-note { color: #56616c; font-size: 12px; }
    .mtt-preview { overflow: auto; background: white; }
    @media print { .sidebar-panel, .title-line, details { display:none; }
      .main-panel { width:100%; } .mtt-preview { overflow:visible; } }
    .ldj-ratio { position:relative; }
    .ldj-ratio > .shiny-plot-output { position:absolute; inset:0; }
  "))),
  div(class = "title-line",
      h2("LDJ PharmaStudio", style = "margin-bottom: 3px;"),
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
tags$details(tags$summary("Tabla MTT, series y controles"),
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
textOutput("mtt_run_status")),
tags$details(tags$summary("Resultados inferenciales MTT"),
  h4("ANOVA MTT"),tableOutput("mtt_anova_table"),
  h4("Dunnett MTT"),tableOutput("mtt_dunnett_table"),
  verbatimTextOutput("mtt_inference_report"),
  downloadButton("mtt_statistics","Descargar resultados MTT (JSON)"))),
actionButton("run_analysis","Ejecutar análisis"),
textOutput("workflow_status"),
tags$details(tags$summary("Opciones de tabla, presentación y dosis cero"),
textInput("control_label_default", "Etiqueta del control", "No tratado"),
selectInput("control_type_default", "Tipo predeterminado de control cero",
 c("No tratado"="untreated","Vehículo"="vehicle","Control negativo"="negative",
 "Control positivo"="positive","Infectado sin tratamiento"="infected_untreated")),
selectInput("dispersion", "Dispersión", c("SD","SEM","IC95")),
helpText("Para distinguir controles en la tabla: control_type, control_label, observation_id, experiment_id, control_source e inference_eligible. Sin procedencia completa se muestran solo como referencia visual."),
selectInput("zero_policy", "Dosis cero en el ajuste", c("Excluir del ajuste; conservar control"="exclude","Incluir en ajuste"="include")),
checkboxInput("multipanel", "Figura multipanel: vista general y detalle por serie", FALSE)),
selectInput("model", "Modelo", c("4PL","3PL")),
selectInput("threshold_mode", "Definición del umbral", c("Respuesta absoluta"="absolute","Porcentaje de la media del control"="control")),
selectInput("reason_method","Justificación metodológica",
  c("Modelo 4PL con mesetas libres"="free","Respuesta normalizada"="normalized","Modelo seleccionado mediante comparación de ajustes"="comparison","Personalizada"="custom")),
conditionalPanel("input.reason_method === 'comparison'",
  actionButton("compare_models","Comparar 3PL y 4PL (AIC)"),verbatimTextOutput("model_comparison_status")),
conditionalPanel("input.reason_method === 'custom'",
  textInput("model_reason","Justificación del modelo/restricciones","")),
selectInput("unit", "Unidad de concentración", c("uM","nM","mM","M")),
tags$details(id="advanced_analysis",tags$summary("Configuración avanzada · ANOVA y Dunnett"),
  helpText("Los ajustes y análisis descriptivos están disponibles sin confirmar independencia."),
  selectizeInput("experiment_id","Identificador del experimento",choices=NULL,
    options=list(create=TRUE,placeholder="Ej. ensayo-01")),
  selectizeInput("control_experiment","Experimento del control",choices=NULL,
    options=list(create=TRUE,placeholder="Ej. ensayo-01")),
  selectInput("provenance_choice","Procedencia del control",
    c("Procedencia desconocida"="","Mismo experimento"="Mismo experimento",
      "Control físico compartido"="Control físico compartido","Experimento diferente"="Experimento diferente",
      "Personalizada"="custom")),
  conditionalPanel("input.provenance_choice === 'custom'",
    textInput("control_source","Procedencia personalizada","")),
  selectInput("unit_id_field","Columna de unidad experimental independiente",choices=c("independent_unit_id")),
  helpText("Use control_set_id para registrar explícitamente un conjunto de controles compartidos. Use una unidad física por réplica. Crear un identificador administrativo no acredita independencia ni procedencia."),
  checkboxInput("independent", "Confirmo réplicas independientes y control del mismo experimento", FALSE),
  numericInput("control", "Concentración del control", 0, min=0),
  conditionalPanel("input.module === 'mtt'",
    selectInput("mtt_inference","Análisis estadístico",c("ANOVA de una vía + Dunnett contra control"="anova","Solo análisis descriptivo"="descriptive")),
    selectInput("mtt_reference","Control de referencia",c("No tratado"="untreated","Vehículo"="vehicle","Negativo"="negative","Positivo"="positive","Infectado sin tratamiento"="infected_untreated"))),
  conditionalPanel("input.module === 'activity'",
    selectInput("activity_control_type","Control inferencial de actividad",c("Infectado sin tratamiento"="infected_untreated","Células sanas sin tratamiento"="untreated","Vehículo"="vehicle","Negativo"="negative","Positivo"="positive")))),
conditionalPanel("input.module === 'safety'",
selectInput("safety_display","Vista de potencia y seguridad",c("Editorial 4×2"="editorial","Resumen transversal"="summary")),
conditionalPanel("input.module === 'safety' && input.safety_display === 'editorial'",
  selectInput("cc20_source","Fuente de CC20",c("Calculado por regresión"="regression",
    "Proporcionado en tabla"="table","Mostrar ambas estimaciones"="both"),selected="table"),
  numericInput("cc20_tolerance","Tolerancia (% del CC20 del ajuste)",10,min=0,step=1),
  checkboxInput("safety_show_fit","Superponer la curva ajustada",FALSE),
  checkboxInput("safety_show_si","Mostrar etiqueta de SI",FALSE),
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
selectInput("figure_language","Idioma de etiquetas predeterminadas",c("Español"="es","English"="en")),
conditionalPanel("input.module !== 'universal'", uiOutput("editor_context")),
p("Las opciones gráficas no recalculan modelos ni comparaciones. La demo usa resultados calculados exclusivamente de datos sintéticos."),
selectInput("mtt_target","Aplicar estilo a",c("Todos los paneles"="all",LETTERS[1:8])),
checkboxInput("mtt_significance","Mostrar significancia",TRUE),
checkboxInput("mtt_anova_summary","Mostrar resumen ANOVA",TRUE),
selectInput("anova_position","Ubicación del resumen ANOVA",
  c("Sobre el área de trazado"="subtitle","Debajo del panel"="caption")),
selectInput("anova_format","Formato de ANOVA",c("Compacto"="compact","Decimal"="decimal","Científico"="scientific")),
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
,
  selectInput("axis_target","Aplicar ejes a",c("Todos"="all",LETTERS[1:8])),
  tags$details(open=NA,tags$summary("Eje Y"),
    selectInput("axis_y_mode","Límites Y",c("Automáticos"="auto","Manuales"="manual")),
    numericInput("axis_y_min","Mínimo Y",0),numericInput("axis_y_max","Máximo Y",120),
    numericInput("axis_y_step","Intervalo Y (0 = automático)",0,min=0),
    numericInput("axis_y_decimals","Decimales Y",0,min=0,max=8),
    numericInput("axis_y_top","Expansión superior",.08,min=0,max=1,step=.01),
    numericInput("axis_y_bottom","Expansión inferior",.01,min=0,max=1,step=.01),
    checkboxInput("axis_y_show","Mostrar etiquetas Y",TRUE),
    selectInput("axis_y_face","Grosor de etiquetas Y",c("Normal"="plain","Negrita"="bold")),
    numericInput("axis_y_size","Tamaño de etiquetas Y",9,min=4,max=30)),
  tags$details(tags$summary("Eje X"),
    selectInput("axis_x_mode","Límites X",c("Automáticos"="auto","Manuales"="manual")),
    numericInput("axis_x_min","Mínimo X",.001),numericInput("axis_x_max","Máximo X",100),
    textInput("axis_x_breaks","Divisiones X separadas por punto y coma",""),
    checkboxInput("axis_x_scientific","Notación científica X",FALSE),
    sliderInput("axis_x_angle","Rotación X",0,90,45),
    numericInput("axis_x_wrap","Caracteres por línea",12,min=5,max=60),
    textInput("axis_infected","Etiqueta gráfica del control infectado","Inf. untreated"),
    textInput("axis_vehicle","Etiqueta gráfica del vehículo","Vehículo"),
    numericInput("axis_x_size","Tamaño de etiquetas X",8,min=4,max=30),
    numericInput("axis_category_padding","Separación de bordes categóricos",.65,min=.2,max=2,step=.05)),
  numericInput("axis_tick_length","Longitud de marcas (pt)",3,min=0,max=12),
  numericInput("axis_tick_width","Grosor de marcas (mm)",.35,min=.1,max=2,step=.05),
  actionButton("axes_restore","Restaurar ejes"),
  helpText("El eje Y categórico de Potencia conserva los nombres de los parámetros. Las anotaciones de significancia se separan en el editor de paneles.")
),
tabPanel("Dimensiones y distribución",
  numericInput("width","Ancho de exportación (pulgadas)",7,min=2,max=20),
  numericInput("height","Alto de exportación (pulgadas)",5,min=2,max=20),
  selectInput("layout_row","Fila para ajustar",c("Todas"=0,1:4)),
  sliderInput("row_weight","Altura relativa de la fila y sus paneles",.6,2,1,step=.1),
  sliderInput("layout_left","Anchura de columna izquierda respecto a la derecha",.5,2,1,step=.1),
  numericInput("layout_hgap","Separación horizontal (pt)",8,min=0,max=40),
  numericInput("layout_vgap","Separación vertical (pt)",10,min=0,max=40),
  numericInput("layout_inner","Margen interno (pt)",9,min=0,max=40),
  numericInput("layout_outer","Margen externo (pt)",12,min=0,max=40),
  textOutput("layout_ratio"),
  selectInput("preview_mode","Modo de vista previa",c("Ajustar al ancho"="fit","Tamaño real"="actual")),
  sliderInput("preview_zoom","Zoom de vista previa",min=60,max=150,value=100,step=10),
  actionButton("layout_restore","Restaurar proporciones originales")),
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
      textOutput("demo_indicator"),
      conditionalPanel("input.inspector === 'Datos'",
        wellPanel(h3("1. Datos"),selectInput("entry_type","Tipo de entrada",
          c("Observaciones individuales"="observations","Parámetros resumidos"="parameters")),
          actionButton("blank_table","Nueva tabla en blanco"),
          actionButton("load_example","Cargar ejemplo del módulo"),
          actionButton("restore_example","Restaurar ejemplo"),
          actionButton("restore_project","Volver a mi tabla"),
          downloadButton("example_csv","Datos de ejemplo CSV"),
          downloadButton("example_xlsx","Datos de ejemplo XLSX"),
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
      return(if(original_example("mtt")) unique(get_example("mtt")$Compuesto) else ldj_mtt_defaults()$order)
    if(is.null(input$group)||input$group==".none"||!input$group%in%names(dataset())) return("Datos")
    z <- as.character(dataset()[[input$group]]);sort(unique(z[!is.na(z)&nzchar(z)]))
  })
  module_panel_styles <- reactiveVal(list())
  appearance <- ldj_color_server(input,output,session,color_series,module_panel_styles)
  table_result <- reactiveVal(NULL)
  table_snapshot <- reactiveVal(NULL)
  mtt_control_map <- reactiveVal(NULL)
  analysis_signature <- reactive(serialize(list(data=dataset(),control_map=mtt_control_map(),x=input$x,y=input$y,
    group=input$group,series=lapply(1:4,function(i) input[[paste0("mtt_series_",i)]]),
    unit=input$unit,model=input$model,reason=list(input$reason_method,input$model_reason),
    mtt_inference=input$mtt_inference,mtt_reference=input$mtt_reference,
    threshold=input$threshold_mode,zero=input$zero_policy,
    unit_id_field=input$unit_id_field,provenance=input$provenance_choice,control=input$control,control_type=input$control_type_default,
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
      verify_model_choice()
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
  mtt_styles <- function(value) {
    module <- input$module %||% "mtt"
    all_styles <- module_panel_styles()
    if(missing(value)) return(all_styles[[module]] %||% list())
    all_styles[[module]] <- value
    module_panel_styles(all_styles)
  }
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
  mtt_demo_inference <- reactive(ldj_mtt_demo_inference(mtt_bundle()))
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
  editorial_rows <- reactive({
    key <- editorial_module()
    if(is.null(key)) return(0L)
    if(key=="mtt") return(4L)
    max(1L,min(4L,length(color_series())))
  })
  observeEvent(editorial_rows(), {
    tags <- LETTERS[seq_len(2L*editorial_rows())]
    updateSelectInput(session,"mtt_target",choices=c("Todos los paneles"="all",tags))
  })
  output$editor_context <- renderUI({
    key <- editorial_module()
    if(is.null(key)) return(NULL)
    p(switch(key,mtt="Citotoxicidad MTT",activity="Actividad farmacológica",safety="Potencia y seguridad"))
  })
  mtt_editable_example <- reactive({
    req(original_example("mtt"))
    verify_model_choice()
    d <- get_example("mtt")
    ldj_mtt_analyze_table(d,"Concentracion_uM","Viabilidad_pct","Compuesto",unique(d$Compuesto),
      "uM",input$model,inference=identical(input$mtt_inference,"anova"),
      reference=input$mtt_reference %||% "untreated",independent=TRUE)
  })
  figure_labels <- reactive({
    english <- identical(input$figure_language,"en")
    x <- input$x_label
    y <- input$y_label
    if(english && identical(x,"Concentración (µM)")) x <- "Concentration (µM)"
    if(english && identical(y,"Viabilidad celular (%)")) y <- "Cell viability (%)"
    if(english && identical(y,"Células infectadas (%)")) y <- "Infected cells (%)"
    list(x=x,y=y,english=english)
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
    cfg <- list(palette=pal,figure_title=input$plot_title,x_label=figure_labels()$x,y_label=figure_labels()$y,
      control_label=input$control_label_default,
      font_size=input$font_size,font_family=input$font_family,show_points=input$points,
      point_shape=as.integer(input$point_shape),point_size=input$point_size,
      summary_measure=input$dispersion,x_scale=input$x_scale,panels=mtt_styles(),
      axes=axis_settings(),layout=editorial_layout(),percent=grepl("%",input$y_label %||% "",fixed=TRUE),
      plot_width=input$width,plot_height=input$height,dpi=as.integer(input$dpi),appearance=appearance(),
      show_significance=input$mtt_significance,show_anova=input$mtt_anova_summary,anova_position=input$anova_position,anova_format=input$anova_format,show_ns=input$mtt_show_ns,
      significance_offset=input$mtt_mark_offset,significance_size=input$mtt_mark_size)
    if(original_example("mtt")) {
      result <- ldj_mtt_display_unit(mtt_editable_example(),input$mtt_display_unit)
      if(identical(input$x_label,"Concentración (µM)")) {
        unit <- if(result$unit=="uM") "µM" else result$unit
        cfg$x_label <- paste0(if(figure_labels()$english) "Concentration" else "Concentración"," (",unit,")")
      }
      return(ldj_mtt_table_figure(result,cfg))
    }
    if(isTRUE(b$synthetic)) {
      cfg$short <- stats::setNames(paste("Ejemplo",LETTERS[1:4]),ldj_mtt_defaults()$order)
      cfg$label_significance <- FALSE
      if(identical(input$mtt_inference,"anova") && identical(input$mtt_reference,"untreated"))
        cfg$demo_inference <- mtt_demo_inference()
      cfg$plot_caption <- paste("DEMO · datos sintéticos. Medias ±",input$dispersion,
        "; bandas locales: dispersión observada.",
        "Seis controles físicos compartidos entre dos contextos gráficos.",input$caption)
    }
    if(isTRUE(input$mtt_user_data)) {
      validate(need(!is.null(table_result()),"Ejecute el análisis de la tabla."),
        need(identical(table_snapshot(),analysis_signature()),"Datos o configuración modificados: vuelva a ejecutar el análisis."))
      cfg$x_label <- if(identical(input$x_label,"Concentración (µM)"))
         paste0(if(figure_labels()$english) "Concentration (" else "Concentración (",
           if(input$mtt_display_unit=="uM") "µM" else input$mtt_display_unit,")") else figure_labels()$x
      return(ldj_mtt_table_figure(ldj_mtt_display_unit(table_result(),input$mtt_display_unit),cfg))
    }
    ldj_mtt_editor_build(b,cfg)
  })
  current_mtt_report <- reactive({
    req(identical(figure_template(),"mtt4x2"))
    if(original_example("mtt")) return(ldj_mtt_inference_report(mtt_editable_example()))
    if(!isTRUE(input$mtt_user_data) && identical(input$mtt_source,"synthetic") &&
       identical(input$mtt_inference,"anova") && identical(input$mtt_reference,"untreated"))
      return(mtt_demo_inference())
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
    if(original_example("mtt")) return(head(dataset(),60))
    head(mtt_bundle()$orig,60)})
  figure_size <- reactive({
    width <- input$width %||% 7
    height <- input$height %||% 5
    req(width>0,height>0)
    c(width=width,height=height)
  })
  output$layout_ratio <- renderText({
    size <- figure_size()
    rows <- max(editorial_rows(),1)
    paste(sprintf("Relación ancho/alto %.3f.",size[1]/size[2]),
      sprintf("Altura nominal por fila %.2f pulgadas.",size[2]/rows))
  })
  editorial_layout <- reactive(list(left=input$layout_left %||% 1,row=as.integer(input$layout_row %||% 0),
    row_weight=input$row_weight %||% 1,hgap=input$layout_hgap %||% 8,vgap=input$layout_vgap %||% 10,
    inner=input$layout_inner %||% 9,outer=input$layout_outer %||% 12))
  observeEvent(input$layout_restore,{
    key <- editorial_module()
    size <- if(is.null(key)) c(width=7,height=5) else ldj_editorial_dimensions(key)
    updateNumericInput(session,"width",value=size[1]);updateNumericInput(session,"height",value=size[2])
    updateSelectInput(session,"layout_row",selected="0");updateSliderInput(session,"row_weight",value=1)
    updateSliderInput(session,"layout_left",value=1);updateSliderInput(session,"preview_zoom",value=100)
    updateSelectInput(session,"preview_mode",selected="fit")
    for(id in names(c(layout_hgap=8,layout_vgap=10,layout_inner=9,layout_outer=12))) {
      defaults <- c(layout_hgap=8,layout_vgap=10,layout_inner=9,layout_outer=12)
      updateNumericInput(session,id,value=defaults[[id]])
    }
  })
  axis_settings <- reactive({
    limits <- function(mode,lo,hi) {
      if(!identical(mode,"manual")) return(NULL)
      validate(need(all(is.finite(c(lo,hi))) && lo<hi,"Indique límites de eje finitos y crecientes."))
      c(lo,hi)
    }
    raw <- trimws(input$axis_x_breaks %||% "")
    breaks <- if(nzchar(raw)) suppressWarnings(as.numeric(strsplit(raw,";",fixed=TRUE)[[1]])) else NULL
    validate(need(is.null(breaks) || all(is.finite(breaks)),"Use divisiones X numéricas separadas por punto y coma."))
    list(target=input$axis_target %||% "all",
      xlim=limits(input$axis_x_mode,input$axis_x_min,input$axis_x_max),
      ylim=limits(input$axis_y_mode,input$axis_y_min,input$axis_y_max),xbreaks=breaks,
      ylog=identical(input$y_scale,"log10"),ystep=input$axis_y_step %||% 0,
      decimals=input$axis_y_decimals %||% 0,lower_space=input$axis_y_bottom %||% .01,
      upper_space=input$axis_y_top %||% .08,show_y=!identical(input$axis_y_show,FALSE),
      yface=input$axis_y_face %||% "plain",ysize=input$axis_y_size %||% 9,
      xsize=input$axis_x_size %||% 8,angle=input$axis_x_angle %||% 45,wrap=input$axis_x_wrap %||% 12,
      infected=input$axis_infected %||% "Inf. untreated",vehicle=input$axis_vehicle %||% "Vehículo",
      category_padding=input$axis_category_padding %||% .65,scientific=isTRUE(input$axis_x_scientific),
      tick_length=input$axis_tick_length %||% 3,tick_width=input$axis_tick_width %||% .35)
  })
  observeEvent(input$axes_restore,{
    for(id in c("axis_x_mode","axis_y_mode")) updateSelectInput(session,id,selected="auto")
    updateSelectInput(session,"x_scale",selected="log10");updateSelectInput(session,"y_scale",selected="linear")
    updateSelectInput(session,"axis_target",selected="all");updateSelectInput(session,"axis_y_face",selected="plain")
    defaults <- c(axis_x_min=.001,axis_x_max=100,axis_y_min=0,axis_y_max=120,axis_y_step=0,
      axis_y_decimals=0,axis_y_top=.08,axis_y_bottom=.01,axis_y_size=9,axis_x_wrap=12,
      axis_x_size=8,axis_category_padding=.65,axis_tick_length=3,axis_tick_width=.35)
    for(id in names(defaults)) updateNumericInput(session,id,value=defaults[[id]])
    updateSliderInput(session,"axis_x_angle",value=45)
    updateTextInput(session,"axis_infected",value="Inf. untreated");updateTextInput(session,"axis_vehicle",value="Vehículo")
    updateTextInput(session,"axis_x_breaks",value="")
    updateCheckboxInput(session,"axis_y_show",value=TRUE);updateCheckboxInput(session,"axis_x_scientific",value=FALSE)
  })
  output$plot_container <- renderUI({
    if(is.null(editorial_module())) return(plotOutput("plot",height="520px"))
    size <- figure_size()
    zoom <- (input$preview_zoom %||% 100)/100
    actual <- identical(input$preview_mode,"actual")
    width <- if(actual) paste0(round(size[1]*96*zoom),"px") else paste0(round(100*zoom),"%")
    tags$div(class="ldj-ratio",style=sprintf("width:%s;min-width:%dpx;aspect-ratio:%.8f;",
      width,round(500*zoom),size[1]/size[2]),plotOutput("plot",height="100%",width="100%"))
  })

  example_module <- reactiveVal(NULL)
  source_data <- reactiveVal(demo_data)
  working <- reactiveVal(demo_data)
  dataset <- reactive(working())
  examples <- reactiveVal(list())
  get_example <- function(key) {
    if(!key %in% c("mtt","activity","safety")) return(demo_data)
    cache <- isolate(examples())
    if(is.null(cache[[key]])) {
      cache[[key]] <- editorial_example(key)
      examples(cache)
    }
    cache[[key]]
  }
  original_example <- function(key) identical(example_module(),key) &&
    !manual() && identical(dataset(),get_example(key))
  project <- reactiveVal(NULL)
  output$demo_indicator <- renderText({
    if(!is.null(example_module())) "Demostración · datos sintéticos" else
      if(!manual() && identical(dataset(),demo_data)) "Demostración · datos sintéticos" else ""
  })
  output$example_csv <- downloadHandler(filename=function() paste0("LDJ-",input$module,"-sintetico.csv"),
    content=function(file) write.csv(get_example(input$module),file,row.names=FALSE))
  output$example_xlsx <- downloadHandler(filename=function() paste0("LDJ-",input$module,"-sintetico.xlsx"),
    content=function(file) writexl::write_xlsx(list(Demostracion=get_example(input$module)),file))
  # Only the explicitly synthetic bundled fixture may auto-document an example fit.
  # Imported/manual data still require an independent model justification.
  demo_reason <- "Demostración sintética: ajuste 4PL para ilustrar la interfaz; revise la adecuación del modelo antes de usar datos experimentales."
  observeEvent(list(input$module, dataset()), {
    reason <- isolate(input$model_reason) %||% ""
    bundled <- identical(dataset(), demo_data) ||
      (original_example("activity"))
    if (identical(input$module, "activity") && bundled && !nzchar(trimws(reason))) {
      updateTextInput(session, "model_reason", value=demo_reason)
    } else if (!bundled && identical(reason, demo_reason)) {
      updateTextInput(session, "model_reason", value="")
    }
  },ignoreInit=TRUE)
  observeEvent(input$model,{
    choices <- c(if(identical(input$model,"3PL")) c("Modelo 3PL con asíntota inferior fijada en cero"="fixed") else
      c("Modelo 4PL con mesetas libres"="free"),"Respuesta normalizada"="normalized","Modelo seleccionado mediante comparación de ajustes"="comparison","Personalizada"="custom")
    updateSelectInput(session,"reason_method",choices=choices)
  })
  observeEvent(dataset(),{
    d <- dataset()
    values <- function(x) { x <- trimws(as.character(x)); unique(x[!is.na(x)&nzchar(x)]) }
    experiments <- values(d$experiment_id)
    controls <- if("control_type" %in% names(d)) which(!is.na(d$control_type)&d$control_type!="treatment") else integer()
    control_experiments <- values(d$experiment_id[controls])
    control_names <- c(untreated="No tratado",infected_untreated="Infectado sin tratamiento",
      vehicle="Vehículo",negative="Control negativo",positive="Control positivo")
    types <- intersect(names(control_names),values(d$control_type[controls]))
    physical_choices <- ldj_control_reference_choices(d)
    for(id in c("mtt_reference","activity_control_type")) {
      preferred <- if(id=="mtt_reference") "untreated" else "infected_untreated"
      previous <- isolate(input[[id]]) %||% ""
      choices <- if(length(types)) stats::setNames(types,control_names[types]) else c("Sin control físico en la tabla"="")
      choices <- c(choices,physical_choices)
      selected <- if(previous %in% unname(physical_choices)) previous else if(preferred %in% types) preferred else if(previous %in% types) previous else
        if(length(types)) types[1] else ""
      updateSelectInput(session,id,choices=choices,selected=selected)
    }
    for(id in c("experiment_id","control_experiment")) {
      options <- if(id=="experiment_id") experiments else control_experiments
      previous <- isolate(input[[id]]) %||% ""
      updateSelectizeInput(session,id,choices=unique(c("",options,previous)),
        selected=if(previous %in% options) previous else if(length(options)==1L) options else "")
    }
    updateSelectInput(session,"unit_id_field",choices=unique(c("independent_unit_id",names(d))),selected="independent_unit_id")
  })
  observations <- reactive({
    d <- dataset()
    col <- input$unit_id_field %||% "independent_unit_id"
    if(col!="independent_unit_id" && col %in% names(d)) d$independent_unit_id <- d[[col]]
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
    example_module(NULL)
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
  load_editorial_example <- function(key) {
    specialized <- key %in% c("mtt","activity","safety")
    d <- get_example(key)
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
    if(specialized) updateCheckboxInput(session,"independent",value=FALSE)
    if(identical(key,"mtt")) {
      updateSelectInput(session,"mtt_view",selected="editorial")
      updateSelectInput(session,"mtt_inference",selected="anova")
      updateSelectInput(session,"mtt_reference",selected="untreated")
      updateSelectInput(session,"threshold_mode",selected="absolute")
      updateTextInput(session,"y_label",value="Viabilidad celular (%)")
    }
    if(identical(key,"safety")) {
      updateSelectInput(session,"cc20_source",selected="table")
      updateCheckboxInput(session,"safety_show_si",value=TRUE)
      updateSelectInput(session,"safety_display",selected="editorial")
      updateTextInput(session,"y_label",value="Viabilidad celular (%)")
    }
    if(identical(key,"activity")) {
      updateCheckboxInput(session,"activity_editorial",value=TRUE)
      updateSelectInput(session,"activity_control_type",selected="infected_untreated")
      updateSelectInput(session,"threshold_mode",selected="absolute")
      updateTextInput(session,"y_label",value="Células infectadas (%)")
      updateTextInput(session,"model_reason",value=demo_reason)
    }
  }
  select_demo <- function() {
    if(is.null(example_module()) && !identical(dataset(),demo_data))
      project(list(data=dataset(),source=source_data(),manual=manual(),draft=draft_pending()))
    load_editorial_example(input$module)
  }
  observeEvent(input$load_example,select_demo())
  observeEvent(input$restore_example,select_demo())
  observeEvent(input$module,{
    key <- input$module
    if(identical(key,"safety") && identical(input$safety_display,"summary")) return()
    if(key %in% c("mtt","activity","safety") && !manual() &&
       (original_example(example_module() %||% "universal") || identical(dataset(),demo_data)))
      load_editorial_example(key)
  },ignoreInit=TRUE)
  observeEvent(input$restore_project,{
    saved <- project()
    if(is.null(saved)) return()
    example_module(NULL)
    working(saved$data)
    source_data(saved$source)
    manual(saved$manual)
    draft_pending(saved$draft)
    manual_snapshot(NULL)
    updateCheckboxInput(session,"mtt_user_data",value=TRUE)
    updateCheckboxInput(session,"safety_table",value="parameter" %in% names(saved$data))
  })
  grid_values <- function(table) lapply(table,function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  mark_edited_example <- function() {
    if(manual()) return()
    manual(TRUE)
    manual_snapshot(NULL)
    updateCheckboxInput(session,"independent",value=FALSE)
    if(identical(input$module,"mtt")) updateCheckboxInput(session,"mtt_user_data",value=TRUE)
  }
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
    if(!is.null(example_module()) && !identical(grid_values(d),grid_values(working())))
      mark_edited_example()
    working(d)
    draft_pending(FALSE)
  }
  observeEvent(input$grid, {
    if(!manual() && is.null(example_module())) return()
    changes <- input$grid$changes
    if(identical(changes$event,"afterChange") && is.null(changes$changes)) return()
    if(is.null(changes$event)) return()
    d <- tryCatch(ldj_mtt_normalize_numeric(grid_data(),input$decimal %||% "."),
      error=function(e) NULL)
    changed <- is.null(d) || !identical(grid_values(d),grid_values(working()))
    if(changed) mark_edited_example()
    draft_pending(changed)
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
        example_module(NULL); source_data(d); working(d)
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
      example_module(NULL); source_data(d); working(d)
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=NULL))
  })
  observeEvent(input$apply_paste, {
    tryCatch({
      d <- ldj_mtt_paste_table(input$paste_tsv,
        header=!identical(input$has_header,FALSE),
        decimal=if(is.null(input$decimal)) "." else input$decimal)
      example_module(NULL); source_data(d); working(d)
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
    rhandsontable(working(), readOnly=FALSE, rowHeaders=TRUE, stretchH="all",contextMenu=TRUE,useTypes=FALSE,
      height=520)
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
  declared_provenance <- reactive({
    choice <- input$provenance_choice %||% ""
    if(identical(choice,"custom")) return(input$control_source %||% "")
    if(!nzchar(choice)) return(input$control_source %||% "")
    if(identical(choice,"Experimento diferente")) return("")
    choice
  })
  inference_data <- reactive({
    req(selection_ready())
    d <- visual_data()
    controls <- d[d$.is_control,,drop=FALSE]
    if(!nrow(controls) || any(!controls$.eligible)) stop("Seleccione controles elegibles con procedencia completa.")
    if(any(!is.finite(controls[[input$x]]))) stop("Control con concentración no informada: la comparación por dosis requiere mapeo explícito.")
    map_data(d,input$x,input$y,input$group,input$decimal)
  })

  comparison_fit <- reactiveVal(NULL)
  comparison_signature <- reactive(list(data=dataset(),x=input$x,y=input$y,group=input$group,
    unit=input$unit,threshold=input$threshold_mode,zero=input$zero_policy,control=input$control))
  observeEvent(input$compare_models,{
    tryCatch({
      d <- mapped()
      fits <- lapply(split(d,d$series),function(series) {
        scores <- vapply(c("3PL","4PL"),function(model) {
          fit <- fit_response(series,model,50,input$threshold_mode,input$control,input$zero_policy)
          as.numeric(stats::AIC(fit$fit))
        },numeric(1))
        if(any(!is.finite(scores))) stop("AIC no estimable para ambos modelos.")
        scores
      })
      comparison_fit(list(signature=comparison_signature(),scores=do.call(rbind,fits)))
    },error=function(e) {
      comparison_fit(NULL)
      showNotification(conditionMessage(e),type="error")
    })
  })
  comparison_current <- reactive(!is.null(comparison_fit()) &&
    identical(comparison_fit()$signature,comparison_signature()))
  output$model_comparison_status <- renderPrint({
    if(comparison_current()) print(comparison_fit()$scores) else
      cat("Ejecute una comparación con los datos y parámetros vigentes.")
  })
  verify_model_choice <- function() {
    if(identical(input$reason_method,"comparison"))
      validate(need(comparison_current(),"Ejecute primero la comparación 3PL y 4PL."))
    if(identical(input$reason_method,"custom"))
      validate(need(nzchar(trimws(input$model_reason %||% "")),"Indique la justificación personalizada."))
  }
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
    verify_model_choice()
    reason <- if(identical(input$reason_method,"custom")) trimws(input$model_reason %||% "") else
      as.character(input$reason_method %||% "free")
    teaching_example <- original_example("activity")
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
    validate_provenance(input$experiment_id,input$control_experiment,declared_provenance(),input$independent)
    do.call(rbind,lapply(split(inference_data(),inference_data()$series),function(g) {
      z <- compare_control(g,input$control)$anova
      z$term <- rownames(z); z$series <- g$series[1]; z
    }))
  })
  output$diagnostics <- renderTable({
    req(identical(input$module,"mtt"),isTRUE(input$independent))
    validate_provenance(input$experiment_id,input$control_experiment,declared_provenance(),input$independent)
    do.call(rbind,lapply(split(inference_data(),inference_data()$series),function(g) {
      z <- compare_control(g,input$control)$diagnostics; z$series <- g$series[1]; z
    }))
  })
  output$comparisons <- renderTable({
    req(identical(input$module,"mtt"),isTRUE(input$independent))
    validate_provenance(input$experiment_id,input$control_experiment,declared_provenance(),input$independent)
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
        analysis=list(threshold_mode=input$threshold_mode,zero_policy=input$zero_policy,model=input$model,model_reason=input$model_reason,reason_method=input$reason_method,experiment=input$experiment_id,control_experiment=input$control_experiment,control_source=input$control_source)),
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
    teaching_example <- original_example("activity")
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
        input$plot_title,y_label=figure_labels()$y,cc20_source=input$cc20_source %||% "table",
        cc20_tolerance=input$cc20_tolerance %||% 10,
        show_fit=isTRUE(input$safety_show_fit),
        viability_label=if(figure_labels()$english && identical(input$safety_viability_label,"80–100% viable"))
          "80–100% viable cells" else input$safety_viability_label,english=figure_labels()$english,
        panel_styles=mtt_styles(),font_size=input$font_size,font_family=input$font_family,show_si=isTRUE(input$safety_show_si),
        axes=axis_settings(),layout=editorial_layout(),x_scale=input$x_scale))
    if(identical(input$module,"safety")) return(ldj_safety_plot(analysis()$table,appearance(),
      input$safety_unit %||% "uM",input$plot_title,input$font_size,input$font_family,isTRUE(input$grid_lines)))
    req(selection_ready())
    if(identical(input$module,"activity") && isTRUE(input$activity_editorial)) {
      result <- activity_result()
      return(ldj_mtt_table_figure(result,list(appearance=appearance(),font_size=input$font_size,
        font_family=input$font_family,summary_measure=input$dispersion,show_points=input$points,
        point_size=input$point_size,point_shape=as.integer(input$point_shape),x_scale=input$x_scale,
        reference_control=input$activity_control_type,show_other_controls=isTRUE(input$activity_other_controls),
        show_significance=isTRUE(input$mtt_significance),show_anova=isTRUE(input$mtt_anova_summary),anova_position=input$anova_position,anova_format=input$anova_format,
        show_ns=isTRUE(input$mtt_show_ns),significance_size=input$mtt_mark_size,
        significance_offset=input$mtt_mark_offset,
        x_label=if(identical(input$x_label,"Concentración (µM)"))
          paste0(if(figure_labels()$english) "Concentration (" else "Concentración (",
            if(input$unit=="uM") "µM" else input$unit,")") else figure_labels()$x,
        y_label=figure_labels()$y,figure_title=input$plot_title,panels=mtt_styles(),grid=isTRUE(input$grid_lines),
        axes=axis_settings(),layout=editorial_layout(),percent=grepl("%",input$y_label %||% "",fixed=TRUE),
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
      updateSelectInput(session,paste0("registry_",module),choices=c("Seleccionar"="",stats::setNames(as.character(names(items)),labels)))
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
