# Display labels only. Physical dose keys stay numeric and unchanged.
ldj_synergy_dose_labels <- function(doses, digits = 3L, notation = "significant") {
  stopifnot(is.numeric(doses), all(is.finite(doses)), digits %in% 1:15)
  encode <- function(precision) switch(notation,
    decimal = formatC(doses, format = "f", digits = precision),
    scientific = formatC(doses, format = "e", digits = precision - 1L),
    significant = format(signif(doses, precision), trim = TRUE, scientific = NA, digits = precision))
  labels <- encode(digits)
  distinct <- !duplicated(doses)
  for (precision in seq.int(digits, 15L)) {
    labels <- encode(precision)
    if (!anyDuplicated(labels[distinct]) &&
        !any(doses != 0 & suppressWarnings(as.numeric(labels)) == 0, na.rm = TRUE)) return(labels)
  }
  sprintf("%.17g", doses)
}

ldj_synergy_figure_dimensions <- function(n, columns) {
  if (n == 1L) return(c(width = 7, height = 5))
  if (n == 6L && columns == 2L) return(c(width = 10, height = 8))
  if (n == 12L && columns == 2L) return(c(width = 12, height = 17))
  columns <- min(columns, n)
  c(width = max(7, 4.2 * columns), height = max(5, 2.5 * ceiling(n / columns) + 0.6))
}

ldj_synergy_demo <- function(size=7L,scenario="generic") {
  stopifnot(size %in% 5:12)
  doses <- c(0,2^seq(-4,2,length.out=size-1L))
  d <- expand.grid(dose_A=doses,dose_B=doses,biological_replicate_id=paste0("u",1:3),
    KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  scale <- if(scenario=="second") .6 else 1
  a <- .8*d$dose_A/(d$dose_A+scale)
  b <- .7*d$dose_B/(d$dose_B+.6)
  interaction <- (d$dose_A>0 & d$dose_B>0) * (16*exp(-abs(log1p(d$dose_A)-.5)-abs(log1p(d$dose_B)-.6)) -
    14*exp(-abs(log1p(d$dose_A)-1.8)-abs(log1p(d$dose_B)-.1)))
  d$readout_value <- 100*(a+b-a*b)+interaction
  positive <- d$dose_A>0 | d$dose_B>0
  d$readout_value[positive] <- pmin(100,pmax(0,d$readout_value[positive]+
    c(-1,0,1)[match(d$biological_replicate_id[positive],paste0("u",1:3))]))
  d$compound_A <- "Compuesto A"; d$compound_B <- "Compuesto B"
  d$unit_A <- d$unit_B <- "uM"
  d$readout_type <- "percent_inhibition"
  d$experiment_id <- paste0("SIM-",d$biological_replicate_id)
  d$observation_id <- sprintf("SIM-%04d",seq_len(nrow(d)))
  d
}

ldj_synergy_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::h3("Synergy Studio"),
    shiny::textOutput(ns("provenance")),
    shiny::tabsetPanel(id=ns("views"),selected="Clasificación",
      shiny::tabPanel("Datos",
        shiny::numericInput(ns("size"),"Tamaño de la matriz, incluido cero",7,min=5,max=12,step=1),
        shiny::selectInput(ns("scenario"),"Escenario simulado",c("A"="generic","B"="second")),
        shiny::actionButton(ns("example"),"Restaurar simulación"),
        shiny::fileInput(ns("file"),"Tabla larga CSV, TSV o Excel",accept=c(".csv",".tsv",".xls",".xlsx")),
        shiny::selectInput(ns("sheet"),"Hoja",choices=NULL),
        rhandsontable::rHandsontableOutput(ns("grid")),
        shiny::actionButton(ns("calculate"),"Calcular combinaciones")),
      shiny::tabPanel("Clasificación",
        shiny::selectInput(ns("template"),"Plantilla editorial",c("Vista por unidad"="unit",
          "Clasificación y eficacia · 6 paneles"="class","Integral · 12 paneles"="integral", "Beneficio · 6 paneles"="benefit",
          "Synergy Landscape"="landscape", "Model Concordance"="concordance",
          "Experimental Reproducibility"="reproducibility", "Dose Optimization"="optimization",
          "Composición personalizada"="custom")),
        shiny::textInput(ns("title"),"Título de figura",""),
        shiny::selectInput(ns("model"),"Referencia descriptiva",c("Bliss","HSA","Loewe")),
        shiny::selectInput(ns("agreement_models"), "Concordancia descriptiva",
          c("HSA frente a Bliss" = "HSA/Bliss", "HSA frente a Loewe" = "HSA/Loewe",
            "Bliss frente a Loewe" = "Bliss/Loewe", "Tres modelos · dominio común" = "HSA/Bliss/Loewe")),
        shiny::numericInput(ns("cutoff"),"Umbral descriptivo en puntos porcentuales",10,min=.1,max=100),
        shiny::selectInput(ns("unit"),"Unidad de presentación",c("uM","nM","mM","M")),
        shiny::helpText("La clasificación describe un exceso frente a la referencia. Loewe nativo es descriptivo. Su inferencia permanece ND. ZIP permanece experimental."),
        shiny::tags$details(shiny::tags$summary("Figure Studio"),
          shiny::conditionalPanel("input.template === 'custom'", ns = ns,
            shiny::selectizeInput(ns("gallery_types"), "Gráficos en orden (máximo 12)",
              stats::setNames(names(ldj_synergy_gallery()), vapply(ldj_synergy_gallery(), `[[`, "", 1L)),
              selected = c("observed", "bubble", "conditional_a", "dumbbell"), multiple = TRUE,
              options = list(maxItems = 12)),
            shiny::textInput(ns("gallery_sequence"), "Secuencia de identificadores (permite duplicados)", "",
              placeholder = "observed@mapa,bubble@serie1,bubble@serie2"),
            shiny::textInput(ns("gallery_layout"), "Matriz de diseño opcional", "",
              placeholder = "AB/CC/DD"),
            shiny::textInput(ns("gallery_widths"), "Anchuras relativas opcionales", "", placeholder = "1,2"),
            shiny::textInput(ns("gallery_heights"), "Alturas relativas opcionales", "", placeholder = "1,1,2")),
          shiny::selectInput(ns("gallery_comparator"), "Comparador del dumbbell",
            c("Mejor monoterapia" = "best", "Monoterapia A" = "a", "Monoterapia B" = "b")),
          shiny::numericInput(ns("gallery_pairs"), "Pares en el dumbbell", 8, min = 1, max = 24),
          shiny::selectInput(ns("gallery_surface_z"), "Superficie · variable Z",
            c("Exceso (pp)" = "score", "Observada (%)" = "observed", "Esperada (%)" = "expected")),
          shiny::numericInput(ns("gallery_surface_theta"), "Superficie · rotación (°)", 38, min = -180, max = 180),
          shiny::numericInput(ns("gallery_surface_perspective"), "Superficie · perspectiva (0 = ortográfica)", 0, min = 0, max = .55, step = .05),
          shiny::numericInput(ns("gallery_surface_vertical"), "Superficie · altura relativa", .6, min = .2, max = 1.1, step = .05),
          shiny::numericInput(ns("gallery_surface_mesh"), "Superficie · grosor de malla", 0.25, min = 0.05, max = 2, step = 0.05),
          shiny::numericInput(ns("gallery_surface_alpha"), "Superficie · opacidad", 0.75, min = 0.15, max = 1, step = 0.05),
          shiny::numericInput(ns("gallery_surface_points"), "Superficie · tamaño de puntos", 1, min = 0.3, max = 5, step = 0.1),
          shiny::numericInput(ns("gallery_surface_labels"), "Superficie · tamaño de graduaciones", 3.2, min = 2, max = 6, step = 0.1),
          shiny::numericInput(ns("gallery_marginal_top"), "Mapa marginal · altura del perfil A", 1, min = 0.5, max = 3, step = 0.1),
          shiny::numericInput(ns("gallery_marginal_right"), "Mapa marginal · anchura del perfil B", 1, min = 0.5, max = 3, step = 0.1),
          shiny::numericInput(ns("gallery_marginal_points"), "Mapa marginal · tamaño de puntos", 1.7, min = 0.4, max = 5, step = 0.1),
          shiny::numericInput(ns("gallery_density_height"), "Densidades · altura relativa", 0.72, min = 0.15, max = 0.9, step = 0.05),
          shiny::numericInput(ns("gallery_density_points"), "Densidades · tamaño de puntos", 1.8, min = 0.4, max = 5, step = 0.1),
          shiny::numericInput(ns("gallery_density_alpha"), "Densidades · opacidad de puntos", 0.75, min = 0.15, max = 1, step = 0.05),
          shiny::numericInput(ns("gallery_surface_phi"), "Superficie · elevación (°)", 28, min = 5, max = 85),
          shiny::numericInput(ns("gallery_density_adjust"), "Densidades · multiplicador del ancho de banda", 1, min = .2, max = 4, step = .1),
          shiny::uiOutput(ns("gallery_context_ui")),
          shiny::selectInput(ns("gallery_marginal_field"), "Mapa marginal · variable",
            c("Inhibición observada (%)" = "observed", "Exceso (pp)" = "score")),
          shiny::numericInput(ns("gallery_isobole_window"), "Isobola · tolerancia de observaciones (pp)", 10, min = 0, max = 50),
          shiny::numericInput(ns("gallery_isobole_effect"), "Efecto de la isobola (%)", 50, min = 1, max = 99),
          shiny::checkboxInput(ns("gallery_values"), "Etiquetas numéricas en los mapas", FALSE),
          shiny::numericInput(ns("figure_columns"), "Columnas", 2, min = 1, max = 6, step = 1),
          shiny::selectInput(ns("size_preset"), "Dimensiones",
            c("Adaptar a los paneles" = "auto", "Una columna · ancho inicial 3.5 in" = "one",
              "Dos columnas · ancho inicial 7 in" = "two", "Página · ancho inicial 8.3 in" = "page",
              "Personalizadas" = "custom")),
          shiny::selectInput(ns("size_units"), "Unidad de dimensiones", c("in", "cm", "mm")),
          shiny::numericInput(ns("size_width"), "Ancho personalizado", 7, min = 0.1),
          shiny::numericInput(ns("size_height"), "Alto personalizado", 5, min = 0.1),
          shiny::textOutput(ns("size_summary")),
          shiny::selectInput(ns("dose_notation"), "Formato de dosis",
            c("Cifras significativas" = "significant", "Decimales" = "decimal", "Científico" = "scientific")),
          shiny::numericInput(ns("dose_digits"), "Cifras / decimales (mínimo)", 3, min = 1, max = 15, step = 1),
          shiny::numericInput(ns("dose_rotation"), "Rotación de etiquetas X (°)", 35, min = 0, max = 90, step = 5),
          shiny::numericInput(ns("dose_ticks"), "Máximo de etiquetas por eje", 6, min = 2, max = 30, step = 1),
          shiny::numericInput(ns("heat_ticks_y"), "Máximo de etiquetas Y en mapas", 6, min = 2, max = 30, step = 1),
          shiny::numericInput(ns("heat_text_x"), "Tamaño de etiquetas X en mapas (pt)", 8, min = 5, max = 18),
          shiny::numericInput(ns("heat_text_y"), "Tamaño de etiquetas Y en mapas (pt)", 8, min = 5, max = 18),
          shiny::numericInput(ns("heat_aspect"), "Proporción alto / ancho del mapa (0 = flexible)", 0, min = 0, max = 3, step = .05),
          shiny::checkboxInput(ns("heat_square"), "Celdas cuadradas", FALSE),
          shiny::selectInput(ns("class_scope"), "Clasificación",
            c("Por unidad biológica" = "unit", "Por experimento" = "experiment",
              "Experimento seleccionado" = "selected", "Global" = "global")),
          shiny::uiOutput(ns("class_experiment_ui")),
          shiny::numericInput(ns("class_columns"), "Columnas de la clasificación (0 = automático)", 0, min = 0, max = 8, step = 1),
          shiny::numericInput(ns("class_spacing"), "Separación de facetas (pt)", 5, min = 0, max = 25, step = 1),
          shiny::textInput(ns("panel_order"), "Paneles visibles en orden (letras separadas por comas)", ""),
          shiny::selectInput(ns("panel_export"), "Exportar", c("Figura completa" = "", stats::setNames(LETTERS[1:12], LETTERS[1:12]))),
          shiny::selectInput(ns("edit_panel"), "Panel para editar", LETTERS[1:12]),
          shiny::textInput(ns("panel_title"), "Título del panel", ""),
          shiny::textInput(ns("panel_subtitle"), "Subtítulo", ""),
          shiny::textInput(ns("panel_tag"), "Identificador", ""),
          shiny::textInput(ns("contrast_a_label"), "Etiqueta de contraste 1", "vs. monoterapia A"),
          shiny::textInput(ns("contrast_b_label"), "Etiqueta de contraste 2", "vs. monoterapia B"),
          shiny::textInput(ns("contrast_best_label"), "Etiqueta de contraste 3", "vs. mejor monoterapia"),
          shiny::textInput(ns("panel_x"), "Etiqueta X", ""),
          shiny::textInput(ns("panel_y"), "Etiqueta Y", ""),
          shiny::selectInput(ns("panel_legend"), "Leyenda del panel",
            c("Heredar" = "", "Abajo" = "bottom", "Derecha" = "right", "Oculta" = "none")),
          shiny::numericInput(ns("panel_font"), "Tipografía del panel (pt, 0 = heredar)", 0, min = 0, max = 24),
          shiny::selectInput(ns("panel_family"), "Familia tipográfica", c("Heredar" = "", "Sans" = "sans", "Serif" = "serif", "Monoespaciada" = "mono")),
          shiny::numericInput(ns("panel_gap"), "Separación entre facetas (pt, −1 = heredar)", -1, min = -1, max = 24),
          shiny::numericInput(ns("panel_ticks_x"), "Graduaciones X (0 = heredar)", 0, min = 0, max = 12),
          shiny::numericInput(ns("panel_ticks_y"), "Graduaciones Y (0 = heredar)", 0, min = 0, max = 12),
          shiny::numericInput(ns("panel_min_width"), "Anchura mínima por faceta (in, 0 = flexible)", 0, min = 0, max = 4, step = .1),
          shiny::numericInput(ns("panel_min_height"), "Altura mínima por faceta (in, 0 = flexible)", 0, min = 0, max = 4, step = .1),
          shiny::numericInput(ns("panel_margin"), "Margen del panel (pt, 0 = automático)", 0, min = 0, max = 24),
          shiny::actionButton(ns("save_panel"), "Aplicar al panel"),
          shiny::numericInput(ns("forest_point"), "Símbolo individual del forest plot (mm)", 2, min = .5, max = 6, step = .25),
          shiny::numericInput(ns("forest_global"), "Símbolo global del forest plot (mm)", 3, min = .5, max = 6, step = .25),
          shiny::numericInput(ns("forest_interval"), "Grosor del IC95% del forest plot (mm)", .5, min = .1, max = 3, step = .1),
          shiny::textInput(ns("benefit_legend"), "Título de leyenda de beneficio", "Condición experimental"),
          shiny::fileInput(ns("figure_json"), "Restaurar composición JSON", accept = ".json"),
          shiny::downloadButton(ns("figure_json_out"), "Guardar composición JSON")),
        shiny::tags$style(shiny::HTML("
          .ldj-preview { overflow:auto; min-width:0; max-height:78vh; background:#eef1f5; padding:8px; }
          .ldj-preview:fullscreen { max-height:none; width:100%; height:100%; }
          .ldj-preview .shiny-plot-output { width:max-content !important; }
          .ldj-preview img { display:block; max-width:none !important; height:auto !important; }
          .ldj-table-scroll { width:100%; max-width:100%; overflow:auto; max-height:60vh; }
          .ldj-preview-controls { display:flex; flex-wrap:wrap; align-items:center; gap:.7rem; }
          .ldj-preview-controls .form-group { margin-bottom:0; }
          .ldj-preview-controls select { max-width:100%; }
        ")),
        shiny::div(class="ldj-preview-controls",
          shiny::selectInput(ns("preview_panel"), "Vista previa", c("Composición" = "", stats::setNames(LETTERS[1:12], LETTERS[1:12]))),
          shiny::tags$label("Escala", shiny::tags$select(
            onchange="this.closest('.ldj-preview-controls').nextElementSibling.dataset.zoom=this.value",
            shiny::tags$option(value="fit", "Ajustar al ancho"),
            shiny::tags$option(value="100", "Tamaño real · 100%"),
            lapply(c(25, 50, 75, 125, 150, 175, 200), function(x) shiny::tags$option(value=x, paste0(x, "%"))))),
          shiny::tags$button(type="button", class="btn btn-default",
            onclick="const v=this.parentElement.nextElementSibling; if(document.fullscreenElement){document.exitFullscreen()}else if(v.requestFullscreen){v.requestFullscreen()}",
            "Pantalla completa")),
        shiny::div(class="ldj-preview", tabindex="0", role="region", `aria-label`="Vista previa desplazable",
          `data-zoom`="fit", shiny::plotOutput(ns("plot"), height="auto")),
        shiny::tags$script(shiny::HTML("
          document.querySelectorAll('.ldj-preview').forEach(function(view){
            if(view.dataset.ready) return;
            view.dataset.ready='1';
            function scale(){
              const img=view.querySelector('img');
              if(!img || !img.naturalWidth) return;
              const zoom=view.dataset.zoom;
              const width=zoom==='fit' ? Math.min(img.naturalWidth,Math.max(1,view.clientWidth-16)) :
                img.naturalWidth*Number(zoom)/100;
              img.style.width=width+'px';
            }
            new ResizeObserver(scale).observe(view);
            new MutationObserver(scale).observe(view,{childList:true,subtree:true,attributes:true,attributeFilter:['data-zoom','src']});
            view.addEventListener('load',scale,true);
          });
        ")),
        shiny::downloadButton(ns("csv"),"Resultados por experimento"),
        shiny::checkboxInput(ns("independence"),"Confirmo que los experimentos son independientes",FALSE),
        shiny::helpText("Contrastes unilaterales a dosis equivalentes, IC95% bilaterales y ajuste Holm por análisis. Se exige un mínimo de tres experimentos con variación entre ellos."),
        shiny::downloadButton(ns("inference_csv"),"Contrastes e incertidumbre CSV"),
        shiny::div(class = "ldj-table-scroll", tabindex = "0", shiny::tableOutput(ns("interaction_evidence"))),
        shiny::downloadButton(ns("svg"),"SVG"),
        shiny::downloadButton(ns("pdf"),"PDF"),
        shiny::downloadButton(ns("png"),"PNG 600 dpi"),
        shiny::downloadButton(ns("png300"),"PNG 300 dpi"),
        shiny::tags$details(shiny::tags$summary("Modelos experimentales"),
          shiny::selectInput(ns("experimental_model"), "Modelo en evaluación",
            c("Ninguno" = "", "Loewe continuo nativo" = "Loewe", "ZIP · SynergyFinder 3.20.0" = "ZIP")),
          shiny::helpText("Resultados provisionales por unidad biológica. Cobertura y exceso corresponden a las celdas evaluables."),
          shiny::div(class = "ldj-table-scroll", tabindex = "0", shiny::tableOutput(ns("experimental_summary"))),
          shiny::plotOutput(ns("experimental_plot")),
          shiny::downloadButton(ns("experimental_csv"), "Descargar celdas y diagnósticos CSV"))),
      shiny::tabPanel("Incertidumbre",
        shiny::checkboxInput(ns("bootstrap_independent"),
          "Confirmo que los experimentos incluidos son independientes", FALSE),
        shiny::selectInput(ns("bootstrap_model"), "Modelo", c("HSA", "Bliss")),
        shiny::selectInput(ns("bootstrap_iterations"), "Iteraciones", c(1000, 5000, 10000)),
        shiny::numericInput(ns("bootstrap_seed"), "Semilla", 1, min = 0, max = 2147483647, step = 1),
        shiny::textOutput(ns("bootstrap_summary")),
        shiny::div(class = "ldj-table-scroll", tabindex = "0", shiny::tableOutput(ns("bootstrap_scores"))),
        shiny::plotOutput(ns("bootstrap_plot")),
        shiny::div(class = "ldj-table-scroll", tabindex = "0", shiny::tableOutput(ns("bootstrap_loo"))),
        shiny::downloadButton(ns("bootstrap_csv"), "Descargar incertidumbre CSV")),
      shiny::tabPanel("Beneficio farmacológico",
        shiny::helpText("Use una fila por par de dosis y contexto experimental. Los umbrales in vitro no establecen seguridad clínica."),
        shiny::actionButton(ns("benefit_demo"), "Cargar ejemplo sintético"),
        shiny::textOutput(ns("benefit_source")),
        shiny::fluidRow(
          shiny::column(6, shiny::fileInput(ns("activity_file"), "Actividad CSV, TSV o Excel"),
            shiny::uiOutput(ns("activity_sheet_ui")),
            rhandsontable::rHandsontableOutput(ns("activity_grid"))),
          shiny::column(6, shiny::fileInput(ns("viability_file"), "Viabilidad CSV, TSV o Excel"),
            shiny::uiOutput(ns("viability_sheet_ui")),
            rhandsontable::rHandsontableOutput(ns("viability_grid")))),
        shiny::numericInput(ns("activity_min"), "Actividad mínima (%)", 70, min = 0, max = 100),
        shiny::numericInput(ns("viability_min"), "Viabilidad mínima (%)", 80, min = 0, max = 100),
        shiny::selectInput(ns("benefit_sort"), "Ordenar por",
          c("Eficacia" = "activity_pct", "Viabilidad" = "viability_pct", "Concentración" = "dose")),
        shiny::textOutput(ns("benefit_status")),
        shiny::plotOutput(ns("benefit_plot"), height = "700px"),
        shiny::div(class = "ldj-table-scroll", tabindex = "0", shiny::tableOutput(ns("benefit_hits"))),
        shiny::downloadButton(ns("benefit_csv"), "Descargar combinaciones elegibles CSV"))))

}

ldj_synergy_server <- function(id,appearance,figure_size) {
  shiny::moduleServer(id,function(input,output,session) {
    data <- shiny::reactiveVal(ldj_synergy_demo())
    simulated <- shiny::reactiveVal(TRUE)
    results <- shiny::reactiveVal(ldj_synergy_excess(shiny::isolate(data())))
    stale <- shiny::reactiveVal(FALSE)
    make_demo <- function(size,scenario) {
      d <- ldj_synergy_demo(size,scenario)
      z <- ldj_synergy_excess(d)
      data(d); results(z); simulated(TRUE); stale(FALSE)
    }
    shiny::observeEvent(input$example,make_demo(input$size,input$scenario))
    read_table <- function(sheet=NULL) {
      tryCatch({
        file <- input$file
        d <- ldj_mtt_import_table(file$datapath,file$name,sheet=sheet)
        data(d); simulated(FALSE); stale(TRUE)
      },error=function(e) shiny::showNotification(conditionMessage(e),type="error"))
    }
    shiny::observeEvent(input$file,{
      if(tolower(tools::file_ext(input$file$name)) %in% c("xls","xlsx")) {
        sheets <- readxl::excel_sheets(input$file$datapath)
        shiny::updateSelectInput(session,"sheet",choices=sheets,selected=sheets[1L])
        read_table(sheets[1L])
      } else read_table()
    })
    shiny::observeEvent(input$sheet,{shiny::req(input$file);read_table(input$sheet)})
    shiny::observeEvent(input$grid,{
      change <- input$grid$changes
      if(!is.null(change$event) && (!identical(change$event,"afterChange") || !is.null(change$changes))) {
        stale(TRUE); simulated(FALSE)
      }
    },ignoreInit=TRUE)
    output$grid <- rhandsontable::renderRHandsontable(rhandsontable::rhandsontable(data(),
      useTypes=FALSE,readOnly=FALSE,height=520,contextMenu=TRUE))
    shiny::observeEvent(input$calculate,{
      tryCatch({
        d <- if(is.null(input$grid)) data() else ldj_mtt_normalize_numeric(rhandsontable::hot_to_r(input$grid))
        results(ldj_synergy_excess(d))
        stale(FALSE)
        shiny::updateTabsetPanel(session,"views",selected="Clasificación")
      },error=function(e) shiny::showNotification(conditionMessage(e),type="error"))
    })
    output$provenance <- shiny::renderText(if(simulated()) "DEMO — DATOS SIMULADOS" else
      if(stale()) "Datos editados o importados. Calcule para actualizar los resultados." else "Datos editados o importados.")
    native_cells <- shiny::reactive({
      shiny::validate(shiny::need(!stale(), "Calcule los datos vigentes."))
      ldj_synergy_model_cells(results())
    })
    classified <- shiny::reactive({
      shiny::validate(shiny::need(!stale(),"Calcule los datos vigentes."))
      d <- results()
      {
        n <- native_cells()
        key <- function(x) paste(x$experiment_id, x$biological_replicate_id,
          signif(x$dose_A_M, 14), signif(x$dose_B_M, 14), sep = "\r")
        j <- match(key(d), key(n))
        stopifnot(!anyNA(j), !anyDuplicated(j))
        d$Loewe_expected <- n$expected[j]
        d$Loewe_excess <- n$excess[j]
        d$Loewe_status <- n$status[j]
        if (identical(input$model, "Loewe")) d$nd_reason <- ifelse(n$evaluable[j], "", n$status[j])
        for (col in c("global_pp", "evaluated_n", "matrix_n", "coverage", "common_n",
          "common_domain", "Bliss_common_pp", "HSA_common_pp",
          "A_bottom", "A_top", "A_ec50", "A_hill", "A_rmse", "A_fit_status",
          "B_bottom", "B_top", "B_ec50", "B_hill", "B_rmse", "B_fit_status"))
          d[[paste0("Loewe_", col)]] <- n[[col]][j]
        d$Loewe_inference_status <- "ND · Inferencia específica sin validar"
      }
      d$score <- d[[paste0(input$model,"_excess")]]
      d$expected <- d[[paste0(input$model,"_expected")]]
      describe <- function(excess) ifelse(!is.finite(excess), "ND",
        ifelse(excess > input$cutoff, "Sinergia descriptiva",
          ifelse(excess < -input$cutoff, "Antagonismo descriptivo", "Región descriptivamente aditiva")))
      d$HSA_category <- describe(d$HSA_excess)
      d$Bliss_category <- describe(d$Bliss_excess)
      d$Loewe_category <- describe(d$Loewe_excess)
      d$model_agreement_HSA_Bliss <- ifelse(d$HSA_category == "ND" | d$Bliss_category == "ND", "ND",
        ifelse(d$HSA_category == d$Bliss_category, "Concordancia", "Discordancia"))
      d$model_agreement <- d$model_agreement_HSA_Bliss
      selection <- if (is.null(input$agreement_models)) "HSA/Bliss" else input$agreement_models
      models <- strsplit(selection, "/", fixed = TRUE)[[1L]]
      categories <- d[paste0(models, "_category")]
      valid <- rowSums(categories == "ND") == 0L
      equal <- Reduce(`&`, lapply(categories[-1L], function(x) x == categories[[1L]]))
      d$agreement_models <- paste(models, collapse = " / ")
      d$agreement_common <- valid
      d$agreement_status <- ifelse(!valid, "ND", ifelse(equal, "Concordancia", "Discordancia"))
      d$agreement_total_n <- nrow(d)
      d$agreement_common_n <- sum(valid)
      d$agreement_excluded_n <- sum(!valid)
      d$agreement_coverage <- mean(valid)
      d$agreement_exclusion <- ifelse(valid, "",
        ifelse("Loewe" %in% models & !is.finite(d$Loewe_excess), d$Loewe_status, "missing_model_excess"))
      d$category <- describe(d$score)
      d
    })
    experimental <- shiny::reactive({
      model <- shiny::req(input$experimental_model)
      shiny::validate(shiny::need(!stale(), "Calcule los datos vigentes."))
      if (model == "Loewe") native_cells() else ldj_synergy_model_cells(results(), model)
    })
    output$experimental_summary <- shiny::renderTable({
      d <- experimental()
      groups <- split(d, list(d$experiment_id, d$biological_replicate_id), drop = TRUE)
      do.call(rbind, lapply(groups, function(z) data.frame(
        Experimento = z$experiment_id[1L], Unidad = z$biological_replicate_id[1L],
        Evaluables = sum(z$evaluable), Total = nrow(z), Dominio_común = sum(z$common_domain),
        Exceso_pp = z$global_pp[1L], Exclusiones = paste(unique(z$status[!z$evaluable]), collapse = ", "))))
    }, na = "ND")
    output$experimental_plot <- shiny::renderPlot({
      d <- experimental()
      d$a <- dose_factor(ldj_convert_concentration(d$dose_A_M, "M", input$unit))
      d$b <- dose_factor(ldj_convert_concentration(d$dose_B_M, "M", input$unit))
      d$context <- interaction(d$experiment_id, d$biological_replicate_id, sep = " · ")
      dose_axes(ggplot2::ggplot(d, ggplot2::aes(a, b, fill = excess)) +
        ggplot2::geom_tile() + ggplot2::facet_wrap(~context, ncol = 2, labeller = ggplot2::label_wrap_gen(22)) +
        ggplot2::scale_fill_gradient2(low = "#287fae", mid = "#f5f2ed", high = "#c34e46",
          na.value = "#9ca3af") +
        ggplot2::labs(x = paste("Dosis A", input$unit), y = paste("Dosis B", input$unit), fill = "Exceso (pp)") +
        ldj_plot_theme(ldj_element_style(appearance())))
    })
    output$experimental_csv <- shiny::downloadHandler(filename = function() paste0("LDJ-", input$experimental_model, "-diagnostico.csv"),
      content = function(path) utils::write.csv(experimental(), path, row.names = FALSE, na = ""))
    output$class_experiment_ui <- shiny::renderUI({
      shiny::req(identical(input$class_scope, "selected"))
      experiments <- sort(unique(classified()$experiment_id))
      restored <- restored_class_experiment()
      shiny::selectInput(session$ns("class_experiment"), "Experimento", experiments,
        selected = if (restored %in% experiments) restored else experiments[1L])
    })
    restored_class_experiment <- shiny::reactiveVal("")
    uncertainty <- shiny::reactive({
      shiny::validate(shiny::need(!stale(), "Calcule los datos vigentes."),
        shiny::need(isTRUE(input$bootstrap_independent),
          "Confirme la independencia experimental para estimar el intervalo."),
        shiny::need(length(input$bootstrap_seed) == 1L && is.finite(input$bootstrap_seed) &&
          input$bootstrap_seed == trunc(input$bootstrap_seed) &&
          input$bootstrap_seed >= 0 && input$bootstrap_seed <= .Machine$integer.max,
          "Introduzca una semilla entera válida."))
      ldj_synergy_bootstrap(results(), metric = paste0(input$bootstrap_model, "_excess"),
        independent = TRUE, iterations = as.integer(input$bootstrap_iterations),
        seed = as.integer(input$bootstrap_seed))
    })
    output$bootstrap_summary <- shiny::renderText({
      x <- uncertainty()
      interval <- if (all(is.finite(x$interval))) {
        sprintf("IC95%% percentil %.2f a %.2f pp", x$interval[1L], x$interval[2L])
      } else if (x$n_experiments < 3L) {
        "IC95% no estimable. Se requieren al menos tres experimentos completos."
      } else "IC95% no estimable por remuestreos fallidos."
      sprintf("Incluidos %d · Excluidos %d · %s", x$n_experiments, x$n_excluded, interval)
    })
    output$bootstrap_scores <- shiny::renderTable(uncertainty()$scores, na = "ND")
    output$bootstrap_loo <- shiny::renderTable(uncertainty()$leave_one_out, na = "ND")
    forest_plot <- function(x) {
      plot_data <- x$scores[x$scores$complete, c("experiment_id", "estimate")]
      shiny::validate(shiny::need(nrow(plot_data) > 0L, "Ningún experimento completo."))
      plot_data$label <- paste("Experimento", plot_data$experiment_id)
      pooled <- data.frame(label = "Estimación global", estimate = x$estimate,
        lower = x$interval[1L], upper = x$interval[2L])
      interval_available <- is.finite(pooled$lower) && is.finite(pooled$upper)
      pooled$label <- if (!interval_available) "Estimación global · IC95% ND"
        else if (pooled$lower == pooled$upper) "Estimación global · IC95% degenerado"
        else pooled$label
      ggplot2::ggplot(plot_data, ggplot2::aes(x = estimate, y = label)) +
        ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
        ggplot2::geom_point(size = input$forest_point %||% 2) +
        ggplot2::geom_point(data = pooled, shape = 18, size = input$forest_global %||% 3) +
        ggplot2::geom_segment(data = pooled[interval_available, , drop = FALSE],
          ggplot2::aes(x = lower, xend = upper, yend = label), linewidth = input$forest_interval %||% .5) +
        ggplot2::labs(x = "Exceso (pp)", y = NULL) +
        ldj_plot_theme(ldj_element_style(appearance()))
    }
    output$bootstrap_plot <- shiny::renderPlot(forest_plot(uncertainty()))
    output$bootstrap_csv <- shiny::downloadHandler(filename = function() "LDJ-bootstrap.csv",
      content = function(path) {
        x <- uncertainty()
        tab <- x$scores
        tab$leave_one_out <- x$leave_one_out$estimate[
          match(tab$experiment_id, x$leave_one_out$experiment_id)]
        tab$metric <- x$metric
        tab$pooled <- x$estimate
        tab$lower95 <- x$interval[1L]
        tab$upper95 <- x$interval[2L]
        tab$status <- x$status
        tab$iterations <- x$iterations
        tab$seed <- x$seed
        tab$failed <- x$failed
        utils::write.csv(tab, path, row.names = FALSE, na = "ND")
      })
    activity <- shiny::reactiveVal(NULL)
    viability <- shiny::reactiveVal(NULL)
    benefit_demo <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$benefit_demo, {
      example <- expand.grid(dose_A = c(0.1, 0.5, 1), dose_B = c(0.1, 0.5, 1))
      example$unit_A <- example$unit_B <- "uM"
      example$compound_A <- "Sintético A"
      example$compound_B <- "Sintético B"
      example$cell_line <- "Sintética"
      example$exposure_hours <- 24
      example$experiment_id <- "SIM-1"
      dose <- example$dose_A + example$dose_B
      activity(transform(example, activity_pct = 100 * dose / (0.3 + dose)))
      viability(transform(example, viability_pct = 100 - 18 * dose))
      benefit_demo(TRUE)
    })
    benefit_input <- function(kind, value) {
      file <- shiny::reactive(input[[paste0(kind, "_file")]])
      sheets <- shiny::reactive({
        f <- file()
        if (is.null(f) || !tolower(tools::file_ext(f$name)) %in% c("xls", "xlsx"))
          return(character())
        readxl::excel_sheets(f$datapath)
      })
      output[[paste0(kind, "_sheet_ui")]] <- shiny::renderUI({
        choices <- sheets()
        if (length(choices)) shiny::selectInput(session$ns(paste0(kind, "_sheet")), "Hoja", choices)
      })
      shiny::observeEvent(list(file(), input[[paste0(kind, "_sheet")]]), {
        f <- file()
        shiny::req(f)
        tryCatch({
          choices <- sheets()
          sheet <- input[[paste0(kind, "_sheet")]]
          if (length(choices) && !isTRUE(sheet %in% choices)) sheet <- choices[1L]
          value(ldj_mtt_import_table(f$datapath, f$name,
            sheet = if (length(choices)) sheet else NULL))
          benefit_demo(FALSE)
        }, error = function(e) {
          value(NULL)
          benefit_demo(FALSE)
          shiny::showNotification(conditionMessage(e), type = "error")
        })
      }, ignoreInit = TRUE)
      output[[paste0(kind, "_grid")]] <- rhandsontable::renderRHandsontable({
        shiny::req(value())
        rhandsontable::rhandsontable(value(), useTypes = FALSE, height = 240)
      })
      shiny::reactive({
        table <- value()
        grid <- input[[paste0(kind, "_grid")]]
        if (is.null(table) || is.null(grid)) return(table)
        ldj_mtt_normalize_numeric(rhandsontable::hot_to_r(grid))
      })
    }
    activity_table <- benefit_input("activity", activity)
    viability_table <- benefit_input("viability", viability)
    output$benefit_source <- shiny::renderText(if (benefit_demo()) "DEMO — DATOS SIMULADOS" else "")
    benefit <- shiny::reactive({
      a <- activity_table()
      v <- viability_table()
      shiny::validate(shiny::need(!is.null(a), "Importe una tabla de actividad o cargue el ejemplo."))
      keys <- c("compound_A", "compound_B", "cell_line", "exposure_hours",
        "experiment_id", "dose_A", "dose_B", "unit_A", "unit_B")
      shiny::validate(shiny::need(all(c(keys, "activity_pct") %in% names(a)),
        paste("Actividad requiere", paste(c(keys, "activity_pct"), collapse = ", "))),
        shiny::need(is.null(v) || all(c(keys, "viability_pct") %in% names(v)),
          paste("Viabilidad requiere", paste(c(keys, "viability_pct"), collapse = ", "))))
      tryCatch(ldj_synergy_benefit(a, v, input$activity_min, input$viability_min),
        error = function(e) shiny::validate(shiny::need(FALSE,
          paste("Revise unidades, valores numéricos, contexto y filas duplicadas.", conditionMessage(e)))))
    })
    hits <- shiny::reactive({
      d <- benefit()
      d <- d[which(d$benefit & d$dose_A_M > 0 & d$dose_B_M > 0), , drop = FALSE]
      index <- if (input$benefit_sort == "dose") order(d$dose_A_M, d$dose_B_M) else
        order(d[[input$benefit_sort]], decreasing = TRUE)
      d[index, , drop = FALSE]
    })
    output$benefit_status <- shiny::renderText({
      d <- benefit()
      sprintf("%d pares cumplen ambos umbrales. %d filas con viabilidad ND o incompatible. %d filas con actividad ND.",
        nrow(hits()), sum(d$status == "viability_ND"), sum(d$status == "activity_ND"))
    })
    output$benefit_hits <- shiny::renderTable({
      d <- hits()
      for (key in c("dose_A_M", "dose_B_M")) d[[key]] <- sprintf("%.5g", d[[key]])
      d
    }, digits = 5, na = "ND")
    dose_factor <- function(dose) {
      values <- sort(unique(dose))
      factor(dose, levels = values,
        labels = ldj_synergy_dose_labels(values, as.integer(input$dose_digits), input$dose_notation))
    }
    heat_settings <- function() list(heat_ticks_y = input$heat_ticks_y %||% 6,
      heat_text_x = input$heat_text_x %||% 8, heat_text_y = input$heat_text_y %||% 8,
      heat_aspect = input$heat_aspect %||% 0, heat_square = isTRUE(input$heat_square),
      class_columns = input$class_columns %||% 0, class_spacing = input$class_spacing %||% 5)
    thin_doses <- function(labels, limit) {
      n <- length(labels)
      if (n <= limit) return(labels)
      labels[unique(round(seq(1, n, length.out = limit)))]
    }
    heat_style <- function(plot) {
      cfg <- heat_settings()
      plot <- plot + ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = cfg$heat_text_x),
        axis.text.y = ggplot2::element_text(size = cfg$heat_text_y),
        legend.key.height = grid::unit(9, "pt"),
        legend.key.width = grid::unit(9, "pt"))
      if (cfg$heat_square) return(plot + ggplot2::coord_fixed())
      if (cfg$heat_aspect > 0) plot <- plot + ggplot2::theme(aspect.ratio = cfg$heat_aspect)
      plot
    }
    dose_breaks <- function(labels) {
      thin_doses(labels, input$dose_ticks)
    }
    dose_axes <- function(plot, y_discrete = TRUE) {
      plot <- plot + ggplot2::scale_x_discrete(breaks = dose_breaks) +
        ggplot2::guides(x = ggplot2::guide_axis(angle = input$dose_rotation, check.overlap = TRUE))
      if (y_discrete) plot <- heat_style(plot + ggplot2::scale_y_discrete(
        breaks = function(labels) thin_doses(labels, min(heat_settings()$heat_ticks_y,
          if (identical(input$template, "integral")) 4L else 30L))))
      plot
    }
    benefit_panels <- shiny::reactive({
      d <- benefit()
      shiny::validate(shiny::need(nrow(d) > 0, "La tabla de actividad está vacía."))
      d$a <- dose_factor(ldj_convert_concentration(d$dose_A_M, "M", input$unit))
      d$b <- dose_factor(ldj_convert_concentration(d$dose_B_M, "M", input$unit))
      d$context <- interaction(d$compound_A, d$compound_B, d$cell_line, d$exposure_hours,
        d$experiment_id, drop = TRUE, sep = " · ")
      levels(d$context) <- vapply(levels(d$context), function(label)
        paste(strwrap(label, width = 29), collapse = "\n"), character(1))
      for (column in c("activity_pct", "viability_pct")) {
        invalid <- !is.finite(d[[column]]) | d[[column]] < 0 | d[[column]] > 100
        d[[column]][invalid] <- NA_real_
      }
      d$threshold_class <- factor(ifelse(is.na(d$benefit), "ND",
        ifelse(d$benefit, "Ambos umbrales", "Por debajo")),
        levels = c("Ambos umbrales", "Por debajo", "ND"))
      style <- ldj_element_style(appearance())
      panel <- function(column, title) {
        graph <- ggplot2::ggplot(d, ggplot2::aes(a, b, fill = .data[[column]])) +
          ggplot2::geom_tile() + ggplot2::facet_wrap(~context, ncol = 2) +
          ggplot2::labs(title = title, x = paste("Dosis A", input$unit), y = paste("Dosis B", input$unit), fill = NULL) +
          ldj_plot_theme(style) + ggplot2::theme(strip.text = ggplot2::element_text(size = 8,
            margin = ggplot2::margin(3, 3, 3, 3)))
        graph <- dose_axes(graph)
        if (column == "threshold_class") graph + ggplot2::scale_fill_manual(
          values = c("Ambos umbrales" = "#157d72", "Por debajo" = "#eadfcf", "ND" = "#9ca3af"))
        else graph + ggplot2::scale_fill_gradient(low = "#f5f2ed",
          high = ldj_palette_catalog()[[appearance()$palette]]$colors[1],
          limits = c(0, 100), na.value = "#9ca3af")
      }
      selected <- d[which(d$benefit & d$dose_A_M > 0 & d$dose_B_M > 0), , drop = FALSE]
      region <- ggplot2::ggplot(selected, ggplot2::aes(activity_pct, viability_pct, colour = context)) +
        ggplot2::geom_point() +
        ggplot2::geom_vline(xintercept = input$activity_min, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = input$viability_min, linetype = "dashed") +
        ggplot2::labs(title = "Regiones seleccionadas", x = "Actividad (%)", y = "Viabilidad (%)",
          colour = if (nzchar(trimws(input$benefit_legend %||% ""))) input$benefit_legend else "Condición experimental") +
        ldj_plot_theme(style)
      list(panel("activity_pct", "Actividad (%)"), panel("viability_pct", "Viabilidad (%)"),
        panel("cytotoxicity_pct", "Citotoxicidad (%)"), panel("threshold_class", "Beneficio farmacológico"),
        region)
    })
    output$benefit_plot <- shiny::renderPlot(
      patchwork::wrap_plots(benefit_panels()[1:4], ncol = 2) + patchwork::plot_annotation(tag_levels = "A"))
    output$benefit_csv <- shiny::downloadHandler(filename = function() "LDJ-beneficio.csv",
      content = function(path) utils::write.csv(hits(), path, row.names = FALSE, na = "ND"))

    panel_overrides <- shiny::reactiveVal(list())
    restored_context <- shiny::reactiveVal("")
    output$gallery_context_ui <- shiny::renderUI({
      d <- classified()
      levels <- levels(interaction(d$experiment_id, d$biological_replicate_id, drop = TRUE))
      requested <- restored_context()
      shiny::selectInput(session$ns("gallery_context"), "Contexto experimental · mapa, superficie e isobola",
        choices = c("Primero disponible" = "", stats::setNames(levels, levels), setdiff(requested, c("", levels))),
        selected = requested)
    })
    panel_key <- function(template, id, instances = NULL) {
      if (identical(template, "custom")) {
        if (is.null(instances)) instances <- ldj_synergy_gallery_sequence(input$gallery_sequence %||% "",
          input$gallery_types %||% character())
        position <- match(id, LETTERS)
        if (!is.na(position) && position <= length(instances))
          return(paste("instance", instances[[position]], names(instances)[position], sep = "/"))
      }
      paste(template, id, sep = "/")
    }
    shiny::observeEvent(input$save_panel, {
      styles <- panel_overrides()
      key <- panel_key(input$template, input$edit_panel)
      styles[[key]] <- list(title = input$panel_title, subtitle = input$panel_subtitle, tag = trimws(input$panel_tag),
        x = input$panel_x, y = input$panel_y, legend = input$panel_legend,
        font = as.character(input$panel_font %||% 0), margin = as.character(input$panel_margin %||% 0),
        family = input$panel_family %||% "", gap = as.character(input$panel_gap %||% -1),
        ticks_x = as.character(input$panel_ticks_x %||% 0), ticks_y = as.character(input$panel_ticks_y %||% 0),
        min_width = as.character(input$panel_min_width %||% 0), min_height = as.character(input$panel_min_height %||% 0))
      if (identical(input$template, "custom")) {
        controls <- c("gallery_surface_mesh", "gallery_surface_alpha", "gallery_surface_points", "gallery_surface_labels", "gallery_marginal_top", "gallery_marginal_right", "gallery_marginal_points", "gallery_density_height", "gallery_density_points", "gallery_density_alpha", "gallery_surface_perspective", "gallery_surface_vertical", "gallery_surface_theta", "gallery_surface_phi", "gallery_density_adjust", "gallery_isobole_effect", "gallery_isobole_window", "gallery_pairs", "gallery_surface_z", "gallery_comparator", "gallery_context", "gallery_marginal_field")
        for (control in controls) {
          value <- input[[control]]
          if (!is.null(value) && length(value) == 1L) styles[[key]][[control]] <- as.character(value)
        }
      }
      panel_overrides(styles)
    })
    output$figure_json_out <- shiny::downloadHandler(filename = function() "LDJ-figure-studio.json",
      content = function(path) jsonlite::write_json(list(version = 3L, template = input$template,
        columns = input$figure_columns, order = input$panel_order, panels = panel_overrides(),
        gallery = list(types = as.list(input$gallery_types %||% character()),
          surface_z = input$gallery_surface_z %||% "score",
          surface_mesh = input$gallery_surface_mesh %||% 0.25,
          surface_alpha = input$gallery_surface_alpha %||% 0.75,
          surface_points = input$gallery_surface_points %||% 1,
          surface_labels = input$gallery_surface_labels %||% 3.2,
          marginal_top = input$gallery_marginal_top %||% 1,
          marginal_right = input$gallery_marginal_right %||% 1,
          marginal_points = input$gallery_marginal_points %||% 1.7,
          density_height = input$gallery_density_height %||% 0.72,
          density_points = input$gallery_density_points %||% 1.8,
          density_alpha = input$gallery_density_alpha %||% 0.75,
          surface_perspective = input$gallery_surface_perspective %||% 0,
          surface_vertical = input$gallery_surface_vertical %||% .6,
          surface_theta = input$gallery_surface_theta %||% 38, surface_phi = input$gallery_surface_phi %||% 28,
          density_adjust = input$gallery_density_adjust %||% 1,
          context = input$gallery_context %||% restored_context(),
          marginal_field = input$gallery_marginal_field %||% "observed",
          isobole_window = input$gallery_isobole_window %||% 10,
          isobole_effect = input$gallery_isobole_effect %||% 50,
          comparator = input$gallery_comparator %||% "best",
          pairs = input$gallery_pairs %||% 8,
          sequence = input$gallery_sequence %||% "",
          layout = input$gallery_layout %||% "",
          widths = input$gallery_widths %||% "", heights = input$gallery_heights %||% "",
          values = isTRUE(input$gallery_values)),
        heat = heat_settings(),
        classification = list(scope = input$class_scope %||% "unit",
          experiment = input$class_experiment %||% ""),
        visual = list(size_preset = input$size_preset, size_units = input$size_units,
          size_width = input$size_width, size_height = input$size_height, unit = input$unit,
          dose_notation = input$dose_notation, dose_digits = input$dose_digits,
          dose_rotation = input$dose_rotation, dose_ticks = input$dose_ticks,
          benefit_legend = input$benefit_legend %||% "Condición experimental",
          forest = list(forest_point = input$forest_point %||% 2,
            forest_global = input$forest_global %||% 3, forest_interval = input$forest_interval %||% .5),
          contrast_labels = unname(as.list(c(input$contrast_a_label, input$contrast_b_label,
            input$contrast_best_label))))),
        path, auto_unbox = TRUE, pretty = TRUE))
    shiny::observeEvent(input$figure_json, {
      tryCatch({
        file <- input$figure_json
        stopifnot(file$size <= 65536)
        x <- jsonlite::read_json(file$datapath, simplifyVector = FALSE)
        scalar_text <- function(y) is.character(y) && length(y) == 1L && !is.na(y)
        stopifnot(isTRUE(x$version %in% c(1, 2, 3)), isTRUE(x$template %in% c("unit", "class", "integral", "benefit", "landscape", "concordance", "reproducibility", "optimization", "custom")),
          is.numeric(x$columns), length(x$columns) == 1L, x$columns %in% 1:6,
          scalar_text(x$order), is.list(x$panels),
          all(vapply(x$panels, function(panel)
            is.list(panel) && all(c("title", "subtitle", "tag") %in% names(panel)) &&
              all(vapply(panel, scalar_text, logical(1))), logical(1))))
        v <- if (x$version == 1) list(size_preset = "auto", size_units = "in",
          size_width = 7, size_height = 5, unit = "uM", dose_notation = "significant",
          dose_digits = 3, dose_rotation = 35, dose_ticks = 6) else x$visual
        valid_number <- function(value, lower, upper) {
          is.numeric(value) && length(value) == 1L && is.finite(value) &&
            value >= lower && value <= upper
        }
        stopifnot(is.list(v), isTRUE(v$size_preset %in% c("auto", "one", "two", "page", "custom")),
          isTRUE(v$size_units %in% c("in", "cm", "mm")), isTRUE(v$unit %in% c("uM", "nM", "mM", "M")),
          isTRUE(v$dose_notation %in% c("significant", "decimal", "scientific")),
          valid_number(v$size_width, 0.1, 762), valid_number(v$size_height, 0.1, 762),
          isTRUE(v$dose_digits %in% 1:15), valid_number(v$dose_rotation, 0, 90),
          isTRUE(v$dose_ticks %in% 2:30))
        if (!is.null(x$classification)) {
          k <- x$classification
          stopifnot(is.list(k), isTRUE(k$scope %in% c("unit", "experiment", "selected", "global")),
            scalar_text(k$experiment))
          restored_class_experiment(k$experiment)
          shiny::updateSelectInput(session, "class_scope", selected = k$scope)
        }
        if (!is.null(x$heat)) {
          h <- x$heat
          stopifnot(is.list(h), isTRUE(h$heat_ticks_y %in% 2:30),
            valid_number(h$heat_text_x, 5, 18), valid_number(h$heat_text_y, 5, 18),
            valid_number(h$heat_aspect, 0, 3), isTRUE(h$class_columns %in% 0:8),
            valid_number(h$class_spacing, 0, 25), is.logical(h$heat_square),
            length(h$heat_square) == 1L, !is.na(h$heat_square))
          for (key in setdiff(names(heat_settings()), "heat_square"))
            shiny::updateNumericInput(session, key, value = h[[key]])
          shiny::updateCheckboxInput(session, "heat_square", value = h$heat_square)
        }
        if (!is.null(v$forest)) {
          forest <- v$forest
          stopifnot(is.list(forest), valid_number(forest$forest_point, .5, 6),
            valid_number(forest$forest_global, .5, 6), valid_number(forest$forest_interval, .1, 3))
          for (key in c("forest_point", "forest_global", "forest_interval"))
            shiny::updateNumericInput(session, key, value = forest[[key]])
        }
        benefit_legend <- v$benefit_legend %||% "Condición experimental"
        stopifnot(scalar_text(benefit_legend))
        shiny::updateTextInput(session, "benefit_legend", value = benefit_legend)
        for (key in c("size_preset", "size_units", "unit", "dose_notation"))
          shiny::updateSelectInput(session, key, selected = v[[key]])
        for (key in c("size_width", "size_height", "dose_digits", "dose_rotation", "dose_ticks"))
          shiny::updateNumericInput(session, key, value = v[[key]])
        contrast_labels <- v$contrast_labels
        if (is.null(contrast_labels)) contrast_labels <- list("vs. monoterapia A", "vs. monoterapia B", "vs. mejor monoterapia")
        stopifnot(length(contrast_labels) == 3L, all(vapply(contrast_labels, scalar_text, logical(1))))
        for (i in seq_along(contrast_labels))
          shiny::updateTextInput(session, c("contrast_a_label", "contrast_b_label", "contrast_best_label")[i],
            value = contrast_labels[[i]])
        if (!is.null(x$gallery)) {
          gallery <- x$gallery
          ids <- unlist(gallery$types, use.names = FALSE)
          stopifnot(length(ids) <= 12L, all(ids %in% names(ldj_synergy_gallery())),
            scalar_text(gallery$layout), length(gallery$values) == 1L, is.logical(gallery$values))
          sequence <- gallery$sequence %||% ""
          widths <- gallery$widths %||% ""
          heights <- gallery$heights %||% ""
          stopifnot(scalar_text(sequence), scalar_text(widths), scalar_text(heights))
          surface_z <- gallery$surface_z %||% "score"
          surface_mesh <- gallery$surface_mesh %||% 0.25
          stopifnot(valid_number(surface_mesh, 0.05, 2))
          shiny::updateNumericInput(session, "gallery_surface_mesh", value = surface_mesh)
          surface_alpha <- gallery$surface_alpha %||% 0.75
          stopifnot(valid_number(surface_alpha, 0.15, 1))
          shiny::updateNumericInput(session, "gallery_surface_alpha", value = surface_alpha)
          surface_points <- gallery$surface_points %||% 1
          stopifnot(valid_number(surface_points, 0.3, 5))
          shiny::updateNumericInput(session, "gallery_surface_points", value = surface_points)
          surface_labels <- gallery$surface_labels %||% 3.2
          stopifnot(valid_number(surface_labels, 2, 6))
          shiny::updateNumericInput(session, "gallery_surface_labels", value = surface_labels)
          marginal_top <- gallery$marginal_top %||% 1
          stopifnot(valid_number(marginal_top, 0.5, 3))
          shiny::updateNumericInput(session, "gallery_marginal_top", value = marginal_top)
          marginal_right <- gallery$marginal_right %||% 1
          stopifnot(valid_number(marginal_right, 0.5, 3))
          shiny::updateNumericInput(session, "gallery_marginal_right", value = marginal_right)
          marginal_points <- gallery$marginal_points %||% 1.7
          stopifnot(valid_number(marginal_points, 0.4, 5))
          shiny::updateNumericInput(session, "gallery_marginal_points", value = marginal_points)
          density_height <- gallery$density_height %||% 0.72
          stopifnot(valid_number(density_height, 0.15, 0.9))
          shiny::updateNumericInput(session, "gallery_density_height", value = density_height)
          density_points <- gallery$density_points %||% 1.8
          stopifnot(valid_number(density_points, 0.4, 5))
          shiny::updateNumericInput(session, "gallery_density_points", value = density_points)
          density_alpha <- gallery$density_alpha %||% 0.75
          stopifnot(valid_number(density_alpha, 0.15, 1))
          shiny::updateNumericInput(session, "gallery_density_alpha", value = density_alpha)
          perspective <- gallery$surface_perspective %||% 0
          vertical <- gallery$surface_vertical %||% .6
          stopifnot(valid_number(perspective, 0, .55), valid_number(vertical, .2, 1.1))
          shiny::updateNumericInput(session, "gallery_surface_perspective", value = perspective)
          shiny::updateNumericInput(session, "gallery_surface_vertical", value = vertical)
          theta <- gallery$surface_theta %||% 38
          phi <- gallery$surface_phi %||% 28
          adjust <- gallery$density_adjust %||% 1
          stopifnot(isTRUE(surface_z %in% c("score", "observed", "expected")),
            valid_number(theta, -180, 180), valid_number(phi, 5, 85), valid_number(adjust, .2, 4))
          shiny::updateSelectInput(session, "gallery_surface_z", selected = surface_z)
          shiny::updateNumericInput(session, "gallery_surface_theta", value = theta)
          shiny::updateNumericInput(session, "gallery_surface_phi", value = phi)
          shiny::updateNumericInput(session, "gallery_density_adjust", value = adjust)
          context <- gallery$context %||% ""
          marginal <- gallery$marginal_field %||% "observed"
          window <- gallery$isobole_window %||% 10
          stopifnot(scalar_text(context), isTRUE(marginal %in% c("observed", "score")), valid_number(window, 0, 50))
          restored_context(context)
          shiny::updateSelectInput(session, "gallery_context", selected = context)
          shiny::updateSelectInput(session, "gallery_marginal_field", selected = marginal)
          shiny::updateNumericInput(session, "gallery_isobole_window", value = window)
          effect <- gallery$isobole_effect %||% 50
          stopifnot(valid_number(effect, 1, 99))
          shiny::updateNumericInput(session, "gallery_isobole_effect", value = effect)
          comparator <- gallery$comparator %||% "best"
          pairs <- gallery$pairs %||% 8
          stopifnot(isTRUE(comparator %in% c("best", "a", "b")),
            valid_number(pairs, 1, 24), pairs == trunc(pairs))
          shiny::updateSelectInput(session, "gallery_comparator", selected = comparator)
          shiny::updateNumericInput(session, "gallery_pairs", value = pairs)
          selected <- ldj_synergy_gallery_sequence(sequence, ids)
          if (nzchar(trimws(gallery$layout))) {
            design <- ldj_synergy_gallery_design(gallery$layout, length(selected))
            rows <- strsplit(design, "\n", fixed = TRUE)[[1L]]
            ldj_synergy_gallery_weights(widths, nchar(rows[1L]))
            ldj_synergy_gallery_weights(heights, length(rows))
          }
          shiny::updateSelectizeInput(session, "gallery_types", selected = ids)
          shiny::updateTextInput(session, "gallery_layout", value = gallery$layout)
          shiny::updateTextInput(session, "gallery_sequence", value = sequence)
          shiny::updateTextInput(session, "gallery_widths", value = widths)
          shiny::updateTextInput(session, "gallery_heights", value = heights)
          shiny::updateCheckboxInput(session, "gallery_values", value = gallery$values)
        }
        if (x$version < 3 && !is.null(x$gallery)) {
          for (i in seq_along(selected)) {
            legacy <- paste("custom", LETTERS[i], sep = "/")
            if (!is.null(x$panels[[legacy]])) {
              x$panels[[panel_key("custom", LETTERS[i], selected)]] <- x$panels[[legacy]]
              x$panels[[legacy]] <- NULL
            }
          }
        }
        panel_overrides(x$panels)
        shiny::updateSelectInput(session, "template", selected = x$template)
        shiny::updateNumericInput(session, "figure_columns", value = x$columns)
        shiny::updateTextInput(session, "panel_order", value = x$order)
      }, error = function(e) shiny::showNotification("Configuración de Figure Studio no válida.", type = "error"))
    })
    figure_panels <- shiny::reactive({
      d <- classified()
      d$a <- dose_factor(ldj_convert_concentration(d$dose_A_M,"M",input$unit))
      d$b <- dose_factor(ldj_convert_concentration(d$dose_B_M,"M",input$unit))
      d$context <- interaction(d$experiment_id,d$biological_replicate_id,drop=TRUE)
      state <- appearance()
      style <- ldj_element_style(state)
      theme <- ldj_plot_theme(style)
      class_data <- d
      class_scope <- input$class_scope %||% "unit"
      class_title <- "Clasificación por unidad"
      aggregation_label <- NULL
      if (class_scope != "unit") {
        mean_valid <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
        class_data <- stats::aggregate(d$score, d[c("a", "b", "experiment_id")], mean_valid)
        names(class_data)[4L] <- "score"
        class_title <- "Clasificación por experimento"
        aggregation_label <- "Media por dosis de unidades biológicas evaluables"
        if (class_scope == "selected") {
          selected <- input$class_experiment %||% restored_class_experiment()
          if (!selected %in% class_data$experiment_id) selected <- class_data$experiment_id[1L]
          class_data <- class_data[class_data$experiment_id == selected, , drop = FALSE]
        }
        class_data$context <- factor(class_data$experiment_id)
        if (class_scope == "global") {
          class_data <- stats::aggregate(class_data$score, class_data[c("a", "b")], mean_valid)
          names(class_data)[3L] <- "score"
          class_data$context <- factor(rep("Global", nrow(class_data)))
          class_title <- "Clasificación global"
          aggregation_label <- "Media por dosis con igual peso por experimento evaluable"
        }
        class_data$category <- ifelse(!is.finite(class_data$score), "ND",
          ifelse(class_data$score > input$cutoff, "Sinergia descriptiva",
            ifelse(class_data$score < -input$cutoff, "Antagonismo descriptivo",
              "Región descriptivamente aditiva")))
      }
      map <- ggplot2::ggplot(class_data,ggplot2::aes(a,b,fill=category))+ggplot2::geom_tile(colour=style$background)+
        ggplot2::scale_fill_manual(values=c(`Sinergia descriptiva`="#c34e46",
          `Antagonismo descriptivo`="#287fae",`Región descriptivamente aditiva`="#eadfcf",
          `Sin exceso relevante`="#eadfcf",ND="#9ca3af"))+
        ggplot2::labs(title=class_title,subtitle=aggregation_label,x=paste("A",input$unit),y=paste("B",input$unit),fill=NULL)+theme
      # The available horizontal space is divided by both the composition
      # columns and the number of facets in the panel.
      heat_cfg <- heat_settings()
      facet_columns <- if (heat_cfg$class_columns > 0) heat_cfg$class_columns else
        min(3L, max(1L, nlevels(class_data$context)))
      facet_breaks <- function(labels) thin_doses(labels,
        min(input$dose_ticks, if (facet_columns > 1L || max(nchar(labels), 0) > 6L) 3L else 5L))
      map <- map + ggplot2::facet_wrap(~context, ncol = facet_columns) +
        ggplot2::scale_x_discrete(breaks = facet_breaks) +
        ggplot2::scale_y_discrete(breaks = function(labels) thin_doses(labels,
          min(heat_cfg$heat_ticks_y, if (identical(input$template, "integral") ||
            nlevels(class_data$context) > facet_columns) 3L else 5L))) +
        ggplot2::guides(x = ggplot2::guide_axis(angle = input$dose_rotation, check.overlap = TRUE)) +
        ggplot2::theme(panel.spacing = grid::unit(heat_cfg$class_spacing, "pt"), legend.position = "bottom")
      map <- heat_style(map) + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2))
      score <- ggplot2::ggplot(d,ggplot2::aes(observed,score))+
        ggplot2::geom_hline(yintercept=c(-input$cutoff,0,input$cutoff),linetype="dashed",colour=style$reference)+
        ggplot2::geom_point(colour=ldj_palette_catalog()[[state$palette]]$colors[1],na.rm=TRUE)+
        ggplot2::labs(title=paste("Exceso frente a",input$model),x="Inhibición observada (%)",y="Exceso (puntos porcentuales)")+theme
      placeholder <- function(title,note) ggplot2::ggplot()+
        ggplot2::annotate("text", x = 0, y = 0, label = paste(strwrap(note, 27), collapse = "\n"),
          size = 3.1, lineheight = 1.05, colour = style$text)+
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::labs(title=title)+ggplot2::theme_void()+
        ggplot2::theme(plot.title=ggplot2::element_text(colour=style$title))
      heat <- function(column,title,diverging=FALSE) {
        by_experiment <- stats::aggregate(d[[column]],d[c("a","b","experiment_id")],
          function(x) if(any(is.finite(x))) mean(x,na.rm=TRUE) else NA_real_)
        names(by_experiment)[4] <- "value"
        grid <- stats::aggregate(value~a+b,by_experiment,
          function(x) if(any(is.finite(x))) mean(x,na.rm=TRUE) else NA_real_,na.action=stats::na.pass)
        p <- ggplot2::ggplot(grid,ggplot2::aes(a,b,fill=value))+ggplot2::geom_tile()+
          ggplot2::labs(title=title,x=paste("A",input$unit),y=paste("B",input$unit),fill=NULL)+theme
        p <- dose_axes(p)
        if (isTRUE(input$gallery_values) || anyNA(grid$value)) {
          grid$label <- ifelse(is.na(grid$value), "ND", if (isTRUE(input$gallery_values)) sprintf("%.1f", grid$value) else "")
          p <- p + ggplot2::geom_text(data = grid, ggplot2::aes(label = label), size = 2.6, colour = style$text)
        }
        if(diverging) p+ggplot2::scale_fill_gradient2(low="#287FAE",mid="#F5F2ED",high="#6B1E58",midpoint=0,na.value="#9CA3AF")
        else p+ggplot2::scale_fill_gradient(low="#f5f2ed",high=ldj_palette_catalog()[[state$palette]]$colors[1],
          na.value="#9ca3af")
      }
      count <- ggplot2::ggplot(d,ggplot2::aes(category))+ggplot2::geom_bar(fill=style$axis)+
        ggplot2::labs(title="Conteos por categoría",x=NULL,y="Pares por unidad")+theme+
        ggplot2::theme(axis.text.x=ggplot2::element_text(angle=25,hjust=1))
      rows <- ldj_synergy_inference(d, isTRUE(input$independence))
      contrasts <- c("against_A", "against_B", "against_best")
      rows <- rows[rows$contrast %in% contrasts, , drop = FALSE]
      display <- c(input$contrast_a_label, input$contrast_b_label, input$contrast_best_label)
      defaults <- c("vs. monoterapia A", "vs. monoterapia B", "vs. mejor monoterapia")
      display[!nzchar(trimws(display))] <- defaults[!nzchar(trimws(display))]
      display <- vapply(display, function(x) paste(strwrap(x, 18), collapse = "\n"), character(1))
      rows$contrast <- factor(rows$contrast, levels = rev(contrasts))
      rows$evidence <- ifelse(is.finite(rows$low) & is.finite(rows$high), "Estimación con IC95%", "Descriptiva")
      intervals <- rows[rows$evidence == "Estimación con IC95%", , drop = FALSE]
      efficacy <- ggplot2::ggplot(rows, ggplot2::aes(estimate_pp, contrast)) +
        ggplot2::geom_vline(xintercept = 0, colour = style$reference) +
        ggplot2::geom_segment(data = intervals, ggplot2::aes(x = low, xend = high, yend = contrast),
          colour = style$axis, alpha = 0.25) +
        ggplot2::geom_point(ggplot2::aes(shape = evidence), colour = style$axis, na.rm = TRUE) +
        ggplot2::scale_shape_manual(values = c("Descriptiva" = 1, "Estimación con IC95%" = 16)) +
        ggplot2::scale_y_discrete(labels = stats::setNames(display, contrasts)) +
        ggplot2::labs(title = "Diferencias frente a monoterapia", x = "Puntos porcentuales",
          y = NULL, shape = NULL) + theme + ggplot2::theme(legend.position = "bottom")
      mono <- function(axis, column, title) {
        dose_axes(ggplot2::ggplot(d, ggplot2::aes(x = .data[[axis]], y = .data[[column]], group = context)) +
          ggplot2::stat_summary(fun = mean, geom = "line", na.rm = TRUE, colour = style$axis) +
          ggplot2::stat_summary(fun = mean, geom = "point", na.rm = TRUE, colour = style$axis) +
          ggplot2::labs(title = title, x = paste("Dosis", axis, input$unit), y = "Inhibición (%)") + theme, y_discrete = FALSE)
      }
      mono_a <- mono("a", "mono_A", "Monoterapia A")
      mono_b <- mono("b", "mono_B", "Monoterapia B")
      forest <- if (identical(input$model, "Loewe")) {
        placeholder("Incertidumbre por experimento", "ND · Inferencia específica de Loewe sin validar")
      } else if (isTRUE(input$bootstrap_independent)) {
        u <- uncertainty()
        if (u$n_experiments) forest_plot(u) + ggplot2::labs(title = "Incertidumbre por experimento")
        else placeholder("Incertidumbre", "ND · Sin experimentos completos")
      } else placeholder("Incertidumbre", "ND · Confirme independencia en Incertidumbre")
      mono_data <- unique(rbind(
        data.frame(dose = d$dose_A_M, response = d$mono_A, context = d$context, agent = "A"),
        data.frame(dose = d$dose_B_M, response = d$mono_B, context = d$context, agent = "B")))
      mono_data$dose_label <- dose_factor(ldj_convert_concentration(mono_data$dose, "M", input$unit))
      mono_overview <- dose_axes(ggplot2::ggplot(mono_data,
        ggplot2::aes(dose_label, response, colour = agent, group = interaction(context, agent))) +
        ggplot2::geom_line(na.rm = TRUE, alpha = 0.65) + ggplot2::geom_point(na.rm = TRUE) +
        ggplot2::labs(title = "Monoterapias A y B", x = paste("Dosis", input$unit),
          y = "Inhibición (%)", colour = "Monoterapia") + theme, y_discrete = FALSE)
      comparison_models <- strsplit(d$agreement_models[[1L]], " / ", fixed = TRUE)[[1L]]
      comparison_subtitle <- sprintf("%s · %d/%d celdas · %d excluidas", d$agreement_models[[1L]],
        sum(d$agreement_common), nrow(d), sum(!d$agreement_common))
      agreement_map <- map + ggplot2::labs(subtitle = comparison_subtitle)
      linked <- if (input$template %in% c("integral", "benefit") && !is.null(activity_table())) benefit_panels() else
        lapply(c("Actividad vinculada", "Viabilidad", "Citotoxicidad", "Beneficio", "Regiones"),
          function(x) placeholder(x, "ND · Sin tabla vinculada"))
      selected_heat <- heat(paste0(input$model, "_excess"), input$model, TRUE)
      gallery_presets <- list(landscape = c("observed", "interaction", "bubble", "observed_expected"),
        concordance = c("concordance", "distribution", "interaction", "observed_expected"),
        reproducibility = c("forest", "distribution", "dumbbell", "observed_expected"),
        optimization = c("conditional_a", "conditional_b", "benefit", "pareto"),
        custom = if (identical(input$template, "custom"))
          tryCatch(ldj_synergy_gallery_sequence(input$gallery_sequence %||% "",
            input$gallery_types %||% character()), error = function(e) character()) else character())
      selected_context <- input$gallery_context %||% restored_context()
      context_grid <- function(context = selected_context) {
        chosen <- if (nzchar(context)) context else levels(d$context)[1L]
        d[as.character(d$context) %in% chosen, , drop = FALSE]
      }
      build_gallery <- function(id, instance = NULL) {
        cfg <- if (!is.null(instance)) panel_overrides()[[paste("instance", id, instance, sep = "/")]]
        pref <- function(field, default, limits = NULL, choices = NULL) {
          value <- cfg[[field]] %||% input[[field]] %||% default
          if (!is.null(limits)) {
            value <- suppressWarnings(as.numeric(value))
            if (!is.finite(value) || value < limits[1L] || value > limits[2L]) return(default)
          }
          if (!is.null(choices) && !value %in% choices) return(default)
          value
        }
        nd <- function(reason) placeholder(ldj_synergy_gallery()[[id]][1L], paste("ND ·", reason))
        conditional <- function(axis, fixed) {
          valid <- d
          if (!any(is.finite(valid$observed))) return(nd("Sin inhibición observada"))
          levels_fixed <- levels(valid[[fixed]])
          colours <- ldj_series_colors(state, levels_fixed)
          shapes <- stats::setNames(rep(c(16, 1, 17, 2, 15, 0, 18, 5), length.out = length(levels_fixed)),
            levels_fixed)
          ggplot2::ggplot(valid, ggplot2::aes(x = .data[[axis]], y = observed,
            colour = .data[[fixed]], shape = .data[[fixed]], group = interaction(context, .data[[fixed]]))) +
            ggplot2::geom_line(linewidth = .5, na.rm = TRUE) + ggplot2::geom_point(size = 1.4, na.rm = TRUE) +
            ggplot2::scale_colour_manual(values = colours) + ggplot2::scale_shape_manual(values = shapes) +
            ggplot2::scale_x_discrete(breaks = function(x) thin_doses(x, 2)) +
            ggplot2::facet_wrap(~experiment_id, ncol = min(2L, length(unique(valid$experiment_id))),
              labeller = ggplot2::label_wrap_gen(22)) +
            ggplot2::labs(x = paste(if (axis == "a") "Dosis A" else "Dosis B", input$unit),
              y = "Inhibición (%)", colour = if (fixed == "a") "Dosis A fija" else "Dosis B fija",
              shape = if (fixed == "a") "Dosis A fija" else "Dosis B fija",
              title = ldj_synergy_gallery()[[id]][1L], subtitle = "Perfiles observados") + theme +
            ggplot2::theme(legend.position = "bottom", legend.key.width = grid::unit(9, "pt"))
        }
        paired <- d[is.finite(d$HSA_excess) & is.finite(d$observed), , drop = FALSE]
        common <- d[is.finite(d$HSA_excess) & is.finite(d$Bliss_excess), , drop = FALSE]
        shared <- d[d$agreement_common, , drop = FALSE]
        comparison_pairs <- function() {
          pairs <- utils::combn(comparison_models, 2L, simplify = FALSE)
          do.call(rbind, lapply(pairs, function(pair) data.frame(
            first = shared[[paste0(pair[1L], "_excess")]],
            second = shared[[paste0(pair[2L], "_excess")]],
            experiment_id = shared$experiment_id, pair = paste(pair, collapse = " / "))))
        }
        switch(id,
          mono = mono_overview,
          observed = heat("observed", "Inhibición observada"),
          interaction = selected_heat,
          observed_expected = {
            valid <- d[is.finite(d$score) & is.finite(d$expected) & is.finite(d$observed), , drop = FALSE]
            if (!nrow(valid)) return(nd("Sin celdas evaluables"))
            ggplot2::ggplot(valid, ggplot2::aes(expected, observed)) +
              ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = style$axis) +
              ggplot2::geom_point(colour = "#287FAE", alpha = .65) +
              ggplot2::coord_equal() + ggplot2::labs(title = "Observado frente a esperado", subtitle = input$model,
                x = "Esperada (%)", y = "Observada (%)") + theme
          },
          conditional_a = conditional("a", "b"),
          conditional_b = conditional("b", "a"),
          forest = forest,
          bubble = {
            points <- d
            points$direction <- factor(ifelse(!is.finite(d$score), "ND",
              ifelse(d$score > 0, "Positivo", ifelse(d$score < 0, "Negativo", "Cero"))),
              levels = c("Positivo", "Negativo", "Cero", "ND"))
            points$magnitude <- abs(points$score)
            ggplot2::ggplot(points, ggplot2::aes(a, b)) +
              ggplot2::geom_point(data = points[is.finite(points$magnitude), ],
                ggplot2::aes(size = magnitude, shape = direction, colour = direction)) +
              ggplot2::geom_point(data = points[!is.finite(points$magnitude), ],
                ggplot2::aes(shape = direction, colour = direction), size = 2) +
              ggplot2::scale_size_area(max_size = 4, n.breaks = 3) +
              ggplot2::scale_shape_manual(values = c(Positivo = 17, Negativo = 25, Cero = 1, ND = 4)) +
              ggplot2::scale_colour_manual(values = c(Positivo = "#6B1E58", Negativo = "#287FAE", Cero = "#59606A", ND = "#59606A")) +
              ggplot2::facet_wrap(~context, ncol = 2, labeller = ggplot2::label_wrap_gen(22)) +
              ggplot2::scale_x_discrete(breaks = function(x) thin_doses(x, 2),
                guide = ggplot2::guide_axis(angle = 35), expand = ggplot2::expansion(add = .55)) +
              ggplot2::scale_y_discrete(breaks = function(x) thin_doses(x, 4)) +
              ggplot2::labs(title = paste("Interacción ·", input$model), x = paste("Dosis A", input$unit),
                y = paste("Dosis B", input$unit), colour = "Dirección", shape = "Dirección", size = "|Exceso| (pp)") + theme +
              ggplot2::theme(legend.position = "bottom", legend.box = "vertical",
                legend.key.width = grid::unit(10, "pt"), panel.spacing = grid::unit(13, "pt"))
          },
          dumbbell = {
            if (!nrow(paired)) return(nd("Sin pares con controles y monoterapias correspondientes"))
            comparator <- pref("gallery_comparator", "best", choices = c("best", "a", "b"))
            paired$reference <- paired[[switch(comparator, a = "mono_A", b = "mono_B", "HSA_expected")]]
            pair_limit <- as.integer(pref("gallery_pairs", 8, c(1, 24)))
            paired <- utils::head(paired[order(-abs(paired$observed - paired$reference)), ], pair_limit)
            paired$pair <- rev(seq_len(nrow(paired)))
            format_dose <- function(x) format(signif(ldj_convert_concentration(x, "M", input$unit), 3), trim = TRUE)
            tags <- paste(format_dose(paired$dose_A_M), format_dose(paired$dose_B_M),
              sep = " / ")
            tags <- paste(tags, vapply(as.character(paired$context), function(x) paste(strwrap(x, 24), collapse = "\n"), character(1)), sep = "\n")
            ggplot2::ggplot(paired, ggplot2::aes(y = pair)) +
              ggplot2::geom_segment(ggplot2::aes(x = reference, xend = observed, yend = pair), colour = style$axis) +
              ggplot2::geom_point(ggplot2::aes(x = reference, shape = "Monoterapia"), colour = "#287FAE", size = 2) +
              ggplot2::geom_point(ggplot2::aes(x = observed, shape = "Combinación"), colour = "#6B1E58", size = 2) +
              ggplot2::scale_shape_manual(values = c("Monoterapia" = 1, "Combinación" = 16)) +
              ggplot2::scale_y_continuous(breaks = paired$pair, labels = tags) +
              ggplot2::labs(title = "Comparación pareada descriptiva", subtitle = sprintf("%s · hasta %d diferencias absolutas mayores",
                switch(comparator, a = "Monoterapia A", b = "Monoterapia B", "Mejor monoterapia"), pair_limit),
                x = "Inhibición (%)", y = paste("A / B", input$unit), shape = NULL) + theme +
              ggplot2::theme(legend.position = "bottom", axis.text.y = ggplot2::element_text(lineheight = .95))
          },
          concordance = {
            if (!nrow(shared)) return(nd(comparison_subtitle))
            paired <- comparison_pairs()
            ggplot2::ggplot(paired, ggplot2::aes(first, second)) +
              ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = style$axis) +
              ggplot2::geom_point(colour = style$point, size = style$size, alpha = style$alpha) +
              ggplot2::facet_wrap(~pair) +
              ggplot2::coord_equal() + ggplot2::labs(title = "Concordancia descriptiva",
                subtitle = comparison_subtitle, x = "Primer modelo (pp)", y = "Segundo modelo (pp)") + theme
          },
          bland_altman = {
            if (!nrow(shared)) return(nd(comparison_subtitle))
            paired <- comparison_pairs()
            paired$mean_pp <- (paired$second + paired$first) / 2
            paired$difference_pp <- paired$second - paired$first
            lines <- lapply(split(paired$difference_pp, paired$pair), function(x) {
              spread <- stats::sd(x)
              c(mean(x), if (length(x) >= 3L && spread > 0) mean(x) + c(-1, 1) * 1.96 * spread)
            })
            guides <- data.frame(pair = rep(names(lines), lengths(lines)), value = unlist(lines),
              guide = unlist(lapply(lengths(lines), function(n) c("Media", rep("Límites", n - 1L)))))
            ggplot2::ggplot(paired, ggplot2::aes(mean_pp, difference_pp, colour = experiment_id)) +
              ggplot2::geom_hline(data = guides, ggplot2::aes(yintercept = value, linetype = guide),
                colour = style$axis, show.legend = FALSE) +
              ggplot2::geom_point(alpha = style$alpha, size = style$size) +
              ggplot2::facet_wrap(~pair) +
              ggplot2::scale_colour_manual(values = ldj_series_colors(state, paired$experiment_id)) +
              ggplot2::labs(title = "Bland–Altman descriptivo", subtitle = comparison_subtitle,
                x = "Media de los modelos (pp)", y = "Segundo menos primero (pp)", colour = "Experimento") + theme
          },
          upset = {
            if (!nrow(common)) return(nd("Sin celdas compartidas"))
            membership <- expand.grid(HSA = c(FALSE, TRUE), Bliss = c(FALSE, TRUE))
            membership$n <- mapply(function(h, b)
              sum((common$HSA_excess > input$cutoff) == h & (common$Bliss_excess > input$cutoff) == b),
              membership$HSA, membership$Bliss)
            membership$column <- seq_len(nrow(membership))
            gap <- max(1, max(membership$n) * .16)
            marks <- data.frame(column = rep(membership$column, 2),
              row = rep(c(-gap, -2 * gap), each = nrow(membership)),
              member = c(membership$HSA, membership$Bliss))
            both <- membership[membership$HSA & membership$Bliss, , drop = FALSE]
            ggplot2::ggplot(membership, ggplot2::aes(column, n)) +
              ggplot2::geom_col(fill = ldj_series_colors(state, "Intersección")[[1L]]) +
              ggplot2::geom_text(ggplot2::aes(label = n), vjust = -.4, colour = style$text) +
              ggplot2::geom_segment(data = both, ggplot2::aes(x = column, xend = column,
                y = -gap, yend = -2 * gap), inherit.aes = FALSE, colour = style$axis) +
              ggplot2::geom_point(data = marks, ggplot2::aes(column, row, shape = member),
                inherit.aes = FALSE, colour = style$axis, size = style$size) +
              ggplot2::scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16), guide = "none") +
              ggplot2::scale_x_continuous(breaks = membership$column,
                labels = c("Ninguno", "Solo HSA", "Solo Bliss", "Ambos")) +
              ggplot2::scale_y_continuous(breaks = c(-2 * gap, -gap, 0, max(membership$n)),
                labels = c("Bliss", "HSA", "0", max(membership$n)),
                expand = ggplot2::expansion(mult = c(.08, .16))) +
              ggplot2::labs(title = "Intersecciones por modelo", subtitle = sprintf(
                "Umbral > %g pp · %d celdas ND excluidas", input$cutoff, nrow(d) - nrow(common)),
                x = NULL, y = "Celdas compartidas") + theme
          },
          marginal = {
            grid <- context_grid(pref("gallery_context", selected_context))
            if (!nrow(grid)) return(nd("Contexto experimental no disponible"))
            field <- pref("gallery_marginal_field", "observed", choices = c("observed", "score"))
            if (!any(is.finite(grid[[field]]))) return(nd("Sin resultados para la variable"))
            scale <- if (field == "score") ggplot2::scale_fill_gradient2(
              low = "#287FAE", mid = "#F5F2ED", high = "#6B1E58", na.value = "#9CA3AF")
              else ggplot2::scale_fill_gradient(low = "#F5F2ED",
                high = ldj_series_colors(state, "Inhibición")[[1L]], na.value = "#9CA3AF")
            main <- ggplot2::ggplot(grid, ggplot2::aes(a, b, fill = .data[[field]])) +
              ggplot2::geom_tile() + scale +
              ggplot2::labs(title = "Mapa con monoterapias", subtitle = as.character(grid$context[1L]),
                x = paste("Dosis A", input$unit), y = paste("Dosis B", input$unit), fill = if (field == "score") "Exceso (pp)" else "Inhibición (%)") + theme +
              ggplot2::scale_x_discrete(limits = levels(grid$a), breaks = function(x) thin_doses(x, 3)) +
              ggplot2::scale_y_discrete(limits = levels(grid$b), breaks = function(x) thin_doses(x, 4)) +
              ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = grid::unit(20, "mm"))) +
              ggplot2::theme(legend.position = "bottom")
            a <- grid[!duplicated(grid$a), , drop = FALSE]
            b <- grid[!duplicated(grid$b), , drop = FALSE]
            top <- ggplot2::ggplot(a, ggplot2::aes(a, mono_A, group = 1)) +
              ggplot2::geom_line(colour = style$curve, na.rm = TRUE) +
              ggplot2::geom_point(colour = style$point, size = pref("gallery_marginal_points", 1.7, c(.4, 5)), na.rm = TRUE) +
              ggplot2::labs(x = NULL, y = "Mono A (%)") + theme +
              ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
            right <- ggplot2::ggplot(b, ggplot2::aes(mono_B, b, group = 1)) +
              ggplot2::geom_line(colour = style$curve, orientation = "y", na.rm = TRUE) +
              ggplot2::geom_point(colour = style$point, size = pref("gallery_marginal_points", 1.7, c(.4, 5)), na.rm = TRUE) +
              ggplot2::labs(x = "Mono B (%)", y = NULL) + theme +
              ggplot2::scale_x_continuous(breaks = c(0, 100), limits = c(0, 100)) +
              ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
            top <- top + ggplot2::scale_x_discrete(limits = levels(grid$a))
            right <- right + ggplot2::scale_y_discrete(limits = levels(grid$b))
            composite <- patchwork::wrap_plots(list(top, right, main), design = "A#\nCB",
              widths = c(3, pref("gallery_marginal_right", 1, c(.5, 3))),
              heights = c(pref("gallery_marginal_top", 1, c(.5, 3)), 3))
            patchwork::wrap_elements(full = composite, ignore_tag = FALSE)
          },
          surface = {
            grid <- context_grid(pref("gallery_context", selected_context))
            if (!nrow(grid)) return(nd("Contexto experimental no disponible"))
            x <- sort(unique(grid$dose_A_M))
            y <- sort(unique(grid$dose_B_M))
            values <- matrix(NA_real_, length(x), length(y))
            field <- pref("gallery_surface_z", "score", choices = c("score", "observed", "expected"))
            values[cbind(match(grid$dose_A_M, x), match(grid$dose_B_M, y))] <- grid[[field]]
            mesh <- ldj_synergy_surface(x, y, values,
              theta = pref("gallery_surface_theta", 38, c(-180, 180)), phi = pref("gallery_surface_phi", 28, c(5, 85)),
              perspective = pref("gallery_surface_perspective", 0, c(0, .55)),
              vertical = pref("gallery_surface_vertical", .6, c(.2, 1.1)))
            if (is.null(mesh) || is.null(mesh$faces)) return(nd("Sin rectángulos evaluables"))
            mesh$faces$face <- factor(mesh$faces$face, levels = unique(mesh$faces$face))
            ticks <- mesh$ticks
            doses <- ticks$axis != "Z"
            ticks$tick[doses] <- ldj_convert_concentration(ticks$tick[doses], "M", input$unit)
            ticks$label <- paste(ticks$axis, format(signif(ticks$tick, 3), trim = TRUE))
            groups <- split(seq_len(nrow(ticks)), ticks$axis)
            limits <- c(A = suppressWarnings(as.numeric(cfg$ticks_x %||% 2)),
              B = suppressWarnings(as.numeric(cfg$ticks_y %||% 2)), Z = 2)
            labels <- unlist(lapply(groups, function(idx) {
              n <- limits[as.character(ticks$axis[idx[1L]])]
              if (!is.finite(n) || n < 2) n <- 2
              thin_doses(idx, min(n, 6L))
            }), use.names = FALSE)
            labels <- ticks[labels, , drop = FALSE]
            labels$label_v <- labels$v + ifelse(labels$axis == "Z", 1, -1) * .045 * diff(range(mesh$points$v))
            labels$label_u <- labels$u + ifelse(labels$axis == "Z", -.10,
              ifelse(labels$axis == "A", .045, -.045)) * diff(range(mesh$points$u))
            fill_scale <- if (field == "score")
              ggplot2::scale_fill_gradient2(low = "#287FAE", mid = "#F5F2ED", high = "#6B1E58")
              else ggplot2::scale_fill_gradient(low = "#F5F2ED", high = ldj_series_colors(state, "Inhibición")[[1L]])
            ggplot2::ggplot() +
              ggplot2::geom_polygon(data = mesh$faces, ggplot2::aes(u, v, group = face, fill = fill),
                colour = style$axis, linewidth = pref("gallery_surface_mesh", .25, c(.05, 2)),
                alpha = pref("gallery_surface_alpha", .75, c(.15, 1))) +
              ggplot2::geom_path(data = mesh$zero, ggplot2::aes(u, v), colour = "#536475",
                linewidth = .5, linetype = 3) +
              ggplot2::geom_point(data = mesh$points, ggplot2::aes(u, v), colour = style$axis,
                size = pref("gallery_surface_points", 1, c(.3, 5))) +
              ggplot2::geom_path(data = ticks, ggplot2::aes(u, v, group = axis), colour = style$axis) +
              ggplot2::geom_text(data = labels, ggplot2::aes(label_u, label_v, label = label),
                colour = style$text, size = pref("gallery_surface_labels", 3.2, c(2, 6)),
                vjust = .5, check_overlap = TRUE) +
              fill_scale +
              ggplot2::coord_equal(clip = "off", expand = TRUE) + ggplot2::labs(title = "Superficie de interacción",
                subtitle = paste0("A: ", grid$compound_A[1L], " / B: ", grid$compound_B[1L],
                  " (", input$unit, ") · ", as.character(grid$context[1L])),
                fill = switch(field, observed = "Observada (%)", expected = "Esperada (%)", "Exceso (pp)")) + theme +
              ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank(), axis.line = ggplot2::element_blank())
          },
          isobole = {
            grid <- context_grid(pref("gallery_context", selected_context))
            if (!nrow(grid)) return(nd("Contexto experimental no disponible"))
            a <- grid[!duplicated(grid$dose_A_M), , drop = FALSE]
            b <- grid[!duplicated(grid$dose_B_M), , drop = FALSE]
            base <- grid$control[1L]
            fit_a <- ldj_fit_loewe_monotherapy(a$dose_A_M, a$mono_A, base)
            fit_b <- ldj_fit_loewe_monotherapy(b$dose_B_M, b$mono_B, base)
            effect <- pref("gallery_isobole_effect", 50, c(1, 99))
            curve <- ldj_synergy_isobole(fit_a, fit_b, effect)
            if (is.null(curve)) return(nd("Efecto fuera del dominio común o curvas no identificables"))
            for (axis in c("dose_A_M", "dose_B_M")) {
              curve[[axis]] <- ldj_convert_concentration(curve[[axis]], "M", input$unit)
              grid[[axis]] <- ldj_convert_concentration(grid[[axis]], "M", input$unit)
            }
            window <- pref("gallery_isobole_window", 10, c(0, 50))
            matched <- grid[is.finite(grid$observed) & abs(grid$observed - effect) <= window, , drop = FALSE]
            ggplot2::ggplot(matched, ggplot2::aes(dose_A_M, dose_B_M)) +
              ggplot2::geom_point(ggplot2::aes(colour = observed), size = style$size) +
              ggplot2::geom_line(data = curve, colour = style$axis, linetype = 2) +
              ggplot2::scale_colour_gradient(low = "#DCE9F0", high = "#287FAE") +
              ggplot2::labs(title = "Isobola aditiva experimental",
                subtitle = sprintf("Loewe al %g%% · %d observaciones dentro de ±%g pp", effect, nrow(matched), window),
                x = paste("Dosis A", input$unit), y = paste("Dosis B", input$unit), colour = "Inhibición (%)") + theme
          },
          contour = {
            observed <- d[is.finite(d$score), , drop = FALSE]
            if (!nrow(observed)) return(nd("Sin celdas evaluables"))
            x <- levels(d$a)
            y <- levels(d$b)
            d$column <- as.integer(d$a)
            d$row <- as.integer(d$b)
            # Only complete four-vertex rectangles contribute interpolated lines.
            grids <- split(d, d$context)
            has_square <- any(vapply(grids, function(z) {
              values <- matrix(NA_real_, length(x), length(y))
              values[cbind(z$column, z$row)] <- z$score
              if (nrow(values) < 2L || ncol(values) < 2L) return(FALSE)
              finite <- is.finite(values)
              any(finite[-1, -1] & finite[-nrow(values), -1] &
                finite[-1, -ncol(values)] & finite[-nrow(values), -ncol(values)])
            }, logical(1)))
            if (!has_square) return(nd("Sin rectángulos completos para interpolar"))
            ggplot2::ggplot(d, ggplot2::aes(column, row)) +
              ggplot2::geom_tile(ggplot2::aes(fill = score)) +
              ggplot2::geom_contour(ggplot2::aes(z = score, linetype = ggplot2::after_stat(factor(level))),
                breaks = c(-input$cutoff, 0, input$cutoff), colour = style$axis, na.rm = TRUE) +
              ggplot2::geom_point(data = d[is.finite(d$score), , drop = FALSE], colour = style$axis,
                shape = 3, size = 1) +
              ggplot2::facet_wrap(~context, ncol = 2, labeller = ggplot2::label_wrap_gen(22)) +
              ggplot2::scale_x_continuous(breaks = thin_doses(seq_along(x), 3L),
                labels = function(breaks) x[breaks],
                guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 35)) +
              ggplot2::scale_y_continuous(breaks = thin_doses(seq_along(y), 5L),
                labels = function(breaks) y[breaks], guide = ggplot2::guide_axis(check.overlap = TRUE)) +
              ggplot2::scale_fill_gradient2(low = "#287FAE", mid = "#F5F2ED", high = "#6B1E58", na.value = "#9CA3AF") +
              ggplot2::labs(title = "Contornos de interacción", subtitle = "Interpolación lineal local · cruces: dosis evaluables",
                x = paste("Dosis A", input$unit), y = paste("Dosis B", input$unit),
                fill = "Exceso (pp)", linetype = "Nivel (pp)") + theme
          },
          raincloud = ,
          ridgeline = {
            valid <- d[is.finite(d$score), , drop = FALSE]
            if (!nrow(valid)) return(nd("Sin celdas evaluables"))
            cloud <- identical(id, "raincloud")
            density <- ldj_synergy_density(valid$score, valid$experiment_id,
              adjust = pref("gallery_density_adjust", 1, c(.2, 4)), height = pref("gallery_density_height", .72, c(.15, .9)) * if (cloud) .45 else 1)
            valid$index <- match(valid$experiment_id, density$levels)
            curves <- density$curves
            plot <- ggplot2::ggplot(valid) +
              ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = style$axis)
            if (!is.null(curves)) plot <- plot + ggplot2::geom_ribbon(data = curves,
              ggplot2::aes(x, ymin = index, ymax = index + offset, fill = group),
              alpha = .3, colour = style$axis)
            plot <- plot + ggplot2::geom_point(ggplot2::aes(score, index - .13, colour = experiment_id),
                position = ggplot2::position_jitter(width = 0, height = .08, seed = 11),
                alpha = pref("gallery_density_alpha", .75, c(.15, 1)),
                size = pref("gallery_density_points", 1.8, c(.4, 5))) +
              ggplot2::stat_summary(ggplot2::aes(score, index - .27, group = experiment_id),
                fun = stats::median, orientation = "y", geom = "point", shape = 18,
                colour = style$axis, size = style$size * 1.5) +
              ggplot2::scale_y_continuous(breaks = seq_along(density$levels), labels = density$levels) +
              ggplot2::scale_fill_manual(values = ldj_series_colors(state, density$levels), guide = "none") +
              ggplot2::scale_colour_manual(values = ldj_series_colors(state, density$levels), guide = "none")
            plot + ggplot2::labs(title = if (cloud) "Distribución con observaciones" else "Densidades por experimento",
              subtitle = "Celdas descriptivas · rombo: mediana", x = "Exceso (pp)", y = NULL) + theme
          },
          distribution = {
            valid <- d[is.finite(d$score), , drop = FALSE]
            if (!nrow(valid)) return(nd("Sin celdas evaluables"))
            ggplot2::ggplot(valid, ggplot2::aes(experiment_id, score)) +
              ggplot2::geom_hline(yintercept = 0, linetype = 2) +
              ggplot2::geom_boxplot(outlier.shape = NA, fill = "#DCE9F0") +
              ggplot2::geom_point(position = ggplot2::position_jitter(width = .12, height = 0, seed = 11),
                alpha = .45, size = 1.2, colour = "#287FAE") +
              ggplot2::labs(title = "Distribución por experimento", subtitle = "Observaciones: celdas descriptivas",
                x = NULL, y = "Exceso (pp)") + theme
          },
          benefit = if (!is.null(activity_table())) benefit_panels()[[5]] else nd("Sin tablas compatibles"),
          pareto = {
            if (is.null(activity_table())) return(nd("Sin tablas compatibles"))
            block <- benefit()
            block$non_dominated <- ldj_synergy_pareto(block)
            block <- block[!is.na(block$non_dominated), , drop = FALSE]
            if (!nrow(block)) return(nd("Sin pares de combinación compatibles"))
            block$assay <- interaction(block[c("compound_A", "compound_B", "cell_line", "exposure_hours", "experiment_id")],
              drop = TRUE, sep = " · ")
            block$frontier <- ifelse(block$non_dominated, "Frontera", "Dominado")
            ggplot2::ggplot(block, ggplot2::aes(activity_pct, viability_pct, shape = frontier, colour = frontier)) +
              ggplot2::geom_point(size = 2.5) + ggplot2::facet_wrap(~assay, ncol = 2,
                labeller = ggplot2::label_wrap_gen(width = 30)) +
              ggplot2::scale_shape_manual(values = c(Frontera = 17, Dominado = 1)) +
              ggplot2::scale_colour_manual(values = c(Frontera = "#148B83", Dominado = "#59606A")) +
              ggplot2::labs(title = "Frontera de Pareto", subtitle = "Maximizar inhibición y viabilidad in vitro",
                x = "Inhibición (%)", y = "Viabilidad (%)", shape = NULL, colour = NULL) + theme +
              ggplot2::theme(legend.position = "bottom")
          })
      }
      if (input$template %in% names(gallery_presets)) {
        types <- gallery_presets[[input$template]]
        shiny::validate(shiny::need(length(types) > 0L, "Seleccione entre 1 y 12 gráficos con identificadores válidos."))
        panels <- if (identical(input$template, "custom"))
          Map(build_gallery, unname(types), names(types)) else lapply(types, build_gallery)
      } else panels <- switch(input$template,
        class = list(mono_overview,
          heat("observed", "Inhibición observada"), selected_heat, efficacy, forest, map),
        integral = list(mono_a, mono_b, heat("observed", "Inhibición observada"),
          heat("HSA_excess", "HSA", TRUE), heat("Bliss_excess", "Bliss", TRUE),
          if (any(is.finite(d$Loewe_excess))) heat("Loewe_excess", "Loewe", TRUE)
          else placeholder("Loewe", "ND · Sin estimaciones válidas"),
          placeholder("ZIP", "ND · Disponible en el análisis experimental"),
          efficacy, forest, agreement_map,
          linked[[2]] + ggplot2::labs(subtitle = "Tabla de beneficio farmacológico"),
          linked[[4]] + ggplot2::labs(subtitle = "Tabla de beneficio farmacológico")),
        benefit = list(linked[[1]], linked[[2]], linked[[3]],
          placeholder("Interacción vinculada", "ND · Falta verificar la correspondencia de los ensayos"),
          linked[[4]], linked[[5]]),
        list(map, score))
      names(panels) <- LETTERS[seq_along(panels)]
      panel_tags <- stats::setNames(names(panels), names(panels))
      for (id in names(panels)) {
        overrides <- panel_overrides()[[panel_key(input$template, id, gallery_presets$custom)]]
        tag <- id
        margin <- ggplot2::margin(8, 10, 7, 8)
        if (!is.null(overrides)) {
          labels <- overrides[intersect(c("title", "subtitle", "x", "y"), names(overrides))]
          labels <- labels[vapply(labels, nzchar, logical(1))]
          if (length(labels)) panels[[id]] <- panels[[id]] + do.call(ggplot2::labs, labels)
          if (nzchar(trimws(overrides$tag))) tag <- trimws(overrides$tag)
          font <- suppressWarnings(as.numeric(overrides$font %||% 0))
          edge <- suppressWarnings(as.numeric(overrides$margin %||% 0))
          if (is.finite(font) && font > 0 && font <= 24)
            panels[[id]] <- panels[[id]] + ggplot2::theme(
              plot.title = ggplot2::element_text(size = font),
              plot.subtitle = ggplot2::element_text(size = font * .85),
              axis.text = ggplot2::element_text(size = font * .8),
              axis.title = ggplot2::element_text(size = font * .9),
              strip.text = ggplot2::element_text(size = font * .8),
              legend.text = ggplot2::element_text(size = font * .8))
          if (is.finite(edge) && edge > 0 && edge <= 24)
            margin <- ggplot2::margin(edge, edge, edge, edge)
          panel_number <- function(name, lower, upper, default) {
            value <- suppressWarnings(as.numeric(overrides[[name]] %||% default))
            if (!is.finite(value) || value < lower || value > upper) default else value
          }
          gap <- panel_number("gap", -1, 24, -1)
          if (gap >= 0) panels[[id]] <- panels[[id]] +
            ggplot2::theme(panel.spacing = grid::unit(gap, "pt"))
          if (isTRUE(overrides$family %in% c("sans", "serif", "mono")))
            panels[[id]] <- panels[[id]] + ggplot2::theme(text = ggplot2::element_text(family = overrides$family))
          for (axis in c("x", "y")) {
            n <- panel_number(paste0("ticks_", axis), 0, 12, 0)
            if (n > 0 && !inherits(panels[[id]], "patchwork")) panels[[id]] <- local({
              count <- as.integer(n)
              scale <- panels[[id]]$scales$get_scales(axis)
              if (!is.null(scale)) {
                scale <- scale$clone()
                previous <- scale$breaks
                scale$breaks <- if (is.numeric(previous)) thin_doses(previous, count) else if (scale$is_discrete())
                  function(x) thin_doses(x, count) else scales::breaks_extended(n = count)
                if (is.character(scale$labels) && length(scale$labels) > 1L) scale$labels <- ggplot2::waiver()
                suppressMessages(panels[[id]] + scale)
              } else panels[[id]]
            })
          }
          width <- panel_number("min_width", 0, 4, 0)
          height <- panel_number("min_height", 0, 4, 0)
          if (!inherits(panels[[id]], "patchwork")) {
            if (width > 0) panels[[id]] <- panels[[id]] + ggplot2::theme(panel.widths = grid::unit(width, "in"))
            if (height > 0) panels[[id]] <- panels[[id]] + ggplot2::theme(panel.heights = grid::unit(height, "in"))
          }
          if (isTRUE(overrides$legend %in% c("bottom", "right", "none")))
            panels[[id]] <- panels[[id]] + ggplot2::theme(legend.position = overrides$legend)
        }
        panel_tags[[id]] <- tag
        wrap_label <- function(x, width) {
          if (is.null(x)) return(NULL)
          paste(strwrap(x, width = width), collapse = "\n")
        }
        labels <- panels[[id]]$labels
        panels[[id]] <- panels[[id]] + ggplot2::labs(tag = tag,
          title = wrap_label(labels$title, 32), subtitle = wrap_label(labels$subtitle, 38)) +
          ggplot2::theme(plot.margin = margin, plot.title.position = "plot")
      }
      order <- trimws(strsplit(toupper(input$panel_order), ",", fixed = TRUE)[[1L]])
      if (!nzchar(trimws(input$panel_order))) order <- names(panels)
      shiny::validate(shiny::need(length(order) > 0 && !anyDuplicated(order) &&
        all(order %in% names(panels)), "Indique las letras válidas sin repetirlas."))
      shiny::validate(shiny::need(!anyDuplicated(panel_tags[order]),
        "Los identificadores de los paneles visibles deben ser únicos."))
      panels[order]
    })
    figure <- shiny::reactive({
      panels <- figure_panels()
      # Isolate the full grobs so a wide forest axis or fixed map does not
      # reserve the same axis or legend columns in unrelated plots.
      independent <- lapply(panels, function(panel) patchwork::wrap_elements(full = panel, clip = FALSE))
      split_rows <- function(row_heights, wide_row) {
        rows <- lapply(seq_along(row_heights), function(i) {
          indices <- c(2L * i - 1L, 2L * i)
          patchwork::wrap_plots(independent[indices], nrow = 1L,
            widths = if (i == wide_row) c(1, 1.65) else c(1, 1))
        })
        patchwork::wrap_plots(rows, ncol = 1L, heights = row_heights)
      }
      if (identical(input$template, "custom") && nzchar(trimws(input$gallery_layout %||% ""))) {
        design <- tryCatch(ldj_synergy_gallery_design(input$gallery_layout, length(panels)),
          error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(design), "La matriz debe incluir todos los paneles A–L utilizados, cada uno en un rectángulo."))
        rows <- strsplit(design, "\n", fixed = TRUE)[[1L]]
        weights <- tryCatch(list(
          widths = ldj_synergy_gallery_weights(input$gallery_widths %||% "", nchar(rows[1L])),
          heights = ldj_synergy_gallery_weights(input$gallery_heights %||% "", length(rows))),
          error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(weights),
          "Indique una proporción positiva para cada columna o fila del diseño."))
        layout <- patchwork::wrap_plots(independent, design = design,
          widths = weights$widths, heights = weights$heights)
      } else if (identical(names(panels), LETTERS[1:6]) && input$template == "class" && input$figure_columns == 2L) {
        layout <- split_rows(c(1, 1.2, 1.35), 3L)
      } else if (identical(names(panels), LETTERS[1:12]) && input$template == "integral" && input$figure_columns == 2L) {
        layout <- split_rows(c(1, 1, .9, 1.2, 1.4, 1.35), 5L)
      } else {
        layout <- patchwork::wrap_plots(independent, ncol = min(length(panels), input$figure_columns))
      }
      layout +
        patchwork::plot_annotation(title = paste(if(simulated()) "DEMO — DATOS SIMULADOS" else
          "Interacciones descriptivas", input$title),
          caption = if (identical(input$template, "class")) paste("Umbral operativo ±", input$cutoff, "pp") else NULL)
    })
    export_figure <- function() {
      if (!nzchar(input$panel_export)) return(figure())
      panels <- figure_panels()
      shiny::validate(shiny::need(input$panel_export %in% names(panels), "Seleccione un panel visible."))
      panels[[input$panel_export]]
    }
    export_dimensions <- function(single = nzchar(input$panel_export), panel_id = input$panel_export,
                                  preview = FALSE) {
      n <- if (single) 1L else length(figure_panels())
      reference <- ldj_synergy_figure_dimensions(n, input$figure_columns)
      gallery <- input$template %in% c("landscape", "concordance", "reproducibility", "optimization", "custom")
      if (gallery && !single) {
        rows <- ceiling(n / input$figure_columns)
        cols <- min(n, input$figure_columns)
        if (identical(input$template, "custom") && nzchar(trimws(input$gallery_layout %||% ""))) {
          layout <- tryCatch(ldj_synergy_gallery_design(input$gallery_layout, n), error = function(e) "")
          lines <- strsplit(layout, "\n", fixed = TRUE)[[1L]]
          if (length(lines)) {
            rows <- length(lines)
            cols <- nchar(lines[1L])
          }
        }
        reference <- c(width = max(8, 5.6 * cols), height = max(6, 4.2 * rows + .4))
      }
      panels <- figure_panels()
      if (single) panels <- panels[panel_id]
      has_minimum <- vapply(panels, function(panel) {
        !is.null(panel$theme$panel.widths) || !is.null(panel$theme$panel.heights)
      }, logical(1))
      minimum <- c(width = 0, height = 0)
      if (any(has_minimum)) {
        room <- vapply(panels[has_minimum], function(panel) {
          slots <- ggplot2::ggplot_build(panel)$layout$layout
          fixed_space <- function(unit, n, trim) {
            if (is.null(unit)) 0 else as.numeric(unit) * n + trim
          }
          c(width = fixed_space(panel$theme$panel.widths, max(slots$COL), 1.5),
            height = fixed_space(panel$theme$panel.heights, max(slots$ROW), 1.15))
        }, c(width = 0, height = 0))
        cols <- if (single) 1L else min(n, input$figure_columns)
        rows <- ceiling(n / cols)
        minimum <- apply(room, 1L, max) * c(cols, rows)
      }
      measured <- lapply(panels, function(panel) {
        if (inherits(panel, "patchwork")) return(c(width = 4.3, height = 3.2))
        built <- ggplot2::ggplot_build(panel)
        gt <- ggplot2::ggplot_gtable(built)
        slots <- built$layout$layout
        absolute <- function(x, convert) convert(sum(x), "in", valueOnly = TRUE)
        draw_width <- if (is.null(panel$theme$panel.widths)) 1.15 * max(slots$COL) else 0
        draw_height <- if (is.null(panel$theme$panel.heights)) .85 * max(slots$ROW) else 0
        c(width = draw_width + absolute(gt$widths, grid::convertWidth),
          height = draw_height + absolute(gt$heights, grid::convertHeight))
      })
      measured <- do.call(rbind, measured)
      if (!single && identical(input$template, "custom") && nzchar(trimws(input$gallery_layout %||% ""))) {
        layout <- strsplit(ldj_synergy_gallery_design(input$gallery_layout, n), "\n", fixed = TRUE)[[1L]]
        cells <- do.call(rbind, strsplit(layout, "", fixed = TRUE))
        shares <- function(value, count) {
          w <- ldj_synergy_gallery_weights(value %||% "", count)
          if (is.null(w)) w <- rep(1, count)
          w / sum(w)
        }
        xs <- shares(input$gallery_widths, ncol(cells))
        ys <- shares(input$gallery_heights, nrow(cells))
        for (i in seq_len(n)) {
          pos <- which(cells == LETTERS[i], arr.ind = TRUE)
          measured[i, ] <- measured[i, ] / c(sum(xs[unique(pos[,2])]), sum(ys[unique(pos[,1])]))
        }
      } else if (!single) {
        cols <- min(n, input$figure_columns)
        measured <- sweep(measured, 2L, c(cols, ceiling(n / cols)), "*")
      }
      minimum <- pmax(minimum, apply(measured, 2L, max) + c(.20, .45))
      if (!preview && input$size_preset == "custom") {
        inches <- c("in" = 1, cm = 1 / 2.54, mm = 1 / 25.4)
        size <- c(width = input$size_width, height = input$size_height) * inches[[input$size_units]]
        shiny::validate(shiny::need(all(is.finite(size) & size > 0 & size <= 30),
          "Use dimensiones positivas de hasta 30 pulgadas equivalentes."))
        if (!single || nzchar(input$panel_export))
          shiny::validate(shiny::need(all(size >= minimum),
            "Aumente el lienzo o reduzca las dimensiones mínimas de las facetas."))
        return(size)
      }
      target <- if (preview) reference[["width"]] else
        switch(input$size_preset, one = 3.5, two = 7, page = 8.3, reference[["width"]])
      size <- pmax(reference * (target / reference[["width"]]), minimum)
      shiny::validate(shiny::need(all(size <= 30),
        "Las dimensiones mínimas requieren un lienzo mayor a 30 pulgadas."))
      size
    }
    output$size_summary <- shiny::renderText({
      size <- export_dimensions()
      n <- if (nzchar(input$panel_export)) 1L else length(figure_panels())
      standard <- ldj_synergy_figure_dimensions(n, input$figure_columns)
      note <- if (any(size < standard * 0.75)) " · Revise legibilidad: tamaño reducido para esta distribución." else ""
      sprintf("%.2f × %.2f in%s", size[["width"]], size[["height"]], note)
    })
    preview_panel <- shiny::reactive({
      selected <- input$preview_panel %||% ""
      if (selected %in% names(figure_panels())) selected else ""
    })
    preview_pixels <- shiny::reactive({
      selected <- preview_panel()
      ceiling(96 * export_dimensions(single = nzchar(selected), panel_id = selected, preview = TRUE))
    })
    output$plot <- shiny::renderPlot({
      selected <- preview_panel()
      if (nzchar(selected)) figure_panels()[[selected]] else figure()
    }, res = 96, width = function() preview_pixels()[["width"]],
      height = function() preview_pixels()[["height"]])
    evidence <- shiny::reactive({
      d <- ldj_synergy_inference(classified(), isTRUE(input$independence))
      d <- d[d$contrast %in% c("Bliss_excess", "superior_to_both"), , drop = FALSE]
      d$evidence <- ifelse(!is.finite(d$p_holm), "ND",
        ifelse(d$p_holm >= 0.05, "No concluyente",
          ifelse(d$contrast == "superior_to_both", "Superioridad frente a ambas monoterapias",
            "Exceso Bliss positivo")))
      d
    })
    output$interaction_evidence <- shiny::renderTable(evidence(), digits = 5, na = "ND")
    output$png300 <- shiny::downloadHandler(filename = function() "LDJ-synergy-300dpi.png",
      content = function(path) {
        size <- export_dimensions()
        ggplot2::ggsave(path, export_figure(), device = "png", width = size[["width"]],
          height = size[["height"]], dpi = 300)
      })
    output$inference_csv <- shiny::downloadHandler(filename=function() "LDJ-contrastes.csv",
      content=function(path) utils::write.csv(
        ldj_synergy_inference(classified(),isTRUE(input$independence)),path,row.names=FALSE))
    output$csv <- shiny::downloadHandler(filename=function() "LDJ-synergy.csv",
      content=function(path) utils::write.csv(classified(),path,row.names=FALSE))
    for(fmt in c("svg","pdf","png")) local({
      format <- fmt
      output[[format]] <- shiny::downloadHandler(filename=function() paste0("LDJ-synergy.",format),
        content=function(path) {
          size <- export_dimensions()
          ggplot2::ggsave(path,export_figure(),device=format,width=size[["width"]],height=size[["height"]],dpi=600)
        })
    })
  })
}
