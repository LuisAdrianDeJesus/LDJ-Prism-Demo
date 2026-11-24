Sys.unsetenv("LDJ_PRISM_MODE")
chrome <- Sys.which("google-chrome")
if(nzchar(chrome)) Sys.setenv(CHROMOTE_CHROME=chrome)
app <- shinytest2::AppDriver$new(app_dir=".",name="startup",load_timeout=90000)
tryCatch({
  app$wait_for_js("Boolean(document.querySelector('#plot img') && document.querySelector('#plot img').naturalWidth > 0)",timeout=60000)
  stopifnot(app$get_value(input="x")=="Concentracion_uM",
    app$get_value(input="y")=="Viabilidad_pct",app$get_value(input="group")=="Compuesto",
    app$get_js("document.querySelectorAll('.shiny-output-error').length") == 0)
  cat("Browser first-load render with no manual inputs: PASS\n")
  app$set_inputs(plot_title="Synthetic startup test",x_label="Dose",y_label="Response",palette="Viridis")
  app$wait_for_idle()
  stopifnot(app$get_js("document.querySelectorAll('.shiny-output-error').length")==0)
  for(kind in c("bar","line")) {
    app$set_inputs(kind=kind,multipanel=TRUE)
    app$wait_for_idle()
    stopifnot(app$get_js("document.querySelectorAll('.shiny-output-error').length")==0)
    app$set_inputs(inspector="Exportar")
    app$wait_for_idle()
    for(fmt in c("svg","pdf")) {
      f <- tempfile(fileext=paste0(".",fmt))
      app$get_download(fmt,filename=f)
      stopifnot(file.info(f)$size>0); unlink(f)
    }
  }
  cat("Browser synthetic controls, bars, log curves, multipanel and SVG/PDF: PASS\n")
  app$set_inputs(inspector="Gráfica",module="mtt")
  app$wait_for_idle(timeout=90000)
  app$wait_for_js("Boolean(document.querySelector('#plot img') && document.querySelector('#plot img').naturalWidth > 0)",timeout=90000)
  stopifnot(app$get_js("document.querySelectorAll('.shiny-output-error').length")==0)
  app$set_inputs(inspector="Editor de paneles",mtt_target="A",mtt_title="A. Browser panel title",mtt_hex="#123456",mtt_note="Synthetic annotation")
  app$click("mtt_apply"); app$wait_for_idle()
  stopifnot(app$get_js("document.querySelectorAll('.shiny-output-error').length")==0)
  app$set_inputs(inspector="Exportar")
  for(fmt in c("svg","pdf")) {
    f <- tempfile(fileext=paste0(".",fmt)); app$get_download(fmt,filename=f)
    stopifnot(file.info(f)$size>10000)
    if(fmt=="svg") {
      svg <- readLines(f,warn=FALSE)
      stopifnot(any(grepl("Browser panel title",svg,fixed=TRUE)),sum(grepl("ANOVA F",svg,fixed=TRUE))==4)
    }
    unlink(f)
  }
  cat("Browser MTT 8-panel editor, per-panel title/HEX/annotation and vector exports: PASS\n")
  app$set_inputs(inspector="Datos",entry_type="parameters")
  app$click("blank_table")
  app$wait_for_js("document.querySelector('#grid').textContent.includes('estimate')",timeout=30000)
  stopifnot(app$get_value(input="module")=="safety")
  app$run_js("var hot = HTMLWidgets.find('#grid').hot; hot.setDataAtCell([[0,0,'Manual'],[0,1,'IC50'],[0,2,'0.4'],[0,5,'uM']], 'edit');")
  app$wait_for_idle()
  app$click("confirm_table")
  app$wait_for_idle()
  app$click("run_analysis")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  param <- tempfile(fileext=".svg"); app$get_download("svg",filename=param)
  stopifnot(any(grepl("IC50",readLines(param,warn=FALSE),fixed=TRUE))); unlink(param)
  dir.create("test-artifacts",showWarnings=FALSE)
  app$set_inputs(inspector="Datos",entry_type="observations")
  app$click("blank_table")
  app$wait_for_js("document.querySelector('#grid').textContent.includes('concentration')",timeout=30000)
  app$get_screenshot("test-artifacts/manual-data.png")
  app$run_js("var hot = HTMLWidgets.find('#grid').hot; hot.setDataAtCell([[0,0,'1'],[0,1,'75'],[0,2,'Manual']], 'edit');")
  app$wait_for_idle()
  app$click("confirm_table")
  app$set_inputs(module="universal",kind="scatter",multipanel=FALSE)
  stopifnot(app$get_value(input="x")=="concentration",app$get_value(input="y")=="response")
  app$click("run_analysis")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  app$set_inputs(inspector="Exportar")
  f <- tempfile(fileext=".svg"); app$get_download("svg",filename=f)
  stopifnot(file.info(f)$size>0); unlink(f)
  app$get_screenshot("test-artifacts/manual-figure.png")
  cat("Browser blank parameter and observation grids, module navigation and manual SVG: PASS\n")
  capture <- function(name) {
    png <- app$get_chromote_session()$Page$captureScreenshot(format="png",timeout_=60)
    writeBin(jsonlite::base64_dec(png$data),file.path("test-artifacts",name))
  }
  app$set_inputs(inspector="Datos",entry_type="observations")
  app$click("blank_table")
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.getColHeader(3) === 'replicate' && !HTMLWidgets.find('#grid').hot.getDataAtCell(0,0)",timeout=30000)
  fixture <- expand.grid(concentration=c(.125,.25,.5,1,2,4,8),replicate=1:3,
    group=paste0("Manual S",1:4),KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  fixture$response <- with(fixture, 5+95/(1+(concentration/as.numeric(sub("Manual S","",group)))^1.4)+replicate-2)
  rows <- lapply(seq_len(nrow(fixture)),function(i) lapply(fixture[i,c("concentration","response","group","replicate")],as.character))
  rows <- lapply(rows,unname)
  app$run_js(sprintf("var hot=HTMLWidgets.find('#grid').hot; var rows=%s; hot.alter('insert_row',0,rows.length-1); hot.populateFromArray(0,0,rows,undefined,undefined,'edit');",
    jsonlite::toJSON(rows,auto_unbox=TRUE)))
  app$wait_for_idle(timeout=60000)
  capture("mtt-manual-grid.png")
  # Exercise the Posit failure path: a pasted treatment-only table has no control ID columns.
  treatments <- fixture[c("concentration","response","group","replicate")]
  treatment_tsv <- paste(capture.output(write.table(treatments,sep="\t",
    row.names=FALSE,quote=FALSE)),collapse="\n")
  app$set_inputs(paste_tsv=treatment_tsv)
  app$click("apply_paste")
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.countCols() === 4",timeout=30000)
  app$click("confirm_table")
  app$set_inputs(module="mtt",mtt_view="editorial",model_reason="Synthetic test")
  app$set_inputs(mtt_target="A")
  app$click("mtt_reset")
  app$click("run_analysis")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  app$set_inputs(inspector="Análisis")
  app$wait_for_js("document.querySelector('#mtt_run_status').textContent.includes('vigente')",timeout=30000)
  app$set_inputs(inspector="Datos")
  app$wait_for_js("Number(HTMLWidgets.find('#grid')?.hot?.getDataAtCell(0,0)) === 0.125",timeout=30000)
  app$run_js("HTMLWidgets.find('#grid').hot.setDataAtCell(0,1,90,'edit');")
  app$wait_for_idle()
  app$click("confirm_table")
  app$wait_for_js("document.querySelector('#mtt_run_status').textContent.includes('modificados')",timeout=30000)
  capture("mtt-manual-invalidated.png")
  app$click("run_analysis")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  app$set_inputs(inspector="Exportar",width=8,height=12,dpi="600")
  for(fmt in c("svg","pdf","png")) {
    file <- file.path("test-artifacts",paste0("mtt-manual.",fmt))
    app$get_download(fmt,filename=file)
    stopifnot(file.info(file)$size>10000)
    if(fmt=="svg") {
      svg <- readLines(file,warn=FALSE)
      stopifnot(all(vapply(LETTERS[1:8],function(tag) any(grepl(paste0(">",tag,"[. ]"),svg)),logical(1))),
        any(grepl("Manual S4",svg,fixed=TRUE)),
        any(grepl("CC50",svg,fixed=TRUE)),
        any(grepl("Sin comparaciones relativas al control",svg,fixed=TRUE)))
    }
  }
  capture("mtt-manual-figure.png")
  app$set_inputs(module="activity",inspector="Análisis")
  app$click("run_analysis")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  app$set_inputs(inspector="Exportar")
  file <- file.path("test-artifacts","manual-activity.svg")
  app$get_download("svg",filename=file)
  stopifnot(any(grepl("Manual S4",readLines(file,warn=FALSE),fixed=TRUE)))
  app$set_inputs(inspector="Datos")
  app$wait_for_js("Number(HTMLWidgets.find('#grid')?.hot?.getDataAtCell(0,1)) === 90",timeout=30000)
  mixed <- data.frame(concentration=fixture$concentration,response=fixture$response,
    group=fixture$group,replicate=fixture$replicate,parameter="",estimate=NA_real_,unit="")
  mixed$response[1] <- 90
  mixed$lower <- mixed$upper <- NA_real_
  parameters <- expand.grid(group=paste0("Manual S",1:4),parameter=c("IC50","CC20","CC50"),
    stringsAsFactors=FALSE,KEEP.OUT.ATTRS=FALSE)
  parameters$concentration <- parameters$response <- parameters$replicate <- NA_real_
  parameters$estimate <- rep(c(1,.1,8),each=4)
  parameters$unit <- "uM"
  parameters$lower <- .8*parameters$estimate
  parameters$upper <- 1.2*parameters$estimate
  tsv <- paste(capture.output(write.table(rbind(mixed,parameters),sep="\t",row.names=FALSE,quote=FALSE,na="")),collapse="\n")
  app$set_inputs(paste_tsv=tsv)
  app$click("apply_paste")
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.getColHeader(4) === 'parameter'",timeout=30000)
  app$click("confirm_table")
  app$set_inputs(module="safety",safety_table=TRUE,safety_display="editorial")
  app$click("run_analysis")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  app$set_inputs(inspector="Exportar")
  for(source in c("regression","table","both")) {
    app$set_inputs(cc20_source=source)
    file <- file.path("test-artifacts",paste0("safety-",source,".svg"))
    app$get_download("svg",filename=file)
    svg <- paste(readLines(file,warn=FALSE),collapse=" ")
    stopifnot(grepl("Manual S4",svg,fixed=TRUE),
      grepl("CC20 · Ajuste",svg,fixed=TRUE)==(source!="table"),
      grepl("CC20 · Tabla",svg,fixed=TRUE)==(source!="regression"))
    if(source=="both") stopifnot(grepl("Discrepancia:",svg,fixed=TRUE),
      grepl("Advertencia: CC20 &lt; IC50",svg,fixed=TRUE))
  }
  for(fmt in c("pdf","png")) {
    file <- file.path("test-artifacts",paste0("safety-both.",fmt))
    app$get_download(fmt,filename=file)
    stopifnot(file.info(file)$size>10000)
  }
  app$set_inputs(inspector="Datos",module="mtt",unit="nM")
  app$click("load_example")
  app$wait_for_idle()
  stopifnot(app$get_value(input="x")=="Concentracion_uM",app$get_value(input="y")=="Viabilidad_pct",
    app$get_value(input="group")=="Compuesto",app$get_value(input="unit")=="uM",
    !app$get_value(input="mtt_user_data"),!app$get_value(input="safety_table"))
  cat("Browser manual four-series MTT, edit/recalculate/export, activity, safety and example reset: PASS\n")
  # Distinct second run with physical, traceable, independent controls.
  app$click("blank_table")
  with_controls <- rbind(treatments,
    expand.grid(concentration=0,response=c(99,100,101),group=paste0("Manual S",1:4),
      KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE) |> transform(replicate=rep(1:3,4)) |>
      (function(d) d[names(treatments)])())
  with_controls$control_type <- ifelse(with_controls$concentration==0,"untreated","treatment")
  with_controls$observation_id <- paste0("QA-",seq_len(nrow(with_controls)))
  with_controls$independent_unit_id <- with_controls$observation_id
  with_controls$experiment_id <- "QA"
  with_controls$control_source <- "Synthetic fixture"
  with_controls$inference_eligible <- with_controls$concentration==0
  block <- paste(capture.output(write.table(with_controls,sep="\t",row.names=FALSE,
    quote=FALSE)),collapse="\n")
  app$set_inputs(paste_tsv=block)
  app$click("apply_paste")
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.countRows()===96",timeout=30000)
  app$click("confirm_table")
  app$set_inputs(x="concentration",y="response",group="group",unit="uM",mtt_inference="anova",mtt_reference="untreated",independent=TRUE)
  app$click("run_analysis")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  app$set_inputs(inspector="Exportar")
  file <- file.path("test-artifacts","mtt-physical-controls.svg")
  app$get_download("svg",filename=file)
  svg <- readLines(file,warn=FALSE)
  stopifnot(any(grepl("No tratado",svg,fixed=TRUE)),sum(grepl("ANOVA F",svg,fixed=TRUE))==4,
    any(grepl(">****<",svg,fixed=TRUE)))
  app$set_inputs(inspector="Análisis")
  report <- file.path("test-artifacts","mtt-anova-dunnett.json")
  app$get_download("mtt_statistics",filename=report)
  app$set_inputs(inspector="Colores",palette="Viridis")
  app$click("palette_apply")
  app$set_inputs(inspector="Análisis")
  again <- tempfile(fileext=".json")
  app$get_download("mtt_statistics",filename=again)
  stopifnot(identical(readLines(report),readLines(again)))
  app$set_inputs(inspector="Exportar",dpi="600",width=8,height=12)
  for(fmt in c("pdf","png")) {
    export <- file.path("test-artifacts",paste0("mtt-physical-controls.",fmt))
    app$get_download(fmt,filename=export)
    stopifnot(file.info(export)$size>10000)
    if(fmt=="png") {
      con <- file(export,"rb")
      seek(con,16)
      dimensions <- readBin(con,integer(),2,size=4,endian="big")
      close(con)
      stopifnot(identical(dimensions,c(4800L,7200L)))
    }
  }
  capture("mtt-physical-controls-browser.png")
  app$set_inputs(inspector="Análisis",registry_assay="Synthetic viability QA",
    registry_conditions="Synthetic cell line; 24 h; percent response")
  app$click("registry_save")
  app$set_inputs(module="activity",inspector="Análisis",activity_control_type="untreated",
    experiment_id="QA",independent=TRUE,model_reason="Synthetic 4PL regression fixture")
  app$click("run_analysis")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  capture("activity-eligible-controls.png")
  app$set_inputs(inspector="Exportar")
  file <- file.path("test-artifacts","activity-eligible-controls.svg")
  app$get_download("svg",filename=file)
  svg <- readLines(file,warn=FALSE)
  stopifnot(any(grepl("ANOVA F",svg,fixed=TRUE)),any(grepl("IC50",svg,fixed=TRUE)),
    any(grepl("IC95",svg,fixed=TRUE)))
  cat("Traceable MTT controls and activity inference with eligible controls: PASS\n")
  app$set_inputs(inspector="Análisis",registry_assay="Synthetic activity QA",
    registry_conditions="Synthetic cell line; 24 h; percent response")
  app$click("registry_save")
  app$set_inputs(module="safety",inspector="Análisis")
  app$wait_for_idle()
  app$set_inputs(registry_mtt="mtt-1",registry_activity="activity-2",registry_confirm=TRUE)
  registry_file <- file.path("test-artifacts","analytical-registry.json")
  app$get_download("registry_export",filename=registry_file)
  registry <- jsonlite::read_json(registry_file)
  stopifnot(identical(names(registry),c("mtt-1","activity-2")))
  app$click("registry_integrate")
  app$wait_for_idle(timeout=90000)
  stopifnot(app$get_value(input="unit")=="M",app$get_value(input="safety_display")=="editorial")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=90000)
  capture("editorial-integrated.png")
  for(example in c("mtt","activity","safety")) {
    app$set_inputs(module=example,inspector="Datos")
    app$click("load_example")
    app$wait_for_idle(timeout=90000)
    for(fmt in c("csv","xlsx")) {
      path <- file.path("test-artifacts",paste0("data-",example,".",fmt))
      app$get_download(paste0("example_",fmt),filename=path)
      data <- if(fmt=="csv") read.csv(path) else readxl::read_xlsx(path)
      stopifnot(length(unique(data$Compuesto))==4,nrow(data)>=128)
    }
    app$set_inputs(inspector="Colores")
    app$click("colors_reset")
    app$wait_for_idle()
    stopifnot(app$get_value(input="palette")=="LDJ Editorial")
    app$set_inputs(inspector="Gráfica")
    app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=90000)
    capture(paste0("editorial-example-",example,".png"))
    app$set_inputs(inspector="Exportar")
    for(fmt in c("svg","pdf","png")) {
      file <- file.path("test-artifacts",paste0("editorial-example-",example,".",fmt))
      app$get_download(fmt,filename=file)
      stopifnot(file.info(file)$size>10000)
      if(fmt=="svg") {
        svg <- readLines(file,warn=FALSE)
        marker <- if(example=="safety") "80–100% viable" else "ANOVA F"
        metric <- if(example=="mtt") "CC50" else "IC50"
        stopifnot(sum(grepl(marker,svg,fixed=TRUE))>=4,
          sum(grepl(metric,svg,fixed=TRUE))>=4)
      }
    }
    app$set_inputs(inspector="Ejes",x_label="Concentración (µM)")
    app$set_inputs(inspector="Editor de paneles",figure_language="en")
    app$set_inputs(inspector="Exportar")
    language_svg <- tempfile(fileext=".svg")
    app$get_download("svg",filename=language_svg)
    labels <- readLines(language_svg,warn=FALSE)
    stopifnot(any(grepl("Concentration",labels,fixed=TRUE)))
    unlink(language_svg)
    app$set_inputs(figure_language="es")
    app$set_inputs(visual_preset="minimal")
    for(fmt in c("svg","pdf","png")) {
      file <- file.path("test-artifacts",paste0("minimal-example-",example,".",fmt))
      app$get_download(fmt,filename=file)
      stopifnot(file.info(file)$size>10000)
    }
    app$set_inputs(visual_preset="editorial")
  }
  app$set_inputs(module="mtt",inspector="Datos")
  app$wait_for_js("document.querySelector('#demo_indicator')?.textContent.includes('datos sintéticos')",timeout=60000)
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.getDataAtCell(0,2) === 'Compuesto A'",timeout=60000)
  capture("demo-grid-mtt.png")
  app$run_js("HTMLWidgets.find('#grid').hot.setDataAtCell(0,1,77,'edit')")
  app$wait_for_idle()
  app$click("confirm_table")
  app$wait_for_idle()
  stopifnot(!app$get_value(input="independent"))
  app$set_inputs(module="activity",inspector="Datos")
  app$wait_for_js("String(HTMLWidgets.find('#grid')?.hot?.getDataAtCell(0,1)) === '77'",timeout=60000)
  app$set_inputs(module="mtt",inspector="Datos")
  app$wait_for_js("String(HTMLWidgets.find('#grid')?.hot?.getDataAtCell(0,1)) === '77'",timeout=60000)
  app$click("restore_example")
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.getDataAtCell(0,2) === 'Compuesto A' && String(HTMLWidgets.find('#grid').hot.getDataAtCell(0,1)) !== '77'",timeout=60000)
  cat("Edited demonstration remains available when changing modules: PASS\n")
  cat("Synthetic editorial and minimal examples for all three modules: PASS\n")
},finally=app$stop())

