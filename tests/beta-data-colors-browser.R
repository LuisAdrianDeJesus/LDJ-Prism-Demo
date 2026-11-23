Sys.unsetenv("LDJ_PRISM_MODE")
chrome <- Sys.which("google-chrome")
if(nzchar(chrome)) Sys.setenv(CHROMOTE_CHROME=chrome)
dir.create("test-artifacts",showWarnings=FALSE)
style_path <- normalizePath("test-artifacts",mustWork=TRUE)
style_path <- file.path(style_path,"beta-style.json")
app <- shinytest2::AppDriver$new(app_dir=".",name="beta-data-colors",load_timeout=90000)
tryCatch({
  app$set_inputs(inspector="Datos",entry_type="observations")
  app$click("blank_table")
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.countRows()===1",timeout=30000)
  app$click("add_row")
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.countRows()===2",timeout=30000)
  app$run_js("HTMLWidgets.find('#grid').hot.alter('remove_row',1,1)")
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.countRows()===1",timeout=30000)
  app$set_inputs(column_names="dose,response,group,replicate,control_type,observation_id,experiment_id,control_source,inference_eligible")
  app$click("rename_columns")
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.getColHeader(0)==='dose'",timeout=30000)
  fixture <- data.frame(dose=c(1,2),response=c(85,60),group=c("QA-a","QA-b"))
  for (ext in c("csv","tsv","xlsx","xls")) {
    path <- tempfile(fileext=paste0(".",ext))
    if(ext=="xlsx") {
      writexl::write_xlsx(list(First=fixture,Second=transform(fixture,response=c(30,40))),path)
    } else if(ext=="xls") {
      file.copy(readxl::readxl_example("datasets.xls"),path)
    } else {
      write.table(fixture,path,sep=if(ext=="csv") "," else "\t",row.names=FALSE)
    }
    app$upload_file(upload=path)
    if(ext %in% c("xlsx","xls")) {
      sheets <- readxl::excel_sheets(path)
      app$set_inputs(sheet=sheets[[2]])
      d <- readxl::read_excel(path,sheet=sheets[[2]])
      expected <- names(d)[1]
      if(ext=="xlsx") app$wait_for_js("Number(HTMLWidgets.find('#grid')?.hot?.getDataAtCell(0,1))===30",timeout=30000)
    } else expected <- "dose"
    app$wait_for_js(sprintf("HTMLWidgets.find('#grid')?.hot?.getColHeader(0)===%s",
      jsonlite::toJSON(expected,auto_unbox=TRUE)),timeout=30000)
    unlink(path)
    cat("Browser upload",ext,": PASS\n")
  }
  app$upload_file(upload=local({
    path <- tempfile(fileext=".csv")
    write.csv(transform(fixture,alternative_dose=c(5,7),alternative_y=c(71,82),
      alternative_group=c("QA-c","QA-d")),path,row.names=FALSE)
    path
  }))
  app$wait_for_js("HTMLWidgets.find('#grid')?.hot?.getColHeader(5)==='alternative_group'",timeout=30000)
  app$set_inputs(x="alternative_dose",y="alternative_y",group="alternative_group",x_scale="linear")
  app$click("confirm_table")
  app$click("run_analysis")
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  app$set_inputs(inspector="Exportar")
  axes_svg <- tempfile(fileext=".svg")
  app$get_download("svg",filename=axes_svg)
  stopifnot(any(grepl("QA-d",readLines(axes_svg,warn=FALSE),fixed=TRUE)))
  app$set_inputs(inspector="Datos")
  app$click("load_example")
  app$set_inputs(inspector="Colores",visual_preset="minimal",palette="Viridis · semitransparente")
  app$click("palette_apply")
  app$set_inputs(color_scope="panel")
  app$set_inputs(color_target="A",color_element="curve",element_hex="#963C7F")
  app$set_inputs(color_apply=1)
  app$set_inputs(color_scope="series")
  app$set_inputs(color_target="Ejemplo A",color_element="series",element_hex="#123ABC")
  # Direct Shiny action input preserves the test's explicit colour value.
  app$set_inputs(color_apply=2)
  app$get_download("colors_json",filename=style_path)
  original <- jsonlite::read_json(style_path)
  stopifnot(original$preset=="minimal",original$palette=="Viridis · semitransparente",
    original$panels$A$curve=="#963C7F",original$series[["Ejemplo A"]]=="#123ABC")
},finally=app$stop())
restored <- shinytest2::AppDriver$new(app_dir=".",name="beta-style-restore",load_timeout=90000)
tryCatch({
  restored$set_inputs(inspector="Colores")
  restored$upload_file(colors_import=style_path)
  restored$wait_for_js("document.querySelector('#color_checks').textContent.includes('Viridis')",timeout=30000)
  copy <- tempfile(fileext=".json")
  restored$get_download("colors_json",filename=copy)
  stopifnot(identical(original,jsonlite::read_json(copy)),
    restored$get_value(input="palette")=="Viridis · semitransparente",
    restored$get_value(input="visual_preset")=="minimal")
  cat("Style JSON restored in a new browser session: PASS\n")
},finally=restored$stop())
