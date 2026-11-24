Sys.unsetenv("LDJ_PRISM_MODE")
chrome <- Sys.which("google-chrome")
if(nzchar(chrome)) Sys.setenv(CHROMOTE_CHROME=chrome)
app <- shinytest2::AppDriver$new(app_dir=".",name="axis-layout",load_timeout=90000)
dir.create("test-artifacts",showWarnings=FALSE)
tryCatch({
  image_ready <- "Boolean(document.querySelector('#plot img')?.naturalWidth)"
  screenshot <- function(name) {
    view <- app$get_chromote_session()$Page$getLayoutMetrics()
    size <- view$cssContentSize
    png <- app$get_chromote_session()$Page$captureScreenshot(format="png",
      captureBeyondViewport=TRUE,clip=list(x=0,y=0,width=size$width,height=size$height,scale=1),timeout_=90)
    writeBin(jsonlite::base64_dec(png$data),file.path("test-artifacts",name))
  }
  for(module in c("mtt","activity","safety")) {
    app$set_inputs(module=module,inspector="Datos",wait_=FALSE)
    app$click("load_example")
    app$set_inputs(inspector="Gráfica",wait_=FALSE)
    app$wait_for_js(image_ready,timeout=90000)
    for(w in c(1280,1920,2560)) {
      h <- c(`1280`=720,`1920`=1080,`2560`=1440)[[as.character(w)]]
      app$get_chromote_session()$Emulation$setDeviceMetricsOverride(width=w,height=h,deviceScaleFactor=1,mobile=FALSE)
      app$wait_for_idle(timeout=90000)
      sz <- unlist(app$get_js("(() => {const r=document.querySelector('#plot').getBoundingClientRect();return [r.width,r.height]})()"))
      expected <- c(mtt=7260/5130,activity=7200/5070,safety=6270/5010)[[module]]
      stopifnot(length(sz)==2,abs(sz[2]/sz[1]-expected)<.01)
      cat(module,w,h,"viewport;",sz[1],sz[2],"plot CSS pixels\n")
    }
    screenshot(paste0("axis-layout-",module,".png"))
    app$set_inputs(inspector="Exportar",wait_=FALSE)
    for(fmt in c("svg","pdf","png")) {
      path <- file.path("test-artifacts",paste0("axis-layout-",module,".",fmt))
      app$get_download(fmt,filename=path)
      stopifnot(file.info(path)$size>10000)
    }
  }
  app$set_inputs(module="mtt",inspector="Análisis",wait_=FALSE)
  app$wait_for_js("document.querySelector('#advanced_analysis') && !document.querySelector('#advanced_analysis').open",timeout=30000)
  screenshot("axis-basic.png")
  app$run_js("document.querySelector('#advanced_analysis summary').click()")
  app$wait_for_js("document.querySelector('#advanced_analysis').open",timeout=30000)
  screenshot("axis-advanced.png")
  app$run_js("document.querySelector('#advanced_analysis summary').click()")
  app$set_inputs(inspector="Ejes",axis_y_mode="manual",axis_y_min=0,axis_y_max=80,
    axis_y_step=10,axis_target="all",wait_=FALSE)
  app$wait_for_idle(timeout=90000)
  screenshot("axis-editor.png")
  app$set_inputs(inspector="Dimensiones y distribución",layout_row="1",row_weight=1.5,
    width=8,height=12,preview_zoom=120,wait_=FALSE)
  app$wait_for_idle(timeout=90000)
  app$set_inputs(inspector="Exportar",wait_=FALSE)
  path <- file.path("test-artifacts","axis-manual.svg")
  app$get_download("svg",filename=path)
  stopifnot(any(grepl("recorte visual en Y",readLines(path,warn=FALSE),fixed=TRUE)))
  cat("Responsive aspect ratio, axis range without data filtering, and controls: PASS\n")
},finally=app$stop())
