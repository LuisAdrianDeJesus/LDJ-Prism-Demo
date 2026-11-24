# Presentation-only color registry. No statistical functions are called here.
ldj_palette_catalog <- function() {
  bases <- list(
    `LDJ Editorial`=c("#287FAE","#D58A30","#9565B0","#148B83","#6B1E58","#59606A"),
    `Black and White`=c("#202020","#626262","#989898","#C5C5C5","#454545","#E3E3E3"),
    Floral=c("#B45783","#7C77B5","#5A997B","#D49D48","#739DB3","#C47768"),
    `Prism Light`=c("#2275B5","#E38426","#8A60AA","#148F89","#C45163","#718331"),
    `Prism Dark`=c("#71C4F0","#FFB86B","#CCA7EB","#65D4C6","#FF8DA5","#D3DC80"),
    `Colorblind Safe`=c("#0072B2","#D55E00","#009E73","#CC79A7","#E69F00","#56B4E9"),
    Waves=c("#145B83","#208EAD","#25A8A0","#67B894","#426E93","#80BCD0"),
    Starry=c("#344E88","#715C9E","#C28545","#29827B","#A2517A","#8A953D"),
    Pearl=c("#7D8DAD","#B69A7B","#9B88A9","#79A29C","#B68E9D","#949C77"))
  for(nm in c("Viridis","Inferno","Plasma"))
    bases[[nm]] <- grDevices::hcl.colors(6,nm)
  bases$Magma <- substr(viridisLite::magma(6),1,7)
  out <- lapply(names(bases),function(nm) list(name=nm,colors=bases[[nm]],alpha=1,
    background=if(nm=="Prism Dark") "#171D2A" else "#FFFFFF",
    text=if(nm=="Prism Dark") "#F5F7FC" else if(nm=="Black and White") "#202020" else "#273142"))
  names(out) <- names(bases)
  for(nm in setdiff(names(bases),"Black and White")) {
    semi <- out[[nm]]; semi$name <- paste(nm,"· semitransparente"); semi$alpha <- .65
    out[[semi$name]] <- semi
  }
  out
}

ldj_color_elements <- function() c("Serie (barras y curvas)"="series", "Relleno de barras"="bar_fill",
  "Borde de barras"="bar_border","Observaciones"="point","Curva ajustada"="curve",
  "Barras de error"="error","Banda de dispersión observada"="band","Controles"="control",
  "Banda de viabilidad 80–100%"="viability_band","Franja IC50–CC20"="parameter_span",
  "Guías CC20 / CC50 / IC50"="reference","Significancia validada"="significance",
  "Texto"="text","Títulos"="title","Subtítulos"="subtitle","Leyenda"="legend",
  "Fondo de figura"="background","Fondo del panel"="panel_background",
  "Ejes"="axis","Marcas de ejes"="ticks","Cuadrícula"="grid")

ldj_appearance <- function(palette="LDJ Editorial",preset="editorial") {
  catalog <- ldj_palette_catalog()
  aliases <- c(`Okabe-Ito`="Colorblind Safe",Grises="Black and White",
    `Prism clasico B/N`="Black and White",Personalizada="LDJ Editorial")
  if(palette %in% names(aliases)) palette <- aliases[[palette]]
  if(!palette %in% names(catalog)) stop("Paleta desconocida.")
  p <- catalog[[palette]]
  list(schema="ldj-colors",version=1L,palette=palette,preset=preset,
    series=list(),global=list(alpha=p$alpha,linewidth=.65,size=2.1,shape=21,
      linetype="solid",background=p$background,panel_background=p$background,
      text=p$text,title=p$text,subtitle=p$text,legend=p$text,axis=p$text,ticks=p$text,
      grid=if(palette=="Prism Dark") "#465064" else if(palette=="Black and White") "#DDDDDD" else "#DFE3E8",
      control=if(palette=="Prism Dark") "#A9AFBA" else if(palette=="Black and White") "#A0A0A0" else "#9EA7B3",
      reference=if(palette=="Prism Dark") "#D4DBE6" else if(palette=="Black and White") "#666666" else "#6B7280",
      significance=if(palette=="Prism Dark") "#F2BBE0" else if(palette=="Black and White") "#333333" else "#6B1E58"),
    series_styles=list(),panels=list())
}

ldj_series_colors <- function(state,series) {
  # Named assignments survive row, panel and module reordering.
  colors <- ldj_palette_catalog()[[state$palette]]$colors
  keys <- sort(unique(as.character(series)))
  result <- stats::setNames(rep(colors,length.out=length(keys)),keys)
  for(key in intersect(keys,names(state$series))) result[key] <- state$series[[key]]
  result
}

ldj_ensure_series <- function(state,series) {
  missing <- setdiff(sort(unique(as.character(series))),names(state$series))
  colors <- ldj_palette_catalog()[[state$palette]]$colors
  canonical <- c("DTG","Letermovir","CDQ","Lopinavir")
  for(key in missing) {
    index <- if(key %in% paste("Compuesto",LETTERS[1:4])) match(key,paste("Compuesto",LETTERS[1:4])) else if(key%in%canonical) match(key,canonical) else (length(state$series) %% length(colors))+1L
    state$series[[key]] <- colors[index]
  }
  state
}

ldj_element_style <- function(state,series=NULL,panel=NULL,color=NULL) {
  s <- state$global
  if(!is.null(series)) s <- utils::modifyList(s,state$series_styles[[series]] %||% list())
  if(!is.null(panel)) s <- utils::modifyList(s,state$panels[[panel]] %||% list())
  if(is.null(color)) color <- if(!is.null(series)) unname(ldj_series_colors(state,series)[1]) else "#287FAE"
  for(k in c("bar_fill","curve","band")) if(is.null(s[[k]])) s[[k]] <- color
  if(is.null(s$bar_border)) s$bar_border <- s$axis
  if(is.null(s$point)) s$point <- s$axis
  if(is.null(s$error)) s$error <- s$axis
  s
}
`%||%` <- function(a,b) if(is.null(a)) b else a

ldj_color_edit <- function(state,scope="global",target=NULL,element="series",hex,
                           alpha=1,linewidth=.65,size=2.1,shape=21,linetype="solid") {
  if(!is.character(hex)||length(hex)!=1||!grepl("^#[[:xdigit:]]{6}$",hex)) stop("Use HEX #RRGGBB.")
  if(!element %in% unname(ldj_color_elements())) stop("Elemento desconocido.")
  if(!scope %in% c("global","series","panel")) stop("Ámbito desconocido.")
  if(scope!="global" && (is.null(target)||!nzchar(target))) stop("Seleccione un destino.")
  if(scope=="panel" && !target %in% LETTERS[1:8]) stop("Panel inválido.")
  if(!all(is.finite(c(alpha,linewidth,size,shape)))||alpha<0||alpha>1||linewidth<.1||linewidth>5||size<.2||size>12||!shape%in%0:25)
    stop("Opacidad, grosor o símbolo fuera de rango.")
  if(!linetype %in% c("solid","dashed","dotted","dotdash")) stop("Estilo de línea inválido.")
  # Per-element opacity/width/size are stored separately, avoiding accidental cross-element edits.
  edit <- list(); keys <- if(element=="series") c("bar_fill","curve","band") else element
  for(k in keys) { edit[[k]] <- toupper(hex); edit[[paste0(k,"_alpha")]] <- alpha
    edit[[paste0(k,"_linewidth")]] <- linewidth; edit[[paste0(k,"_size")]] <- size
    edit[[paste0(k,"_shape")]] <- shape; edit[[paste0(k,"_linetype")]] <- linetype }
  if(scope=="global") state$global <- utils::modifyList(state$global,edit)
  if(scope=="series") {
    if(element=="series") state$series[[target]] <- toupper(hex)
    state$series_styles[[target]] <- utils::modifyList(state$series_styles[[target]] %||% list(),edit)
  }
  if(scope=="panel") state$panels[[target]] <- utils::modifyList(state$panels[[target]] %||% list(),edit)
  state
}

ldj_validate_colors <- function(x) {
  if(!is.list(x)||!identical(x$schema,"ldj-colors")||length(x$version)!=1||!is.numeric(x$version)||is.na(x$version)||x$version!=1||
     length(x$palette)!=1||!x$palette%in%names(ldj_palette_catalog())||length(x$preset)!=1||!x$preset%in%c("editorial","minimal")) stop("Configuración de color incompatible.")
  if(!all(c("series","global","series_styles","panels")%in%names(x))) stop("Configuración incompleta.")
  if(!all(c("alpha","linewidth","size","shape","linetype","background","panel_background","text","title","subtitle",
    "legend","axis","ticks","grid","control","reference","significance")%in%names(x$global))||
    !is.list(x$series)||!is.list(x$series_styles)||!is.list(x$panels)) stop("Configuración incompleta.")
  allowed <- c(unname(ldj_color_elements()),"alpha","linewidth","size","shape","linetype")
  allowed <- unique(c(allowed,as.vector(outer(unname(ldj_color_elements()),c("alpha","linewidth","size","shape","linetype"),paste,sep="_"))))
  if(length(setdiff(names(x),c("schema","version","palette","preset","series","global","series_styles","panels")))) stop("Campos JSON desconocidos.")
  for(z in c(list(x$global),x$series_styles,x$panels)) {
    if(!is.list(z)||any(!names(z)%in%allowed)) stop("Elemento JSON inválido.")
    for(k in names(z)) {
      v <- z[[k]]
      if(k%in%unname(ldj_color_elements()) && (!is.character(v)||length(v)!=1||!grepl("^#[[:xdigit:]]{6}$",v))) stop("Color JSON inválido.")
      if(grepl("alpha$",k) && (!is.numeric(v)||length(v)!=1||!is.finite(v)||v<0||v>1)) stop("Opacidad JSON inválida.")
      if(grepl("(size|linewidth|shape)$",k) && (!is.numeric(v)||length(v)!=1||!is.finite(v)||v<0||v>40)) stop("Tamaño JSON inválido.")
      if(grepl("linetype$",k) && !v%in%c("solid","dashed","dotted","dotdash")) stop("Línea JSON inválida.")
    }
  }
  if(length(x$panels)&&any(!names(x$panels)%in%LETTERS[1:8])) stop("Panel JSON inválido.")
  if(length(x$series)&&any(!vapply(x$series,function(v) is.character(v)&&length(v)==1&&grepl("^#[[:xdigit:]]{6}$",v),logical(1)))) stop("Serie JSON inválida.")
  x
}

ldj_contrast <- function(a,b) {
  luminance <- function(x) { rgb <- grDevices::col2rgb(x)/255
    rgb <- ifelse(rgb<=.04045,rgb/12.92,((rgb+.055)/1.055)^2.4)
    colSums(rgb*c(.2126,.7152,.0722)) }
  l <- c(luminance(a),luminance(b)); (max(l)+.05)/(min(l)+.05)
}
ldj_color_checks <- function(state,series) {
  if(!length(series)) return("Seleccione al menos una serie para revisar los colores.")
  s <- ldj_element_style(state); colors <- unname(ldj_series_colors(state,series))
  warnings <- character()
  for(k in c("text","title","subtitle","legend")) if(ldj_contrast(s[[k]],s$background)<4.5)
    warnings <- c(warnings,paste("Contraste de",k,"inferior a 4.5:1."))
  distance <- function(cs) if(length(cs)>1) min(as.matrix(stats::dist(t(grDevices::col2rgb(cs))))[lower.tri(matrix(0,length(cs),length(cs)))]) else Inf
  if(distance(colors)<35) warnings <- c(warnings,"Series próximas en RGB: use símbolos o líneas diferentes.")
  gray <- grDevices::convertColor(t(grDevices::col2rgb(colors))/255,"sRGB","XYZ")[,2]
  if(length(gray)>1 && min(stats::dist(gray))<.06) warnings <- c(warnings,"Luminancias próximas en escala de grises: conserve símbolos y estilos de línea.")
  if(requireNamespace("colorspace",quietly=TRUE)) for(type in c("deutan","protan","tritan")) {
    cs <- getExportedValue("colorspace",type)(colors)
    if(distance(cs)<35) warnings <- c(warnings,paste("Separación reducida en simulación",type,"; use símbolos."))
  } else warnings <- c(warnings,"Simulación de daltonismo no disponible: instale colorspace.")
  for(key in series) {
    style <- ldj_element_style(state,key)
    if(!identical(style$bar_fill,style$curve)) warnings <- c(warnings,paste("Barras y curvas tienen colores diferentes en",key))
    if(ldj_contrast(style$text,style$panel_background)<4.5) warnings <- c(warnings,paste("Revise contraste del texto en",key))
  }
  for(tag in names(state$panels)) {
    style <- ldj_element_style(state,panel=tag)
    if(ldj_contrast(style$text,style$panel_background)<4.5) warnings <- c(warnings,paste("Revise contraste del panel",tag))
  }
  if(!length(warnings)) "Contraste y separación comprobados; conserve símbolos para redundancia visual." else unique(warnings)
}

ldj_plot_theme <- function(s,base_size=12,base_family="sans",grid=FALSE) {
  for(k in c("text","title","subtitle","legend","axis","ticks","grid","background","panel_background"))
    s[[k]] <- grDevices::adjustcolor(s[[k]],alpha.f=s[[paste0(k,"_alpha")]] %||% 1)
  ggplot2::theme_classic(base_size=base_size,base_family=base_family)+
    ggplot2::theme(text=ggplot2::element_text(color=s$text),
      plot.title=ggplot2::element_text(color=s$title,face="bold"),
      plot.subtitle=ggplot2::element_text(color=s$subtitle,size=base_size*.8),
      plot.caption=ggplot2::element_text(color=s$text),
      axis.text=ggplot2::element_text(color=s$text),axis.title=ggplot2::element_text(color=s$text),
      axis.line=ggplot2::element_line(color=s$axis,linewidth=s$axis_linewidth %||% .5,linetype=s$axis_linetype %||% "solid"),
      axis.ticks=ggplot2::element_line(color=s$ticks,linewidth=s$ticks_linewidth %||% .5),
      legend.text=ggplot2::element_text(color=s$legend),legend.title=ggplot2::element_text(color=s$legend),
      legend.background=ggplot2::element_rect(fill=s$background,color=NA),
      plot.background=ggplot2::element_rect(fill=s$background,color=NA),
      panel.background=ggplot2::element_rect(fill=s$panel_background,color=NA),
      panel.grid.major=if(grid) ggplot2::element_line(color=s$grid,linewidth=s$grid_linewidth %||% .2,
        linetype=s$grid_linetype %||% "solid") else ggplot2::element_blank(),
      plot.margin=ggplot2::margin(10,18,16,18))
}

ldj_style_existing_plot <- function(p,s,bar=FALSE,preset="editorial") {
  # Update layer presentation while leaving data and computed statistics intact.
  for(i in seq_along(p$layers)) {
    layer <- p$layers[[i]]; geom <- class(layer$geom)[1]
    element <- switch(geom,GeomCol="bar_fill",GeomBar="bar_fill",GeomPoint="point",
      GeomLine="curve",GeomPath="curve",GeomErrorbar="error",GeomRect="band",GeomRibbon="band",
      GeomHline="reference",GeomVline="reference",GeomSegment="reference",GeomText="significance",NULL)
    if(is.null(element)) next
    if(geom%in%c("GeomCol","GeomBar")) {
      if(is.data.frame(p$data) && "conc_uM"%in%names(p$data)) {
        p$data$fill <- ifelse(p$data$conc_uM==0,
          grDevices::adjustcolor(s$control,alpha.f=s$control_alpha %||% s$alpha),
          grDevices::adjustcolor(s$bar_fill,alpha.f=s$bar_fill_alpha %||% s$alpha))
      } else layer$aes_params$fill <- grDevices::adjustcolor(s$bar_fill,alpha.f=s$bar_fill_alpha %||% s$alpha)
      layer$aes_params$colour <- grDevices::adjustcolor(s$bar_border,alpha.f=s$bar_border_alpha %||% 1)
    } else if(geom%in%c("GeomRect","GeomRibbon")) {
      layer$aes_params$fill <- s$band
    } else layer$aes_params$colour <- s[[element]]
    layer$aes_params$alpha <- s[[paste0(element,"_alpha")]] %||%
      if(element=="band") .14 else if(element=="curve") s$alpha else 1
    if(element=="bar_fill") layer$aes_params$alpha <- 1
    if(element=="point") {
      layer$aes_params$size <- s$point_size %||% layer$aes_params$size
      layer$aes_params$shape <- s$point_shape %||% layer$aes_params$shape
      layer$aes_params$fill <- s$panel_background
      layer$aes_params$stroke <- s$point_linewidth %||% layer$aes_params$stroke
    }
    if(element%in%c("curve","error","reference","bar_fill")) {
      width_element <- if(element=="bar_fill") "bar_border" else element
      layer$aes_params$linewidth <- s[[paste0(width_element,"_linewidth")]] %||% layer$aes_params$linewidth
      layer$aes_params$linetype <- s[[paste0(width_element,"_linetype")]] %||% layer$aes_params$linetype
    }
    p$layers[[i]] <- layer
  }
  if(preset=="minimal") p$layers <- Filter(function(layer)
    !class(layer$geom)[1]%in%c("GeomRect","GeomRibbon","GeomVline","GeomLabelRepel"),p$layers)
  p
}

ldj_style_universal <- function(p,state,group_column="ldj_group") {
  layers <- list()
  for(layer in p$layers) {
    d <- if(is.data.frame(layer$data)) layer$data else p$data
    key <- if(group_column%in%names(d)) group_column else if("series"%in%names(d)) "series" else NULL
    if(is.null(key)) {layers[[length(layers)+1L]] <- layer;next}
    geom <- class(layer$geom)[1]
    split_key <- if(geom%in%c("GeomCol","GeomBar") && ".is_control"%in%names(d))
      interaction(d[[key]],d$.is_control,drop=TRUE) else d[[key]]
    for(piece in split(d,split_key,drop=TRUE)) {
      series <- as.character(piece[[key]][1])
      s <- ldj_element_style(state,series)
      geom <- class(layer$geom)[1]
      # A layer copy prevents style edits from mutating cached plot objects.
      z <- ggplot2::ggproto(NULL,layer); z$data <- piece
      element <- switch(geom,GeomCol="bar_fill",GeomBar="bar_fill",GeomPoint="point",
        GeomLine="curve",GeomPath="curve",GeomErrorbar="error",GeomBoxplot="bar_border",NULL)
      if(!is.null(element)) {
        explicit <- !is.null(state$global[[element]]) || !is.null(state$series_styles[[series]][[element]])
        col <- if(explicit) s[[element]] else unname(ldj_series_colors(state,series)[1])
        z$mapping$colour <- NULL
        z$aes_params$colour <- if(element=="bar_fill") grDevices::adjustcolor(s$bar_border,alpha.f=s$bar_border_alpha %||% 1) else col
        if(element=="bar_fill") {
          z$aes_params$fill <- if(".is_control"%in%names(piece) && all(piece$.is_control))
            grDevices::adjustcolor(s$control,alpha.f=s$control_alpha %||% s$alpha) else
            grDevices::adjustcolor(s$bar_fill,alpha.f=s$bar_fill_alpha %||% s$alpha)
        }
        z$aes_params$alpha <- s[[paste0(element,"_alpha")]] %||% s$alpha
        if(element=="bar_fill") z$aes_params$alpha <- 1
        if(element=="point") {z$aes_params$shape <- s$point_shape %||% z$aes_params$shape
          z$aes_params$size <- s$point_size %||% z$aes_params$size;z$aes_params$fill <- col
          z$aes_params$stroke <- s$point_linewidth %||% z$aes_params$stroke}
        if(element!="point") z$aes_params$linewidth <- s[[paste0(if(element=="bar_fill") "bar_border" else element,"_linewidth")]] %||% z$aes_params$linewidth
      }
      layers[[length(layers)+1L]] <- z
    }
  }
  p$layers <- layers
  # Styled layers use fixed colours. Train the discrete scale with an invisible
  # point per observed group so the legend remains meaningful without a second mark.
  if (!is.null(p$mapping$colour) && group_column %in% names(p$data) &&
      !is.null(p$scales$get_scales("colour"))) {
    legend_data <- p$data[!is.na(p$data[[group_column]]) &
      !duplicated(p$data[[group_column]]), , drop=FALSE]
    if (nrow(legend_data)) p <- p +
      ggplot2::geom_point(data=legend_data,alpha=0,na.rm=TRUE,show.legend=TRUE) +
      ggplot2::guides(colour=ggplot2::guide_legend(override.aes=list(alpha=1)))
  }
  p
}

ldj_color_ui <- function() shiny::tagList(
  shiny::selectInput("visual_preset","Diseño",c("LDJ Editorial — Original"="editorial","LDJ Minimalista"="minimal")),
  shiny::selectInput("palette","Esquema",c(names(ldj_palette_catalog()),"Personalizada","Okabe-Ito","Grises","Prism clasico B/N"),selected="LDJ Editorial"),
  shiny::uiOutput("palette_preview"),shiny::uiOutput("palette_gallery"),
  shiny::actionButton("palette_apply","Aplicar esquema completo"),
  shiny::selectInput("color_scope","Aplicar a",c("Toda la figura"="global","Una serie"="series","Un panel A–H"="panel")),
  shiny::selectInput("color_target","Serie / panel",choices=NULL),
  shiny::selectInput("color_element","Elemento",ldj_color_elements()),
  colourpicker::colourInput("element_hex","Color visual / HEX","#287FAE",showColour="both"),
  shiny::sliderInput("element_alpha","Opacidad",0,1,1,.05),
  shiny::sliderInput("element_width","Grosor / fracción entre dosis para sombras",.1,5,.65,.05),
  shiny::sliderInput("element_size","Tamaño de símbolo",.2,12,2.1,.1),
  shiny::selectInput("element_shape","Símbolo",c("Círculo"=21,"Triángulo"=24,"Cuadrado"=22,"Diamante"=23)),
  shiny::selectInput("element_line","Línea",c("Continua"="solid","Guiones"="dashed","Puntos"="dotted","Punto-guion"="dotdash")),
  shiny::actionButton("color_apply","Aplicar al elemento",onclick=paste0(
    "var el=document.getElementById('element_hex');",
    "if(el) Shiny.setInputValue('element_hex',el.value,{priority:'event'});")),
  shiny::actionButton("colors_reset","Restaurar LDJ Editorial — Original"),
  shiny::textInput("scheme_name","Nombre del esquema personalizado","Mi esquema"),
  shiny::actionButton("scheme_save","Guardar esquema en esta sesión"),
  shiny::selectInput("scheme_saved","Esquemas guardados",choices=NULL),
  shiny::actionButton("scheme_load","Cargar esquema guardado"),
  shiny::downloadButton("colors_json","Exportar colores JSON"),
  shiny::fileInput("colors_import","Importar colores JSON",accept=".json"),
  shiny::verbatimTextOutput("color_checks"),shiny::uiOutput("custom_color_inputs"))

ldj_color_server <- function(input,output,session,series) {
  state <- shiny::reactiveVal(ldj_appearance()); saved <- shiny::reactiveVal(list())
  shiny::observeEvent(series(),{state(ldj_ensure_series(shiny::isolate(state()),series()))})
  shiny::observeEvent(input$visual_preset,{s <- state();s$preset <- input$visual_preset;state(s)},ignoreInit=TRUE)
  shiny::observeEvent(input$palette_apply,{
    tryCatch({s <- ldj_appearance(input$palette,input$visual_preset); state(ldj_ensure_series(s,series()))},
      error=function(e) shiny::showNotification(conditionMessage(e),type="error"))
  })
  # Legacy palette changes remain reactive for existing configurations and tests.
  shiny::observeEvent(input$palette,{
    if(input$palette%in%c("Okabe-Ito","Grises","Prism clasico B/N","Personalizada"))
      state(ldj_ensure_series(ldj_appearance(input$palette,input$visual_preset %||% "editorial"),series()))
  },ignoreInit=TRUE)
  shiny::observeEvent(input$color_scope,{
    shiny::updateSelectInput(session,"color_target",choices=if(input$color_scope=="panel") LETTERS[1:8] else series())
  })
  shiny::observeEvent(series(),{if(!identical(input$color_scope,"panel")) shiny::updateSelectInput(session,"color_target",choices=series())})
  shiny::observeEvent(input$color_apply,{
    tryCatch(state(ldj_color_edit(state(),input$color_scope,input$color_target,input$color_element,
      input$element_hex,input$element_alpha,input$element_width,input$element_size,
      as.integer(input$element_shape),input$element_line)),error=function(e) shiny::showNotification(conditionMessage(e),type="error"))
  })
  shiny::observeEvent(input$colors_reset,{
    state(ldj_ensure_series(ldj_appearance(),series()))
    shiny::updateSelectInput(session,"palette",selected="LDJ Editorial")
    shiny::updateSelectInput(session,"visual_preset",selected="editorial")
  })
  output$palette_preview <- shiny::renderUI({
    nm <- input$palette; p <- tryCatch(ldj_appearance(nm),error=function(e) ldj_appearance())
    cols <- ldj_palette_catalog()[[p$palette]]$colors
    shiny::div(style=paste0("padding:8px;background:",p$global$background),
      shiny::tagList(lapply(cols,function(hex) shiny::tags$span(title=hex,style=paste0("display:inline-block;width:25px;height:25px;background:",hex,";opacity:",p$global$alpha,";margin:2px")))))
  })
  output$palette_gallery <- shiny::renderUI(shiny::tags$details(shiny::tags$summary("Galería de esquemas"),
    style="max-height:480px;overflow-y:auto",
    shiny::tagList(lapply(ldj_palette_catalog(),function(p) shiny::div(style="margin-bottom:6px",shiny::tags$small(p$name),shiny::tags$br(),
      shiny::tagList(lapply(p$colors,function(hex) shiny::tags$span(style=paste0("display:inline-block;width:18px;height:18px;background:",hex,";opacity:",p$alpha,";margin:1px")))))))))
  output$color_checks <- shiny::renderText(paste(c(paste("Esquema activo:",state()$palette),
    ldj_color_checks(state(),series())),collapse="\n"))
  shiny::observeEvent(input$scheme_save,{
    nm <- trimws(input$scheme_name); if(!nzchar(nm)) return()
    z <- saved();z[[nm]] <- state();saved(z);shiny::updateSelectInput(session,"scheme_saved",choices=names(z),selected=nm)
  })
  shiny::observeEvent(input$scheme_load,{shiny::req(input$scheme_saved);state(saved()[[input$scheme_saved]])})
  output$colors_json <- shiny::downloadHandler(filename=function() "LDJ_Prism_colors.json",content=function(file)
    jsonlite::write_json(state(),file,pretty=TRUE,auto_unbox=TRUE))
  shiny::observeEvent(input$colors_import,{
    tryCatch({if(input$colors_import$size>1e6) stop("JSON demasiado grande.")
      z <- ldj_validate_colors(jsonlite::fromJSON(input$colors_import$datapath,simplifyVector=FALSE));state(z)
      shiny::updateSelectInput(session,"palette",selected=z$palette)
      shiny::updateSelectInput(session,"visual_preset",selected=z$preset)
    },error=function(e) shiny::showNotification(conditionMessage(e),type="error"))
  })
  state
}


# Reference PNG dimensions translated to inches at 600 dpi.
ldj_editorial_dimensions <- function(module, rows=4L) {
  pixels <- switch(module, mtt=c(5130,7260), activity=c(5070,7200), safety=c(5010,6270))
  stopifnot(!is.null(pixels),length(rows)==1L,rows %in% 1:4)
  stats::setNames(c(pixels[1],pixels[2]*rows/4)/600,c("width","height"))
}
