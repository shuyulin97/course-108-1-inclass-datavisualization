html_create <- function(templatepath){
  readLines(templatepath) ->
    html_template
  function(filename, dir=".", browse=F, edit=F){
    fp_filename <- file.path(dir=dir,filename)
    writeLines(html_template, fp_filename)
    if(browse==T){browseURL(fp_filename)}
    if(edit==T){file.edit(fp_filename)}

  }
}

html_create_bootstrap <- html_create("https://www.dropbox.com/s/o8tcby2g4qmzjdd/bootstrapTemplate.html?dl=1")

html_create_plain <- html_create("https://www.dropbox.com/s/8cnjf0krcxg33gs/plain.html?dl=1")

# ## Do not run ##
# html_create_bootstrap("myBt4.html")
# html_create_plain("myPlain.html")

project_html_create <- function(
  main_file="index.html",
  type="plain",
  dirlib=".",
  structure=c(
    "js",
    "css",
    "external/in_header",
    "external/before_body",
    "external/after_body")
  ){
  paths=file.path(dirlib,structure)
  lapply(paths, function(x) dir.create(x,recursive = T))
  main_file=file.path(dirlib, main_file)
  if(type=="bs"){
    html_create_bootstrap(
      main_file
    )
    } else {
      html_create_plain(
        main_file
      )
    }
  }

# ## Do not run
# project_html_create(
#   main_file="index.html",
#   type="bs",
#   dirlib=".",
#   structure=c(
#     "js",
#     "css",
#     "external/in_header",
#     "external/before_body",
#     "external/after_body"
#   )
# )

webapp_project_create <- function(dirlib=NULL, overwrite=FALSE){
  if(is.null(dirlib)){dirlib="example"}
  if(overwrite==FALSE){
    i<-0
    dirlib_new <- dirlib
    while(dir.exists(dirlib_new) & i < 10){
      i<-i+1
      dirlib_new=paste0(dirlib,"_",i)
      if(!dir.exists(dirlib_new)){
        dir.create(dirlib_new)
        dirlib=dirlib_new
        break
      }
    }    
  } else {
    dir.create(dirlib)
  }

  download.file("https://www.dropbox.com/s/1epjlhcb6nwck7n/webProjectTemplate.zip?dl=1",
                destfile = file.path(dirlib,"webProjectTemplate.zip"))
  unzip(file.path(dirlib,"webProjectTemplate.zip"), exdir=dirlib)
  unlink(file.path(dirlib,"__MACOSX"),recursive = T)
  file.remove(file.path(dirlib,"webProjectTemplate.zip"))
  cat("Project created under: ", dirlib)
}

save_frameableWidget <- function(plot, filename){
  plot %>% 
    widgetframe::frameableWidget() %>%
    htmlwidgets::saveWidget(basename(filename))
  file.link(from=basename(filename), to=filename)
  text <- '
  <iframe src="filename" 
  scrolling = "no" 
  seamless = "seamless" frameBorder = "0" 
  width= "100%" height="100%"></iframe>' %>%
    stringr::str_replace("filename",filename) %>%
  cat('請在html檔貼上：\n',
      'width與height依需要刪去其中一個\n',.,
      '\n\n記得檢查一下路徑')
}

to_frameabeHTML <- function(filename){
  filename %>%
    readLines() ->
    htmlContent
  if(all(!str_detect(htmlContent,"pym.Child"))){  
    # find Plotly.newPlot(...)
    ilocL <- str_which(htmlContent,'(Plotly\\.newPlot\\()')
    ilocR <- str_which(htmlContent[ilocL:length(htmlContent)],'\\)')[[1]]+ilocL-1
    targetLine <- htmlContent[ilocL:ilocR]
    targetLineSerialized <- paste0(targetLine,collapse = '\n ')
    paste0('function drawGraphic(width) {\n',
           targetLineSerialized,'\n',
           '}\n',
           'var pymChild = new pym.Child({ renderCallback: drawGraphic });') -> newTargetLine
    
    if(ilocR>ilocL){
      htmlContent <- htmlContent[-((ilocL+1):ilocR)]
    }
    
    htmlContent[ilocL] <- newTargetLine
    
    
    htmlContent %>% writeLines(filename)
    ## find target id
    targetLineSerialized %>%
      str_replace_all(" ","") %>%
      str_extract("(?<=(Plot\\())[:graph:]+(?=,)")
  }  
  '<div id="example" width="100%" height="100%"></div>
<script type="text/javascript" src="https://pym.nprapps.org/pym.v1.min.js"></script>
<script>
    var pymParent = new pym.Parent("example", "filename", {});
</script>
' %>%
    str_replace("filename",filename) %>%
    cat('請在要引入圖形處貼上：\n\n',
        .,'\n',
        '請檢查scripts最後一行的路徑。\n',
        '"example"可改成自己要的id')
  
}

