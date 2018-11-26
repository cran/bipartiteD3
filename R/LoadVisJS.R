#' LoadVisJS
#'
#'Downloads source code for the open source vis JavaScript libary from vizjs.org if it is not already present
#'in working directory. Uses v1.1.0
#'
#'Used internally by BP_JS_Writer() and bipartite_D3()
#'
#' @export
LoadVisJS <- function(){
  if(!any(list.files() =='vizjs.js')){
    try({

      VISJS<-readLines('http://vizjs.org/viz.v1.1.0.min.js')
  writeLines(VISJS,con = 'vizjs.js')

    } )
  }
}
