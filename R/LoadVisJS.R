#' LoadVisJS
#'
#'Downloads source code for the open source vis JavaScript library from vizjs.org if it is not already present
#'in working directory. Uses v1.1.0
#'
#'Used internally by BP_JS_Writer() and bipartite_D3()
#'
#'@import downloader
#'
#' @export
LoadVisJS <- function(){
  if(!any(list.files() =='vizjs.js')){
    try({
      Hash<-downloader::sha_url('http://vizjs.org/viz.v1.1.0.min.js')
      if(Hash == "295e915d475fdf1c0d7db13668b6b0b526a8e910"){ # Security hash
        downloader::download('http://vizjs.org/viz.v1.1.0.min.js','vizjs.js' )
      }else{
        warning("Hash of http://vizjs.org/viz.v1.1.0.min.js doesn't match stored hash, so not downloading in case it has been modified.
                Please save an up-to-date version of the visjs library as visjs.js library in the working directory" )
      }
    } )
  }
}
