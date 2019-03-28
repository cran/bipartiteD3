#' Convert bipartite-style arrays to dataframe
#'
#' Returns a data frame in the format internally required for bipartiteD3 where the first
#' two columns list the interacting species, and subsequent columns list the link strengths in each site.
#'
#'Array2DF expects an array of multiple bipartite webs as may be created by the webs2array() function in bipartite.
#'This structure includes row and column names to indicate the species, and a named third dimension giving
#'the names of each of the sites
#'
#'Note an array of this format can be passed directly to bipartite_D3 since it will test for an array and apply Array2DF() anyway.
#'
#' @param Array An array of bipartite format
#' @param PrimaryLab Label for the primary level of the bipartite web, e.g. 'Plants'
#' @param SecondaryLab Label for the secondary level of the bipartite web, e.g. 'Pollinators'
#' @param SiteNames Vector of names for the different sires (array slices). By default takes names of input array if they exist.
#'
#' @return A data.frame where the first column is the primary interactor, the second the secondary interactor and subsequent named columns detail the link strengths
#' @import dplyr
#' @import tidyr
#' @examples
#'
#' \dontrun{
#' data(Safariland, vazquenc, package='bipartite')
#' allin1 <- bipartite::webs2array(Safariland, vazquenc)
#' Array2DF(allin1)
#' }
#' @export
Array2DF <- function(Array,
                     PrimaryLab = 'Primary',
                     SecondaryLab='Secondary',
                     SiteNames=NULL){

  if(is.null(SiteNames)){SiteNames<- dimnames(Array)[[3]]}

  purrr::map((1:length(SiteNames)),
             function(slot, Array ){
    Matrix2DF(Matrix = Array[,,slot],
              PrimaryLab = 'PrimaryLab',
              SecondaryLab = 'SecondaryLab',
              SiteLab = 'SiteLab')%>%
      unite(col = 'LinkName', 1:2, sep='_LINK_')-> df2
    return(df2)},Array)%>%
    purrr::reduce(full_join, by = "LinkName" )%>%
    separate('LinkName', c( PrimaryLab,SecondaryLab), sep = '_LINK_')-> xx

  for(column in 3:ncol(xx)){
    xx[[column]]<-replace(xx[[column]], is.na(xx[[column]]), 0)
  }
  xx-> DF_All
   # mutate_all(xx, funs(replace(., is.na(.), 0)))  -> DF_All  ## '.' Disliked in package build
  names(DF_All) <-  c(PrimaryLab,SecondaryLab, SiteNames)

  return(DF_All)
}
