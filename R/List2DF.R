#' Convert bipartite-style list of matrices to dataframe
#'
#' List2DF returns a data frame in the format internally required for bipartiteD3 where the first
#' two columns list the interacting species, and subsequent columns list the link strengths in each site.
#'
#'List2DF expects an list of multiple bipartite webs as may be created by the frame2webs(type.out='list') function in bipartite.
#'This structure includes row and column names to indicate the species, and a named third dimension giving
#'the names of each of the sites
#'
#'Note a list of this format can be passed directly to bipartite_D3 since it will test for an list and apply List2DF() anyway.
#'
#' @param List An list of bipartite format matrices
#' @param PrimaryLab Label for the primary level of the bipartite web, e.g. 'Plants'
#' @param SecondaryLab Label for the secondary level of the bipartite web, e.g. 'Pollinators'
#' @param SiteNames Vector of names for the different sites (list elements). By default takes names of input matrices if they exist.
#'
#' @return A data.frame where the first column is the primary interactor, the second the secondary interactor and subsequent named columns detail the link strengths
#' @import dplyr
#' @import tidyr
#' @examples
#'
#'\dontrun{ testdata <- data.frame(higher = c("bee1","bee1","bee1","bee2","bee1","bee3"),
#' lower = c("plant1","plant2","plant1","plant2","plant3","plant4"),
#' webID = c("meadow","meadow","meadow","meadow","bog","bog"), freq=c(5,9,1,2,3,7))
#' bipartite::frame2webs(testdata, type.out = 'list')-> SmallTestWeb
#'
#' List2DF(SmallTestWeb)
#' }
#' @export
List2DF <- function(List,
                     PrimaryLab = 'Primary',
                     SecondaryLab='Secondary',
                     SiteNames=NULL){

  if(is.null(SiteNames)){SiteNames<- names(List)}
  TmpPrimaryLab<-'TmpPrimaryLab'
  TmpSecondaryLab<-'TmpSecondaryLab'
  TmpSiteLab<- 'TmpSiteLab'


  purrr::map(.x = List,.f = function(mat ){
    Matrix2DF(Matrix = mat,
              PrimaryLab = 'TmpPrimaryLab',
              SecondaryLab = 'TmpSecondaryLab',
              SiteLab = 'TmpSiteLab')%>%
      unite(col = 'LinkName', 1:2, sep='_LINK_')-> df2
    return(df2)} )%>%
    purrr::reduce(full_join, by = "LinkName" )%>%
    separate('LinkName', c( TmpPrimaryLab,TmpSecondaryLab), sep = '_LINK_')-> xx

  for(column in 3:ncol(xx)){
    xx[[column]]<-replace(xx[[column]], is.na(xx[[column]]), 0)
  }
  xx-> DF_All
  # mutate_all(xx, funs(replace(., is.na(.), 0)))  -> DF_All  ## '.' Disliked in package build
  names(DF_All) <-  c(PrimaryLab,SecondaryLab, SiteNames)

  return(DF_All)
}
