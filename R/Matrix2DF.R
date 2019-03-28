#'  Convert a bipartite-style matrix to dataframe
#'
#'Matrix2DF returns a data frame in the format internally required for bipartiteD3 where the first
#' two columns list the interacting species, and the third column lists the link strengths.
#'
#' Matrix2DF expects a matrix of the format used by bipartite, for example that created by frame2webs().
#' This structure includes row and column names to indicate the species, and a named
#' third dimension giving the name of that site.
#'
#' Note a matrix of this format can be passed directly to bipartite_D3() since it will test for a matrix
#' and apply Matrix2DF() anyway.
#' @param Matrix Bipartite network in matrix format
#' @param PrimaryLab Label for the primary level of the bipartite web, e.g. 'Plants'
#' @param SecondaryLab Label for the secondary level of the bipartite web, e.g. 'Pollinators'
#' @param SiteLab Name for the site
#' @import dplyr
#' @import tidyr
#' @return A data.frame where the first column is the primary interactor, the second the secondary
#' interactor and third column detail the link strengths.
#' @examples
#'
#'data(Safariland, package='bipartite')
#' Matrix2DF(Safariland)
#'
#' @export
Matrix2DF<- function(Matrix,
                     PrimaryLab = 'Primary',
                     SecondaryLab='Secondary',
                     SiteLab = 'Site'){

  if(is.null(SiteLab)){SiteLab<- 'Site'}

  Matrix%>%
    as.data.frame()%>%
    tibble::rownames_to_column(var='Prim')%>%
    gather(key ='Sec',value = 'Site', -'Prim')->df


  names(df) <- c(PrimaryLab,SecondaryLab, SiteLab)

  return(df)
}
