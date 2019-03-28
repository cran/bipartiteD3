#' Find Species Order That Minimises Crossover
#'
#' Find an order of species that is likely to minimise cross over.
#' It builds upon the 'cca' method used in the bipartite package, but orders the compartments by size,
#' which tends to give better effects.
#'
#' @param df A network in data.frame format. (row names for primary layer, column names for secondary layer)
#'
#' @return A list containing 'PrimaryOrder' and 'SecondaryOrder', to be used with bipartite_d3()
#'
#' @examples
#'
#'\dontrun{
#'
#' data(Safariland, package='bipartite')
#'
#'
#'S_orders <- OrderByCrossover(Safariland)
#'
#' bipartite_D3(Safariland,
#'   filename = 'SF_sorted',
#'   SortPrimary = S_orders[[1]],
#'   SortSecondary = S_orders[[2]])
#'}
#'
#' @export

OrderByCrossover<-function(df){

  if(!(requireNamespace("vegan", quietly = TRUE) &requireNamespace("bipartite", quietly = TRUE))){
    warning('This function needs vegan and bipartite to be installed first')
    return(NULL)
  }

  co <- bipartite::compart(df)

  row.seq <- NULL
  col.seq <- NULL

  # Put all the specialists first,
  CompartSize = c()
  for(i in 1:co$n.compart){
    CompartSize<-c(CompartSize,sum(abs(co$cweb) == i))
  }

  CompartmentRank<- rank(CompartSize, ties.method = 'first')

  for (m in   order(CompartmentRank)) {
    comp.member <- which(abs(co$cweb) == m, arr.ind = TRUE)
    rs <- unique(comp.member[, 1])
    cs <- unique(comp.member[, 2])
    if (length(rs) < 3 | length(cs) < 3) {
      row.seq <- c(row.seq, rs)
      col.seq <- c(col.seq, cs)
    }
    else {
      ca <- vegan::cca(df[rs, cs])
      row.seq <- c(row.seq, rs[order(summary(ca)$sites[,
                                                       1], decreasing = TRUE)])
      col.seq <- c(col.seq, cs[order(summary(ca)$species[,
                                                         1], decreasing = TRUE)])
    }
  }
  return(list('PrimaryOrder'= rownames(df)[row.seq], 'SecondaryOrder' = colnames(df)[col.seq] ))
}
