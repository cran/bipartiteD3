#' Generate interactive bipartite networks
#'
#'Plots one or more interactive bipartite graphs. Data can be supplied either in bipartite package format or as a data frame and
#'generates an html widget. There are range of display options, see vignette for examples.
#'
#'This function offers a straightforward way to generate an interactive bipartite graph.
#'Hovering over a species will focus just on that species and its interactors, and display their relative fractions.
#' Where multiple networks are examined simultaneously, the selection occurs in tandem.
#'
#' It will try to download source code of a version of the vis JavaScript library, generate a JavaScript file (.js)
#'and a Cascading Style Sheet (.css) and place them in the working directory.
#'These are then used by r2d3() to create an html object.
#'
#'When used in RStudio version 1.2+ this is visible in the viewer pane.
#'If using an earlier version of Rstudio, the graph may appear as a blank space in the default RStudio viewer.
#'In this case, you can still use knitr to create an html file and view it in a browser.
#'
#' Guessing appropriate sizes for the figures can be a process of trial and error. The best values depend on the length of
#' the labels, the number of interactions and their relative weighting. See Vignette for details.
#' If the figure looks weirdly proportioned, the links appear to invert or one of the halves is notably
#'  longer than other other, the main figure margins are probably too small. It is often necessary to
#'   experiment a little with large or complex figures.
#'
#'To include figures as a static plot for publication, there are several options. The r2D3 package
#' provides the save_d3_png and save_d3_html functions to directly save d3 objects. This is normally the easiest.
#' If using RStudio v1.2+, then it is possible to just export from the viewer pane.
#'
#' Otherwise, from an html document generated by knitr, it is often useful to 'print to pdf' within the browser.
#'  Finally is possible to extract the svg segment that relates to the figure from the html file
#' and save it directly as an svg file, which can then be used in e.g. Inkscape.
#'
#' @param data Food web or webs to be plotted. Can be either in data.frame format (Sp1,Sp2, Site1, Site2 etc) or bipartite style matrix, list of matrices or array.
#' @param filename Character string to name the .js and .css files. Do not include a file extension.
#' @param PrimaryLab Character string to label left (lower) half of graph (e.g. 'plant' or 'host').
#' @param SecondaryLab Character string to label right (upper) half of graph (e.g. 'pollinator' or 'parasitoid').
#' @param SiteNames Character vector giving name or names of site or sites. Used as a title for facets.
#' @param colouroption Either 'monochrome', 'brewer' or 'manual'.
#' @param HighlightLab Name of interactor species to highlight. (Text must match).
#' @param HighlightCol Colour to highlight species. Can be any format read by html, eg simple names: 'PINK', hexcode: '#FFC0CB' or rgb:	'rgb(255,192,203)'
#' @param monoChromeCol If using monochrome option, what colour to use. Can be any format read by html, eg simple names 'PINK', hexcode '#FFC0CB' or rgb	'rgb(255,192,203)'
#' @param ColourBy Which set of interactors to colour by. 1= primary, 2= secondary
#' @param BrewerPalette RColorBrewer palette to use, e.g. 'Set3'. Be sure to select one with enough available colours (it will warn)
#' @param NamedColourVector Named vector of colours for manual colour assignment. Can be any format read by html, eg simple names 'PINK', hexcode '#FFC0CB' or rgb	'rgb(255,192,203)'
#' @param MainFigSize c(width, height). Size of html container for the whole figure
#' @param SortPrimary Vector of order of species to arrange primary level. Default is alphabetical
#' @param SortSecondary Vector of order of species to arrange secondary level. Default is alphabetical
#' @param mp Numeric vector c(rows, columns) for distribution of facets
#' @param MinWidth Numeric. Minimum size to shrink unselected interactors to.
#' @param Pad Numeric. Whitespace gap between species.
#' @param IndivFigSize  c(width, height) Size of each facet, specifically the links
#' @param BarSize Thickness of bars representing interactors
#' @param Orientation Either 'horizontal' or 'vertical' orientation. Note that Vertical is currently much better supported!
#' @param EdgeMode Set to 'straight' if you want to avoid default curly lines.
#' @param BoxLabPos c(x_primary,x_secondary) To adjust position of species labels away from graph. Default is based on maximum length of labels.
#' @param IncludePerc Logical - whether or not to show percentage links
#' @param PercPos c(x_p,x_s) To adjust position of percentages away from graph.  Default is based on maximum length of labels.
#' @param PercentageDecimals Number of decimal places to display percentages to. Useful if rare species are rounded to 0.
#' @param CSS_Output_Supress Logical - set to TRUE if you have changed the CSS file manually and don't want it overridden
#' @param PRINT Logical - output generated JavaScript to screen?
#'
#' @return Uses r2d3() to generate an html widget object. Can be viewed either in viewer pane (RStudio V1.2+) or with knitr.
#' See Vignette. As a side effect, saves visjs.js (the vis plotting library), filename.js and filename.css to the working directory.
#' @import dplyr
#' @import tidyr
#' @examples

#' ## Simple Bipartite Style Data Set:
#'\dontrun{  testdata <- data.frame(higher = c("bee1","bee1","bee1","bee2","bee1","bee3"),
#' lower = c("plant1","plant2","plant1","plant2","plant3","plant4"),
#' webID = c("meadow","meadow","meadow","meadow","meadow","meadow"), freq=c(5,9,1,2,3,7))
#' SmallTestWeb <- bipartite::frame2webs(testdata,type.out="array")
#'
#'
#' bipartite_D3(SmallTestWeb, filename = 'demo1')
#'}
#'## For more examples see vignette
#' @export
bipartite_D3<- function(data,
                       filename='bipartiteD3Script',
                       PrimaryLab = 'Primary',
                       SecondaryLab='Secondary',
                       SiteNames=NULL,
                       colouroption=c('monochrome', 'brewer', 'manual')[1],
                       HighlightLab = 'Unlinked',
                       HighlightCol = '#3366CC',
                       monoChromeCol = 'rgb(56,43,61)',
                       ColourBy = c(1, 2)[2],
                       BrewerPalette = 'Accent',
                       NamedColourVector,
                       MainFigSize = NULL,
                       SortPrimary=NULL,
                       SortSecondary=NULL,
                       mp = c(1,1),
                       MinWidth=10,
                       Pad=1,
                       IndivFigSize= c(200, 400),
                       BarSize = 35,
                       Orientation = c('vertical', 'horizontal')[1],
                       EdgeMode = c('straight','smooth' )[2],
                       BoxLabPos = NULL,
                       IncludePerc = TRUE,
                       PercentageDecimals =0,
                       PercPos = NULL,
                       CSS_Output_Supress = FALSE,
                       PRINT=FALSE){
  df<-NULL

  if(tibble::is_tibble(data)){
    data<- as.data.frame(data)
  }

  if(is.data.frame(data)){
    df<- data
  }else{
    if(is.list(data)){
      df<-List2DF(data, PrimaryLab,SecondaryLab,SiteNames)
    }else{
      dimensions<-length(dim(data))
      if(dimensions ==3){
        df<-Array2DF(data, PrimaryLab,SecondaryLab,SiteNames)
      }
      if(dimensions==2){
        df<-Matrix2DF(data,PrimaryLab,SecondaryLab, SiteNames)
      }
    }
  }
  if(is.null(df)){#
    stop('invalid data input. Valid forms are data frame, bipartite style matrix, list or array')
  }

  if((mp[2]*mp[1])> (ncol(df)-2)){
    warning('Making too many facets. Are you sure mp is set ok?')
  }
  if((mp[2]*mp[1])< (ncol(df)-2)){
    warning('Making too few facets. Guessing you want 1 row')
    mp[2]<-ncol(df)-2
  }

  if(is.null( MainFigSize)){
    MainFigSize<-c(mp[2]*700, mp[1]*700) # Having a reasonable guess at a default overall figure size
  }

  BP_JS_Writer(df = df,filename = filename,
               colouroption = colouroption,
               HighlightLab = HighlightLab,
               HighlightCol = HighlightCol,
               monoChromeCol = monoChromeCol,
               ColourBy = ColourBy,
               BrewerPalette = BrewerPalette,
               NamedColourVector = NamedColourVector,
               MainFigSize = MainFigSize ,
               SortPrimary = SortPrimary,
               SortSecondary = SortSecondary,
               mp = mp,
               MinWidth = MinWidth,
               Pad = Pad,
               IndivFigSize = IndivFigSize,
               BarSize = BarSize ,
               Orientation = Orientation,
               EdgeMode = EdgeMode,
               AxisLabels= c(PrimaryLab, SecondaryLab),
               FigureLabel = SiteNames,
               BoxLabPos = BoxLabPos,
               IncludePerc = IncludePerc ,
               PercentageDecimals = PercentageDecimals,
               PercPos = PercPos,
               CSS_Output_Supress = CSS_Output_Supress ,
               PRINT =  PRINT)

  LoadVisJS()

  r2d3::r2d3(data = NA, script = paste0(filename,".js"),
       height= MainFigSize[2], width= MainFigSize[1] ,
       dependencies ="vizjs.js")
}
