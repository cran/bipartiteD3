#' Generate JavaScript file for a bipartite network
#'
#'Function called by bipartite_D3() to write JavaScript and CSS file.
#'In most cases it is better to use bipartite_D3() directly.
#'
#' @param df data.frame containing the names of the interactors and the link strengths. bipartite package data need to be passed through Matrix2DF or Array2DF first.
#' @param filename character string to name the .js and .css files. Do not include a file extension
#' @param colouroption Either 'monochrome', 'brewer' or 'manual'
#' @param HighlightLab Name of interactor to highlight
#' @param HighlightCol Highlight colour
#' @param monoChromeCol If using monochrome option, what colour to use
#' @param ColourBy Which set of interactors to colour by. 1= primary, 2= secondary
#' @param BrewerPalette RColorBrewer palette
#' @param NamedColourVector Named vector of colours for manual colour assignment
#' @param MainFigSize Size of figure, used here to calculate facet spacing.
#' @param SortPrimary Vector detailing order to arrange primary level. Default is alphabetical
#' @param SortSecondary Vector detailing order to arrange secondary level. Default is alphabetical
#' @param mp Numeric vector c(rows, columns)
#' @param MinWidth Numeric. Minumum size to shrink unselected interactors to.
#' @param Pad Numeric. Gap between species.
#' @param IndivFigSize Size of each facet, specifically the interactions.
#' @param BarSize Thickness of bars representing interactors
#' @param Orientation Either 'horizontal' or 'vertical' orientation.
#' @param EdgeMode Set to 'straight' to avoid curly lines.
#' @param AxisLabels c('Primary','Secondary') to override column names of dataframe
#' @param FigureLabel Character vector, to allow overide of use of df column names
#' @param BoxLabPos c(x_p,x_s) To adjust position of species labels. Default is based on maximum length of labels.
#' @param IncludePerc Boolean. whether or not to show percentage links
#' @param PercPos c(x_p,x_s) To adjust postion of percentages.  Default is based on maximum length of labels.
#' @param CSS_Output_Supress Boolean. Set to TRUE if you have changed the CSS file manually and don't want it overriden
#' @param PRINT Boolean. Output generated JavaScript to screen?
#'
#' @return As a side effect, saves visjs.js (vis plotting library), filename.js and filename.css to the working directory.
#' @import dplyr
#' @import tidyr
#' @import bipartite
#' @examples
#'
#' ## Simple Data Set
#' testdata <- data.frame(higher = c("bee1","bee1","bee1","bee2","bee1","bee3"),
#' lower = c("plant1","plant2","plant1","plant2","plant3","plant4"),
#'  Meadow=c(5,9,1,2,3,7))
#'
#' BP_JS_Writer(testdata,PRINT=TRUE)
#'
#'
#' @export
BP_JS_Writer<- function(df, filename='JSBP',
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
                        AxisLabels = NULL,
                        FigureLabel=NULL,
                        BoxLabPos = NULL,
                        IncludePerc = TRUE,
                        PercPos = NULL,
                        CSS_Output_Supress = FALSE,
                        PRINT=FALSE){

  #####################
  #Coverting data frame to JSON format
  #####################
JSON<-'JSON'#Keeps CRAN happpy


  LoadVisJS()

  df%>%
    unite(col = 'JSON', 1:2, sep='","')%>%
    mutate(JSON= paste0('"',JSON, '"'))%>%
    unite(col= 'JSON', sep=',')%>%
    mutate(JSON=paste0('[',JSON,']')) -> JSONColumn


  data<- paste0('var data=[',
                paste0(JSONColumn$JSON,collapse = ',\n'),
                ']\n\n\n')

  ######################
  # Colours
  ###########
  if(!(colouroption %in%  c('monochrome', 'brewer', 'manual'))){
    warning('Invalid Colour Option!
            Must be one of "monochrome", "brewer"or "manual".
            Defaulting to monochrome)')
    colouroption<-'monochrome'}

  ToColour<- unique(df[,ColourBy])
  ToColour<- ToColour[ToColour!=HighlightLab]

  if(colouroption == 'monochrome'){

    colours <- paste0("var color = {'", HighlightLab ,"':'",HighlightCol,"',",
                      paste0(   "'", ToColour, "':'",monoChromeCol,"'",
                                  collapse = ','),
                      "};\n\n") # Don't worry if no unlinked in a dataset, defining extra colours does no harm
  }

  if(colouroption == 'brewer'){
    colours <- paste0("var color = {'", HighlightLab ," ':'",HighlightCol,"',",
                      paste0(  "'",   ToColour, "':'",
                               RColorBrewer::brewer.pal(n=length(ToColour),
                                             name= BrewerPalette), "'",
                                  collapse = ','),
                      "};\n\n")
  }

  if(colouroption == 'manual'){
    ### Needs a named list of colours. Can but does not have to included unlinked. (make sure names don't clash)
    colours <- paste0("var color = {'", HighlightLab ,"':'",HighlightCol,"',",
                      paste0(  "'",  names(NamedColourVector), "':'",
                                 NamedColourVector, "'",
                                 collapse = ','),
                      "};\n\n")

  }

  ########
  ###  Setting Up main figure size
  ########

  SetUp<- paste0(' src="vizjs.js"


  var svg = d3.select("body")
                 .append("svg").attr("width",', MainFigSize[1],
                 ').attr("height", ',MainFigSize[2],');')


  ########
  # Sorting Out Sizing
  ########
  if((mp[2]*mp[1])> (ncol(df)-2)){
    warning('Making too many facets. Are you sure mp is set ok?')
  }
  if((mp[2]*mp[1])< (ncol(df)-2)){
    warning('Making too few facets. Guessing you want 1 row')
    mp[2]<-ncol(df)-2
  }


  if(Orientation=='horizontal' & all(mp !=c(1,1))){
    warning('Horizontal mode not very effective with multiple facets yet')
  }


  if(is.null( MainFigSize)){
    MainFigSize<-c(mp[2]*700, mp[1]*700) # Having a resonable guess at a default overall figure size
  }

  if(is.null(BoxLabPos)){ # If no set value, calculate one

    BoxLabPos<-    (c(max(stringr::str_length(df[,1])),
        max(stringr::str_length(df[,2])))*1.2)+20   # spaces per character
  }

  if(is.null(PercPos)){ # If no set value, calculate one
    PercPos<- (BoxLabPos)*5+c(5,20) #
  }

  LeftSidePadding =20+ BoxLabPos[1] + IncludePerc*PercPos[1]
  RightSidePadding =20+ BoxLabPos[2] + IncludePerc*PercPos[2]

  TotalNeededSidePadding = sum(20+ BoxLabPos + IncludePerc*PercPos) + BarSize + IndivFigSize[1]

  # Widths. Divide available width by number of columns, giving extra space on each side
  WPerPlot <-(MainFigSize[1] -LeftSidePadding) / mp[2]
  ColPos <- rep(floor(seq(from=LeftSidePadding, by= WPerPlot, length=mp[2]  )), mp[1])


  # Heights Divide available width by number of columns, giving 50 extra space on each side
  HPerPlot <- (MainFigSize[2] -100) / mp[1]
  RowPos <- rep(floor(seq(from=50, by= HPerPlot, length=mp[1]  )), each= mp[2] )

  ######
  # Main Loops
  #####

    if(Orientation == 'horizontal'){IndivFigSize <- rev(IndivFigSize)}

  FigureFacets <- ''
  for(i in 1:(ncol(df)-2)){

    #########
    ### Drawing Base Figure
    ###############



    BaseFigure <- paste0('var g',i,' = svg.append("g").attr("transform","translate(',ColPos[i],',',RowPos[i],')");
                         var bp',i,'=viz.bP()
                         .data(data)
                         .value(d=>d[',i+1,'])
                         .min(',MinWidth,')
                         .pad(',Pad,')
                         .height(',IndivFigSize[2],')
                         .width(',IndivFigSize[1],')
                         .barSize(',BarSize,')
                         .fill(d=>color[d.',c('primary',
                                              'secondary')[ColourBy],'])',
                         if(EdgeMode == 'straight'){'\n.edgeMode("straight")\n'}else{'\n'},
                         if(!is.null(SortSecondary)){paste0('.sortSecondary(sort(["',
                                                            paste0(SortSecondary, collapse = '","'),
                                                            '"]))\n')}else{''},
                         if(!is.null(SortPrimary)){paste0('.sortPrimary(sort(["',
                                                          paste0(SortSecondary, collapse = '","'),
                                                          '"]))\n')}else{''},
                         '.orient("',Orientation,'");

g',i,'.call(bp',i,');')


    ###############
    ### Labels
    ############

    if(is.null(AxisLabels)){
      AxisLabels <- colnames(df)[1:2]
    }
    if(is.null(FigureLabel)){
      FigureLabel <- colnames(df)[-c(1,2)]
    }
    if(Orientation=='vertical'){

    Labelling <- paste0('g',i,'.append("text")
                        .attr("x",-50).attr("y",-8)
                        .style("text-anchor","middle")
                        .text("',AxisLabels[1],'");
                        g',i,'.append("text")
                        .attr("x", 250)
                        .attr("y",-8).style("text-anchor","middle")
                        .text("',AxisLabels[2],'");
                        g',i,'.append("text")
                        .attr("x",100).attr("y",-25)
                        .style("text-anchor","middle")
                        .attr("class","header")
                        .text("',FigureLabel[i],'");')
    }else{
      Labelling <- paste0('g',i,'.append("text")
                        .attr("x",0).attr("y",-10)
                        .style("text-anchor","middle")
                        .attr("class","header")
                        .text("',FigureLabel[i],'");')
    }

    ######
    ## MouseOver Select
    #####

    MouseOver <- paste0('\n\n g',i,'.selectAll(".mainBars")
                        .on("mouseover",mouseover)
                        .on("mouseout",mouseout);')
    ##########
    #Labels
    ##########

    if(Orientation=='vertical'){
        BoxLabels <- paste0('\n\n g',i,'.selectAll(".mainBars").append("text").attr("class","label")
                        .attr("x",d=>(d.part=="primary"? -',BoxLabPos[1],':',BoxLabPos[2],'))
                        .attr("y",d=>+6)
                        .text(d=>d.key)
                        .attr("text-anchor",d=>(d.part=="primary"? "end": "start"));')

    if(IncludePerc){
      BoxPerc <- paste0('\n\n g',i,'.selectAll(".mainBars").append("text").attr("class","perc")
                        .attr("x",d=>(d.part=="primary"? -',PercPos[1],':',PercPos[2],'))
                        .attr("y",d=>+6)
                        .text(function(d){ return d3.format("0.0%")(d.percent)})
                        .attr("text-anchor",d=>(d.part=="primary"? "end": "start")); ')
    }else{
      BoxPerc<- ''
    }

    }
    if(Orientation=='horizontal'){

      BoxLabels <- paste0('\n\n g',i,'.selectAll(".mainBars").append("text").attr("class","label")
                        .attr("x",d=>(d.part=="primary"? -',0,':',0,'))
                        .attr("y",d=>(d.part=="primary"? -',BarSize,':',BarSize,'))
                        .text(d=>d.key)
                        .attr("text-anchor",d=>(d.part=="primary"? "middle ": "middle "));')

      if(IncludePerc){
        BoxPerc <- paste0('\n\n g',i,'.selectAll(".mainBars").append("text").attr("class","perc")
                        .attr("x",d=>(d.part=="primary"? -',0,':',0,'))
                        .attr("y",d=>(d.part=="primary"? -',BarSize+15,':',BarSize+15,'))
                        .text(function(d){ return d3.format("0.0%")(d.percent)})
                        .attr("text-anchor",d=>(d.part=="primary"? "middle ": "middle ")); ')
      }else{
        BoxPerc<- ''
      }


    }
    FigureFacets <- paste0(FigureFacets,'\n\n\n',BaseFigure,Labelling,MouseOver,  BoxLabels, BoxPerc)
  }

  ###################
  # MouseOver and MouseOut Functions
  ############

  is<- 1:(ncol(df)-2)

  MO_funcs<- paste0( '\n\nfunction mouseover(d){\n',
                     paste0('bp',is,'.mouseover(d);
                            g',is,'.selectAll(".mainBars")
                            .select(".perc")
                            .text(function(d){ return d3.format("0.0%")(d.percent)});', collapse='\n'),
                     '\n}

                     function mouseout(d){\n',

                     paste0('bp',is,'.mouseout(d);
                            g',is,'.selectAll(".mainBars")
                            .select(".perc")
                            .text(function(d){ return d3.format("0.0%")(d.percent)});', collapse='\n'),
                     '\n}'
                     )


  #######
  # Final Output
  #########

  Output<- paste0(data, '\n function sort(sortOrder){
                    return function(a,b){ return d3.ascending(sortOrder.indexOf(a),sortOrder.indexOf(b)) }
                  }\n',
                  colours,
                  FigureFacets,
                  MO_funcs, sep='\n\n')
  writeLines(Output, paste0(filename,'.js'))


  ######
  # CSS File - Feel free to tinker!
  ######

  writeLines(
    ' .mainBars{
    shape-rendering: auto;
    fill-opacity: 1;
    stroke-width: 0.5px;
    stroke: rgb(0, 0, 0);
    stroke-opacity: 0;
  }
    .subBars{
    shape-rendering:crispEdges;
    }
    .edges{
    stroke:none;
    fill-opacity:0.3;
    }
    .label{
    color:#000000;
    }', paste0(filename,'.css'))




  if(PRINT){cat(Output)}
  }
