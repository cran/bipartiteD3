# News

## v0.3.0 Patch updates to d3

 Updates to d3 and r2d3 had deprecated d3.nest - this version points towards version 5 of d3. 
 
 Also updated some other parts to comply with new CRAN checks. 

## v0.2.0 Several patches and improvements
 
 Updated vignette to point out r2d3 functions to save d3 objects and other updates

 LoadVisJS now checks the a security hash before saving and using. It also no longer prints an unnecessary 'no line ending' warning.

 The number of decimal places on the percentages shown can now be controlled, useful when have weak links.

Sorting by secondary and primary layers now works properly.

 bipartite_d3() now tests for a tibble and automatically converts to a data.frame. 

 New function OrderByCrossover(), which generates orders that minimise crossovers in the network. 

## v0.1.0: First release into the wild.

