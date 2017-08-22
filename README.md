# OpenprescribingRplots

A small package for for loading openprescribing.net data into R graphics.

Installation:

`library(devtools)`

`devtools::install_github("fergustaylor/openprescribingRplots")`

Load:

`library(openprescribingRplots)`

__These functions plots leaflet maps overlaying chloropleths of spending data, or list size per CCG.__

 *  plot2017perperson(argument)
 *  plot2017total(argument)
 *  plot2017items(argument)
 *  plot2017quantity(argument)
 *  plot2017quantityperitem(argument)
 *  plot2017listsize()
 
 Update 22/08/17: still debugging to get Leaflet to work.
 
An introduction to the tool is given here - [https://fergustaylor.github.io/blog/post7](https://fergustaylor.github.io/blog/post7)

Further explanation on openprescribingR is given here - [https://fergustaylor.github.io/openprescribingR/](https://fergustaylor.github.io/openprescribingR/)
