#### THIS CODE GENERATES DIAGRAMS FOR NASIS ####
#### You must have a working NASIS ODBC connection to run this code #### 

#load required libraries
library(DBI) #for connecting to NASIS
library(dm) #for building the model and plotting diagram
library(soilDB) #for querying specific tables
library(dplyr) #for joining and filtering data
library(viridis) #for assigning colors to the tables
library(readr) #for adding custom code into the diagram
library(DiagrammeRsvg) #for saving as an SVG file
library(rsvg) #for saving SVG as a pdf

#### THIS SECTION WILL GENERATE ONE DIAGRAM FOR ALL VISIBLE UNRESTRICTED TABLES IN NASIS ####

#the current deployed system table of must be loaded into the NASIS local database for this code to work

#create a connection to local NASIS database
con <- dbConnect(odbc::odbc(), "nasis_local", timeout = 10, uid = "NASISSQLRO", pwd = "nasisRe@d0n1y365")

#query NASIS and build data model object (this takes a bit so be patient)
nasisdm <- dm_from_con(con, dbname = "Nasis-local")

#get the rows of data in the systemtable table -- the current system (only one) needs to be in your local database -- this will produce a list of all tables in the system, then use the systemtable tabvisible column to filter out tables which are hidden
pt4 <- dbQueryNASIS(con, q ="select * FROM systemtable", close = F) %>% filter(tabvisible == T)

#select only the tables matching the filtered ones from the data model
nasisdm2 <- nasisdm %>% dm_select_tbl(any_of(pt4$tabphynm)) %>% dm()

#get the rows of data from the table collection table --- this is for determining the coloring of the diagram. Other methods of coloring could be used to emphasize certain data structures, but the table collection grouping seemed like a good fit. The next part for filtering. in the same way the tables are marked visible or not, the table collections are also marked, in addition there are some tables that are visible to a restricted nasis user group. These are filtered out here, but could be adjusted depending on the audience --- for example expert users may want to view the system tables structure. For this, change nonrestrictedvisible to F
nclt3 <- dbQueryNASIS(con, q ="select * FROM tablecollection", close = F) %>% filter(visibleingrideditor == T & nonrestrictedvisible == T)

#get the rows of data for the system table --- this is for identifying the system version in the diagram and for and placing it in the title
stn2 <- dbQueryNASIS(con, q ="select * FROM system", close = F)

#join the filtered table collection table data to the dataframe created above, this is for getting the table collection names and physical table names matched
pt3 <- pt4 %>% inner_join(nclt3, by = join_by(tablecollectiidref == tablecollectiid, sysiidref == sysiidref))

#filter the data model to only include the tables in the joined/filtered table
nasisdm3 <- nasisdm2 %>% dm_select_tbl(any_of(pt3$tabphynm))

#make a data frame with the unique table collections and assign a viridis color palette hex code. Should consider also trying other palettes
colpt <- data.frame(tablecollectiidref = unique(nclt3$tablecollectiid), color = viridis(length(unique(nclt3$tablecollectiid))))

#join the colors with the pt3 table
pt3 <- pt3 %>% inner_join(colpt)

#dm packges uses a mechanism to assign colors and descriptions via set_names. Use the table name and the color here to make the vector to assign the colors
tt6 <- rlang::set_names(pt3$tabphynm, pt3$color)

#set the colors of the data model with the vector created
nasisdm3 <- nasisdm3 %>% dm_set_colors(!!!tt6)

#make a vector for renaming the physical table names to the table labels
tt5 <- rlang::set_names(pt3$tabphynm, pt3$tablab)

#need the rlang for setting descriptions, the names must be unique, if they are not it will cause it to fail
tt4 <- rlang::set_names(pt3$tabphynm, pt3$tabphynm)

#add descriptions as table physical names --- actual descriptions could also be used but the diagram gets a bit too busy
nasisdm3 <- nasisdm3 %>% dm_set_table_description(any_of(!!tt4))

#rename the tables to the table labels using the vector, once you do this it's a bit more challenging to filter or select the tables, as the table labels have spaces
nasisdm3 <- nasisdm3 %>% dm_rename_tbl(any_of(!!tt5))


#consider replacing the color contrast formula with another one more closely following 508 compliance uncomment and run the next line to open the editor
trace(dm:::is_dark_color, edit = T)


#remove the comment marks and replace the content of the function in the editor with the following function
# function (rgb)
# {
#   rgb_conv <- lapply(rgb, function(x) {
#     i <- x/255
#     if (i <= 0.03928) {
#       i <- i/12.92
#     }
#     else {
#       i <- ((i + 0.055)/1.055)^2.4
#     }
#     return(i)
#   })
#   rgb_calc <- (0.2126 * rgb_conv[[1]]) + (0.7152 * rgb_conv[[2]]) +
#     (0.0722 * rgb_conv[[3]])
#   if (rgb_calc > 0.179)
#     return(F)
#   else return(T)
# }

#make a plot object of the dm --- this is the full diagram with all the tables --- use view_type = "all" to see all columns in all tables --- this might be better applied for the individual table collections, change edge_attrs for making layers
p <- nasisdm3 %>% dm_draw(view_type = "title_only", rankdir ='RL', graph_name = stn2$sysver, node_attrs = "fontname = Arial", graph_attrs = c(paste0("label = '", stn2$sysver, "\nData Model Diagram'"), "labelloc = 't'", "fontsize = 72", "fontname = Arial"), font_size = c(table_description = 12L))



#load dot code for legend and another for the find related paths
dotleg <- read_file("customlegend-nasis-diagram.txt")
fndrlt <- read_file("customfindrelate.txt")
#insert manually created legend into the existing model diagram --- note this also changes the packmode to graph
p$x$diagram <- gsub(pattern = 'packmode= "node"', replacement = dotleg, x = p$x$diagram)

#insert find/relate paths --- there are still a ton of find / relate paths to add --- still working to possibly do this automatically
p$x$diagram <- gsub(pattern = "}$", replacement = fndrlt, x = p$x$diagram)

#plot the object to view
p

#save the plot as a .gv file -- you can open and edit/view this file in Rstudio --- might be easier to add the custom code directy in to this file versus loading in via R code/scripts
writeBin(p$x$diagram, con = paste0(stn2$sysver, "-datamodel-", Sys.Date(), ".gv"))

#export the diagram as an SVG for best quality
psvg <- DiagrammeRsvg::export_svg(gv = p)
writeBin(psvg, con = paste0(stn2$sysver, "-datamodel-", Sys.Date(), ".svg"))

#convert the svg to PDF
rsvg::rsvg_pdf(paste0(stn2$sysver, "-datamodel-", Sys.Date(), ".svg"), paste0(stn2$sysver, " Data Model Structure Diagram-", Sys.Date(), ".pdf"))

#you may want to adjust the layering of the .pdf or .svg so screen readers will read the diagram in a logical order.
