###### THIS CODE IS FOR BUILDING THE MATERIALS FOR THE DIAGRAM GUIDE #############

#load required libraries
library(DBI) #for connecting to NASIS
library(dm) #for building the model and plotting diagram
library(soilDB) #for querying specific tables
library(dplyr) #for joining and filtering data
library(viridis) #for assigning colors to the tables
library(readr) #for adding custom code into the diagram
library(DiagrammeRsvg) #for saving as an SVG file
library(rsvg) #for saving SVG as a pdf
library(ggplot2) # for plotting the legend
library(cowplot) # for extracting the legend from ggplot

#the current deployed system table of must be loaded into the NASIS local database for this code to work

#create a connection to local NASIS database
con <- dbConnect(odbc::odbc(), "nasis_local", timeout = 10, uid = "NASISSQLRO", pwd = "nasisRe@d0n1y365")

#query NASIS and build data model object (this takes a bit so be patient)
nasisdm <- dm_from_con(con, dbname = "Nasis-local")

#get the rows of data in the systemtable table -- the current system (only one) needs to be in your local database -- this will produce a list of all tables in the system, then use the systemtable tabvisible column to filter out tables which are hidden
pt4 <- dbQueryNASIS(con, q ="select * FROM systemtable", close = F) %>% filter(tabvisible == T)

#select only the tables matching the filtered ones from the data model
nasisdm2 <- nasisdm %>% dm_select_tbl(any_of(pt4$tabphynm)) %>% dm()

#get the rows of data from the table collection table --- this is for determining the coloring of the diagram. Other methods of coloring could be used to emphasize certain data structures, but the table collection grouping seemed like a good fit. The next part for filtering. in the same way the tables are marked visible or not, the table collections are also marked, in addition there are some tables that are visible to a restricted nasis user group. These are filtered out here, but could be adjusted depending on the audience --- for example expert users may want to view the system tables structure. For this, change nonrestrictedvisible to F. The arrangement of the table collections here is important for later as the color assignments are done based on this order. The arrangement chosen here is based on the sequence the NASIS client displays them in the table of contents.
nclt3 <- dbQueryNASIS(con, q ="select * FROM tablecollection", close = F) %>% filter(visibleingrideditor == T & nonrestrictedvisible == T) |> arrange(tablecollectsequence)

#get the rows of data for the system table --- this is for identifying the system version in the diagram and for and placing it in the title
stn2 <- dbQueryNASIS(con, q ="select * FROM system", close = F)

#join the filtered table collection table data to the dataframe created above, this is for getting the table collection names and physical table names matched
pt3 <- pt4 %>% inner_join(nclt3, by = join_by(tablecollectiidref == tablecollectiid, sysiidref == sysiidref))

#filter the data model to only include the tables in the joined/filtered table
nasisdm3 <- nasisdm2 %>% dm_select_tbl(any_of(pt3$tabphynm))

#make a data frame with the unique table collections and assign a viridis turbo color palette hex code. Should consider also trying other palettes
colpt <- data.frame(tablecollectiidref = unique(nclt3$tablecollectiid), color = turbo(length(unique(nclt3$tablecollectiid))))

#join the color table with the table data
colptj <- colpt |> inner_join(nclt3, join_by(tablecollectiidref == tablecollectiid)) |> arrange(tablecollectsequence)

# build a plot with the table colors and legend
p <- ggplot(colptj) +
  geom_bar(aes(tablecollectsequence, fill = tablecollectname)) +
  coord_flip() +
  scale_fill_manual(values = colptj$color, breaks = colptj$tablecollectname, name = "Diagram Table Collection Color Legend", guide = guide_legend(ncol = 1)) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14), text = element_text(family = "Arial"))

# extract the legend from the plot for saving
pl <- get_legend(p)
# draw the legend
ggdraw(pl)
# save the legend as .svg
ggsave(paste0(stn2$sysver, " Data Model Diagram Table Collection Color Legend-", Sys.Date(), ".svg"), pl, width = 8.5, height = 11)

######### THIS SECTION IS FOR BUILDING THE EXAMPLES IN THE DIAGRAM GUIDE ###################

#load find/relate dot code for table subset included in guide
fndrlt <- read_file("customfindrelate-guide.txt")

#load relationship path guide .gv, this was created by manually editing the customlegend-nasis-diagram.txt (on github)
reldes <- DiagrammeR::grViz("legend-relate-guide.gv")

#export and save as SVG for use in diagram guide powerpoint
reldesvg <- DiagrammeRsvg::export_svg(gv = reldes)
writeLines(reldesvg, con = paste0(stn2$sysver, "-datamodel-legend-relate-guide-", Sys.Date(), ".svg"))

########### prepare data for extracting subset of tables ############

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

#select only the tables of interest for the guide
nasisdm3fil <- nasisdm3 |> dm_select_tbl("Distribution Metadata", "Distribution Interp Metadata", "Distribution Legend Metadata", "Distribution Text Metadata", "Distribution Mapunit Metadata", "Distribution Component Metadata", "Legend", "Legend Certification History", "Legend Export Certification History", "Legend Area Overlap", "Legend Text", "Legend Mapunit", "Legend Mapunit History", "Legend Mapunit Text", "Legend Mapunit Area Overlap", "Area", "Area Type", "Area Text")

#build diagram including the column names
p <- dm_draw(nasisdm3fil, view_type = "all", rankdir = "RL", edge_attrs = "dir = back", node_attrs = "fontname = Arial", font_size = c(table_description = 12L))

#insert find/relate paths for the guide 
p$x$diagram <- gsub(pattern = "}$", replacement = fndrlt, x = p$x$diagram)

#fix mis-aligned arrows in tables
p$x$diagram <- gsub(pattern = '(?<!PORT="areaiidref")(>areaiidref<)', replacement = ' PORT="areaiidref">areaiidref<', perl = T, x = p$x$diagram)
p$x$diagram <- gsub(pattern = '(BGCOLOR="#D1FAEFFF">liidref<)', replacement = 'BGCOLOR="#D1FAEFFF" PORT="liidref">liidref<', perl = T, x = p$x$diagram)

#plot to check diagram
p

#write to .gv
writeBin(p$x$diagram, con = paste0(stn2$sysver, "-datamodel-guide", Sys.Date(), ".gv"))

# export and save as svg for use in diagram guide powerpoint
psvg <- DiagrammeRsvg::export_svg(gv = p)
writeLines(psvg, con = paste0(stn2$sysver, "-datamodel-guide", Sys.Date(), ".svg"))