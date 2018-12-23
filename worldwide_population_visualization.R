
### Part I. Create the data frame from HTML file 

### Load LatLon.rda,

library(XML)
library(RCurl)


### Download the html file from the url below, and parse it to html_text.
url = "https://en.m.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"
# Download the html file and save it as "CountryPopulation.html"
download.file(url, "CountryPopulation.html")
# Parse the file to html_text
html_text = htmlParse("CountryPopulation.html", encoding = "UTF-8")


### Read the tables in html_text with the readHTMLTable() function, set the 1st row as header.
# Read the tables from html_text
tables = readHTMLTable(doc = html_text, header = TRUE)
# Get the table of population from tables
population = tables[[3]]

### Simplify the data frame.
# First, remove the unnecessary columns.
population = population[, c("Country or area\n", "Population(1 July 2016)[3]", "Population(1 July 2017)[3]")]
# Then, rename the column names
colnames(population) = c("Country", "Population_2016", "Population_2017")
# Show the first 5 row of the new "population" data frame
head(population, 5)
# Remove all the annotations in country names and change country names to uppercase
pattern = "\\[[[:alpha:]]\\]"
population["Country"] = sapply(population["Country"], function(x)toupper(gsub(pattern, "", x)))
# Show the first 5 row of the new population data frame
head(population, 5)


# Load the "LatLon.rda" table from the local folder
load("LatLon.rda")
# Merge (inner join) two dataframes by "Country"
AllData = merge(LatLon, population, by = "Country")
# Show the first 5 rows of AllData
head(AllData, 5)

### Finally, convert the population data to numeric values, and
### calculate the 2016-2017 growth rate percentage of population by country,
### and add the growth rate to AllData as a new column named "Growth". 

# Conver population data to numeric values
AllData$Population_2016 = as.numeric(sapply(AllData$Population_2016, function(x)gsub(",", "", x)))
AllData$Population_2017 = as.numeric(sapply(AllData$Population_2017, function(x)gsub(",", "", x)))
# Add "Growth" column to the AllData
AllData$Growth = (AllData$Population_2017 / AllData$Population_2016 - 1) * 100
# Show the last 5 rows of AllData
tail(AllData, 5)


### Part II.  Create a KML document for google earth visualization 
# Create a base document named doc1
doc1 = newXMLDoc()
# Create nodes "kml" and "document"
kml = newXMLNode("kml", doc = doc1)
document = newXMLNode("Document", parent = kml)

addPlacemark = function(lat,lon,country,code,pop16,pop17,growth,parent){
  # Create a "pm"(Placemark) node who has an attribute named "id" and the relevant value is "code"
  # Assign "parent" as the parent of pm
  pm = newXMLNode("Placemark",attrs=c(id=code),parent=parent)
  # Create "name" node, whose content is "country" and parent is pm
  newXMLNode("name",country,parent=pm)
  # Create "description" node, whose content is about a country's population information and parent is "pm"
  newXMLNode("description",paste(country,"\n population_2016: ",pop16,"\n population_2017: ",pop17,"\n growth: ",growth,sep =""),parent=pm)
  # Create "Point" node, whpse content is a country's coordinates and parent is pm
  newXMLNode("Point",newXMLNode("coordinates",paste(c(lon,lat,0),collapse=",")),parent=pm)
}


# First, define cutpoints for population_2017 and growth
# "cutPoints" is for population_2017 and "cutPoints2" is for growth
cutPoints = unname(quantile(AllData$Population_2017,probs = c(0, 0.05, 0.3, 0.7, 0.95, 1)))
cutPoints2  = unname(quantile(AllData$Growth,probs = c(0, 0.05, 0.3, 0.7, 0.95, 1)))
# Change the value of the first and last break points so that all the data can be assigned to a category
cutPoints[1] = cutPoints[1] - 1
cutPoints[6] = cutPoints[6] + 1
cutPoints2[1] = cutPoints2[1] - 1
cutPoints2[6] = cutPoints2[6] + 1
# Create categories for population_2017 and growth
pop17Cut = as.numeric(cut(AllData$Population_2017, breaks = cutPoints))
growCut = as.numeric(cut(AllData$Growth, breaks = cutPoints2))


addPlacemark = function(lat,lon,country,code,pop16,pop17,growth,parent,pop17cut,growcut,style=TRUE){
  # Create a "pm"(Placemark) node who has an attribute named "id" and the relevant value is "code"
  # Assign "parent" as the parent of pm
  # Create a "name" node whose content is "country" and assign it as the child of "pm" node.
  pm = newXMLNode("Placemark",newXMLNode("name",country),attrs=c(id=code),parent=parent)
  # Create "description" node, whose content is about a country's population information and parent is "pm"
  newXMLNode("description",paste(country,"\n population_2016: ",pop16,"\n population_2017: ",pop17,"\n growth: ",growth,sep =""),parent=pm)
  # Create "Point" node, whpse content is a country's coordinates and parent is pm
  newXMLNode("Point",newXMLNode("coordinates",paste(c(lon,lat,0),collapse=",")),parent=pm)
  # If we need to add style, then create a "styleUrl" node whose content is the color according to categories and parent is "pm"
  if(style){newXMLNode("styleUrl",paste("#YOR",growcut,"-",pop17cut,sep=''),parent=pm)}
}


### function addStyle(), by which we can add style information to KML file.
addStyle = function(parent,scales,colors){
  for(j in 1:5){
    for(k in 1:5){
      st = newXMLNode("Style",attrs=c("id"=paste("YOR",j,"-",k,sep="")),parent=parent)
      newXMLNode("IconStyle",newXMLNode("Icon",paste("color_label/label_",colors[j],".png",sep="")),newXMLNode("scale",scales[k]),parent=st)
    }
  }
}

color_label = c("blue", "green", "yellow", "orange", "red")
scale_label = c(1, 2, 3, 4, 5)


# First, similarly create doc2 as what Q8 did
doc2 = newXMLDoc()
kml = newXMLNode("kml", doc = doc2)
document = newXMLNode("Document", parent = kml)
# Add style information
addStyle(document, scale_label, color_label)
# Add placemarks
for(i in 1: nrow(AllData)){
  curRow = AllData[i, ]
  xpathSApply(doc2, "/kml/Document", 
              function(x)addPlacemark(curRow$Latitude, curRow$Longitude, curRow$Country,
                                      curRow$Code, curRow$Population_2016, curRow$Population_2017,
                                      curRow$Growth, x, pop17Cut[i], growCut[i],style=TRUE))
}

# Save doc2 as "Part3.kml"
saveXML(doc2, file = "Part3.kml")