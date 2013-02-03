#####################################################################
#                                                                   #
# ProcessCompleteGrainMatrices.R                                    #
#                                                                   #
# Peter Owlett (peter.owlett@capgemini.com)                         #
# Copyright Capgemini 2012                                          #
#                                                                   #
# Purpose:  Reads in a pairing of data dimensions and measures then #
#           creates a full list of all pairings and a cross matrix  #
#           mapping measures to domains then exports both as CSV.   #
#                                                                   #
# Use:      The script is looking for a pipe delimited set of files #
#           at a hard coded location called myFilePath.             #
#                                                                   #
#####################################################################

### Libraries
library(xlsReadWrite)   # For reading excel files
library(igraph)         # For generating adjacency matrices
library(Matrix)         # Req for igraph
library(lattice)        # Req for igraph
library(waterfall)      # produces the waterfall chart
library(rjson)          # used to export the json matrix for d3 anims

#####################################################################

### 1. Load in the various data sources from Excel

# First grab the 5TD source
  #Grab the file list
  listSources_5TD <- list.files(path="D:/00_Dev/GrainTransformer/input/5TD/"
                                 ,pattern="*.xls"
                                 ,full.names=TRUE
                                 ,ignore.case=TRUE)

  # Check we only got one 5TD file - if not, then stop
  if (length(listSources_5TD) > 1) {stop("Too many 5TD Sources")}


  

  # Read in the excel file
  source_5TD <- read.xls(as.character(listSources_5TD),
                         colNames = FALSE,
                         sheet = 1,
                         type = "data.frame",
                         from = 1,
                         rowNames = FALSE)  

  # Label the columns
  names(source_5TD) <- c("RecordId","Dimensions","Measures")

# Next grab all the Data Source sources
# Create a list of all the Data Sources source files
listSources_DataSource <- list.files(path="D:/00_Dev/GrainTransformer/input/RegionalDataSource/"
                        ,pattern="*.xls"
                        ,full.names=TRUE
                        ,ignore.case=TRUE)

# For each xls in the data source bin, create a data frame with same name
for (source_DataSource in listSources_DataSource) {
  
  # Pull out the file name using a regex
  sourceName <- strsplit(source_DataSource,"[[:punct:]]")[[1]][(length(strsplit(source_DataSource,"[[:punct:]]")[[1]])-1)]
    
  # create the dataframe using read.xls
  assign(paste("sourceRegionalDataSource_",sourceName,sep="")
        ,read.xls(
          as.character(source_DataSource),
          colNames = FALSE,
          sheet = 1,
          type = "data.frame",
          from = 1,
          rowNames = FALSE))
  
  # delete any empty string rows (due to excel badness)
  sourceOb <- paste("sourceRegionalDataSource_",sourceName,sep="")
  toBeRemoved<-which(get(sourceOb)[,2]=="")
  toBeRemoved<-c(toBeRemoved, which(get(sourceOb)[,3]==""))
  if (length(toBeRemoved) > 0) {
    assign(sourceOb, get(sourceOb)[-toBeRemoved,])
  }
    
}


# Lastly grab all the As Is souces
# Create a list of all the As Is source files
listSources_AsIs <- list.files(path="D:/00_Dev/GrainTransformer/input/RegionalAsIs"
                               ,pattern="*.xls"
                               ,full.names=TRUE
                               ,ignore.case=TRUE)

# Loop through each AsIs source
for (source_AsIs in listSources_AsIs) {
  
  # Pull out the file name using a regex
  sourceName <- strsplit(source_AsIs,"[[:punct:]]")[[1]][(length(strsplit(source_AsIs,"[[:punct:]]")[[1]])-1)]
  
  # create the dataframe using read.xls
  assign(paste("sourceRegionalAsIs_",sourceName,sep="")
         ,read.xls(
           as.character(source_AsIs),
           colNames = FALSE,
           sheet = 1,
           type = "data.frame",
           from = 1,
           rowNames = FALSE))
  
  # delete any empty string rows (due to excel badness)
  sourceOb <- paste("sourceRegionalAsIs_",sourceName,sep="")
  toBeRemoved<-which(get(sourceOb)[,1]=="")
  toBeRemoved<-c(toBeRemoved, which(get(sourceOb)[,2]==""))
  if (length(toBeRemoved) > 0) {
    assign(sourceOb, get(sourceOb)[-toBeRemoved,])
  }
  
}


# Grab the text normalisation tables
normalisationTableSources <- read.xls(as.character("D:/00_Dev/GrainTransformer/input/NormalisationLookup/NormTab.xls"),
                                       colNames = FALSE,
                                       sheet = 1,
                                       type = "data.frame",
                                       from = 2,
                                       rowNames = FALSE)
normalisationTableMeasures <- read.xls(as.character("D:/00_Dev/GrainTransformer/input/NormalisationLookup/NormTab.xls"),
                       colNames = FALSE,
                       sheet = 2,
                       type = "data.frame",
                       from = 1,
                       rowNames = FALSE)




# Clean up
rm(sourceName, source_AsIs, listSources_AsIs, source_DataSource, listSources_DataSource)
rm(listSources_5TD, toBeRemoved, sourceOb)


#####################################################################

### 2. Create and export the normalised pairs listing

### 2.0 Build a function to produce edge lists
produceEdgeList <- function(mySourceData, myName){
  
  # Create the empty edge list data frame
  myEdges <- data.frame()

  # Loop through each row in the first column of the data frame 
  for(i in 1:length(mySourceData[,2])) 
  { 
    
    # Load the two lists into seperate temporary data frames
    dimNameString <- gsub(" +,",",",as.character(mySourceData[i,2]))
    dimNameString <- gsub("\r","",dimNameString)
    dimNameString <- gsub("\n",",",dimNameString)
    dimNameString <- gsub(";",",",dimNameString)
    dimNameString <- gsub(", +",",",dimNameString)
    dimNameString <- gsub("^ +",",",dimNameString)
    dimNameString <- gsub(" +$",",",dimNameString)  
    dimNames <- as.data.frame(strsplit(dimNameString,",")) 
    
    if(length(dimNames[,1]) > 0) {
      dimNames[,1] <- paste("DIM_",toupper(dimNames[,1]), sep="")  
    }
    

    
    msrNameString <- gsub(" ,",",",as.character(mySourceData[i,3]))
    msrNameString <- gsub("\r","",msrNameString)
    msrNameString <- gsub("\n",",",msrNameString)
#    msrNameString <- gsub(";",",",msrNameString) # Removed on AMs guidance
    msrNameString <- gsub(", ",",",msrNameString)
    msrNameString <- gsub("^ +",",",msrNameString)
    msrNameString <- gsub(" +$",",",msrNameString)
    msrNames <- as.data.frame(strsplit(msrNameString,",")) 
    
    if(length(msrNames[,1]) > 0) {
      msrNames[,1] <- paste("MSR_",toupper(msrNames[,1]),sep="")
    }
    
    # Pull through the report ID
    msrNames <- cbind(msrNames,mySourceData[i,1])
    
    
    # Use the merge function to create the cross join
    crossJoinedResult <- merge(dimNames,msrNames,all=TRUE)
    crossJoinedResult <- cbind(crossJoinedResult,1)
    names(crossJoinedResult) <- c("Dimension","Measure","RecordId","Weight")
    
    
    # Load the result into the normalised output data frame
    myEdges <- rbind(myEdges, crossJoinedResult)
    
    # Destroy the temporary variables ready for the next loop
    rm(dimNames, msrNames, crossJoinedResult,dimNameString, msrNameString)
    
  }
  
  # Update all the node names that appear in the normalisation table to the new names 
  myEdges[,2] <- as.character(normalisationTableMeasures[match(myEdges[,2],normalisationTableMeasures[,1]),2]) 
  
  # Remove the DELETES
  myEdges <- myEdges[!(myEdges$Measure %in% c("DELETE")),]
  
  
  # Export the normalised listing as csv
  write.table(myEdges
              ,file=paste("D:/00_Dev/GrainTransformer/output/",myName,".csv",sep="")
              ,quote=FALSE
              ,sep=","
              ,row.names=FALSE
              ,col.names=TRUE)
  
  return(myEdges)
  
}

### 2.1 First up process the 5TD edges
edges_5TD <- produceEdgeList(source_5TD,"edges_5TD")

### 2.2 Cycle through the AS-IS and TO-BE sources producing edge lists

# Create a list of all the source DataSource and AsIs data frames to be processed
listSources_DataSource <- apropos("sourceRegionalDataSource", where = FALSE, mode = "any")
listSources_AsIs <- apropos("sourceRegionalAsIs", where = FALSE, mode = "any")

# cycle through the AS-IS list to produce the edge lists
for (source_DataSource in listSources_DataSource){
  
  myNameShort <- strsplit(source_DataSource,"[[:punct:]]")[[1]][(length(strsplit(source_DataSource,"[[:punct:]]")[[1]]))]
  myName <- paste("sourceRegionalDataSource_",myNameShort,sep="")
  myEdgeName <- paste("edgesDataSource_",myNameShort,sep="")
  assign(myEdgeName,produceEdgeList(get(myName), paste("edgesDataSource_",myNameShort,sep="")))
  
}

# cycle through the TO-BE list to produce the edge lists
for (source_AsIs in listSources_AsIs){
  
  myNameShort <- strsplit(source_AsIs,"[[:punct:]]")[[1]][(length(strsplit(source_AsIs,"[[:punct:]]")[[1]]))]
  myName <- paste("sourceRegionalAsIs_",myNameShort,sep="")
  myEdgeName <- paste("edgesAsIs_",myNameShort,sep="")
  assign(myEdgeName,produceEdgeList(get(myName), paste("edgesAsIs_",myNameShort,sep="")))
  
}

# clean up
rm(listSources_DataSource, listSources_AsIs, myEdgeName, myName, myNameShort, source_DataSource, source_AsIs)


#####################################################################

### 3. Create and export the grain matrix using the iGraph library

# This function initialises the graphs, produces each grain matrix
# then converts into a full (non-sparse) matrix

getGrainMatrix <- function(edgeList, myNameShort){
  
  # Grab the actual object
  edgeListObj <- get(edgeList)
  
  # Create the graph from the edge list
  myGraph <<- graph.data.frame(edgeListObj, directed=FALSE)
  
  # Create a named instance of the graph for use in JSON export
  myGraphName <- paste("grp_",myNameShort,sep="")
  assign(myGraphName, myGraph, pos=1)
  
  # Grab the adjacency matrix
  grainMatrix <- get.adjacency(myGraph, type=c("both"), attr="Weight")
  
  # Convert to a full matrix
  grainMatrix <- as.matrix(grainMatrix)
  
  # Export the matrix as csv
  exportPath <- paste("D:/00_Dev/GrainTransformer/output/",myNameShort,".csv",sep="")
  write.csv(grainMatrix, exportPath)
  
  return(grainMatrix)
  
}

# Get the 5TD grain matrix
grain_5TD <- getGrainMatrix(c("edges_5TD"),"grain_5TD")

# Create a list of all the edge lists for Data Source and AS IS data frames 
listEdges_DataSource <- apropos("edgesDataSource_", where = FALSE, mode = "any")
listEdges_AsIs <- apropos("edgesAsIs_", where = FALSE, mode = "any")

# cycle through the Data Source list to produce the grain matrices
for (edges_DataSource in listEdges_DataSource){
  
  myNameShort <- strsplit(edges_DataSource,"[[:punct:]]")[[1]][(length(strsplit(edges_DataSource,"[[:punct:]]")[[1]]))]
  myName <- paste("edgesDataSource_",myNameShort,sep="")
  myGrainName <- paste("grainDataSource_",myNameShort,sep="")
  assign(myGrainName,getGrainMatrix(myName,paste("grainDataSource_",myNameShort,sep="")))
  
}

# cycle through the AS-IS list to produce the grain matrices
for (edges_AsIs in listEdges_AsIs){
  
  myNameShort <- strsplit(edges_AsIs,"[[:punct:]]")[[1]][(length(strsplit(edges_AsIs,"[[:punct:]]")[[1]]))]
  myName <- paste("edgesAsIs_",myNameShort,sep="")
  myGrainName <- paste("grainAsIs_",myNameShort,sep="")
  assign(myGrainName,getGrainMatrix(myName,paste("grainAsIs_",myNameShort,sep="")))
  
}


# clean up
rm(edges_DataSource, edges_AsIs, listEdges_AsIs, listEdges_DataSource)
rm(myGrainName, myName, myNameShort)



#####################################################################

### 4. Create the extended matrices to they all match on dims and msrs

# Create the master matrix template
# first grabbing the unique dimensions and measures
    sortaUniqueDimensions <- unique(edges_5TD[,1])
    sortaUniqueMeasures <- unique(edges_5TD[,2])
    
    listEdges_DataSource <- apropos("edgesDataSource_", where = FALSE, mode = "any")
    listEdges_AsIs <- apropos("edgesAsIs_", where = FALSE, mode = "any")
    
    for (edges_DataSource in listEdges_DataSource){
    
      sortaUniqueDimensions <- c(sortaUniqueDimensions, unique(get(edges_DataSource)[,1]))
      sortaUniqueMeasures <- c(sortaUniqueMeasures, unique(get(edges_DataSource)[,2]))
      
    }
    
    for (edges_AsIs in listEdges_AsIs){
    
      sortaUniqueDimensions <- c(sortaUniqueDimensions, unique(get(edges_AsIs)[,1]))
      sortaUniqueMeasures <- c(sortaUniqueMeasures, unique(get(edges_AsIs)[,2]))
      
    }
    
    uniqueDimensions <- unique(sortaUniqueDimensions)
    uniqueMeasures <- unique(sortaUniqueMeasures)
    
    # As we still have symetrical adjacency matrices at this point we need to remerge our lists
    uniqueDimsMsrs <- c(uniqueDimensions, uniqueMeasures)
    uniqueMsrsDims <- c(uniqueMeasures, uniqueDimensions)
    
    
    # then building an empty matrix and convert to data frame
    mmDims <- list(uniqueMsrsDims,uniqueDimsMsrs)
    masterMatrixTemplate <- matrix(ncol=length(mmDims[[2]]),nrow=length(mmDims[[1]]), dimnames=mmDims)
    masterDataFrameTemplate <- as.data.frame(masterMatrixTemplate)


# Set up the full grain function
# NOTE - grainMatrix must be a numeric matrix and masterTemplate must be a data
# frame grainName is just a string for naming the returned object and file write.
# NOTE - the full grains have been normalise so all values are zero or 1
createFullGrain <- function(grainMatrix, grainName, masterTemplate){
  
  # Turn our matrix into a data frame for the merge 
  localGrainName <- paste("full_grain_", grainName,sep="")
  assign(localGrainName, as.data.frame(grainMatrix))
  localGrainObj <- get(localGrainName)
  
  # Merge our grain frame into the template to get all the right columns and rows
  localGrainObj <- merge(localGrainObj, masterTemplate,by="row.names",all.y=TRUE)

  # remove the .y columns that got pulled in
  col_idx <- grep("\\.y", names(localGrainObj))
  localGrainObj <- localGrainObj[,c((1:ncol(localGrainObj))[-col_idx])]
  
  # rename the .x columns to remove the .x (so they match the master list again)
  col_idx <- grep("\\.x", names(localGrainObj))
  names(localGrainObj) <- gsub("\\.x","",names(localGrainObj))
  
  # Order the columns alphabetically then bring rows names back to front 
  localGrainObj <- localGrainObj[,order(names(localGrainObj))]
  col_idx <- grep("Row", names(localGrainObj))
  localGrainObj <- localGrainObj[, c(col_idx, (1:ncol(localGrainObj))[-col_idx])]
  
  # turn all NAs into zero as well
  localGrainObj[is.na(localGrainObj)] <- 0

  # turn it back into a matrix
  localGrainObjTemp <- as.matrix(localGrainObj, rownames.force=TRUE)
  localGrainObjTemp2 <- apply(localGrainObjTemp[,2:ncol(localGrainObjTemp)], 1, as.numeric)
  localGrainObjTemp2[(localGrainObjTemp2) > 0 ] <- 1  # normalise to 1s and 0s
  dimnames(localGrainObjTemp2) <- list(as.vector(localGrainObj[,1]),as.vector(localGrainObj[,1]))
  localGrainObj <- localGrainObjTemp2
  
  # write out the full grain
  exportPath <- paste("D:/00_Dev/GrainTransformer/output/",grainName,".csv",sep="")
  write.csv(localGrainObj, exportPath)
  
  #return the matrix to the call
  return(localGrainObj)
  
}


# Create the full grain for 5TD
full_Grain_5TD <- createFullGrain(grain_5TD,"full_Grain_5TD",masterDataFrameTemplate)
  
# Create a list of all the grains for Data Source and AS-IS data frames 
listGrains_DataSource <- apropos("grainDataSource_", where = FALSE, mode = "any")
listGrains_AsIs <- apropos("grainAsIs_", where = FALSE, mode = "any")

# cycle through the Data Source list to produce the full grain matrices
for (grain_DataSource in listGrains_DataSource){
  
  myNameShort <- strsplit(grain_DataSource,"[[:punct:]]")[[1]][(length(strsplit(grain_DataSource,"[[:punct:]]")[[1]]))]
  assign(paste("full_GrainDataSource_", myNameShort,sep=""), createFullGrain(get(grain_DataSource), paste("full_GrainDataSource_", myNameShort,sep=""),masterDataFrameTemplate))
  
}

# cycle through the As-ISlist to produce the full grain matrices
for (grain_AsIs in listGrains_AsIs){
  
  myNameShort <- strsplit(grain_AsIs,"[[:punct:]]")[[1]][(length(strsplit(grain_AsIs,"[[:punct:]]")[[1]]))]
  assign(paste("full_GrainAsIs_", myNameShort,sep=""), createFullGrain(get(grain_AsIs), paste("full_GrainAsIs_", myNameShort,sep=""),masterDataFrameTemplate))
  
}

# clean up
# rm(edges_DataSource, edges_AsIs, grain_DataSource, grain_AsIs, listEdges_DataSource, listEdges_AsIs)
# rm(listGrains_DataSource, listGrains_AsIs, mmDims, myNameShort, sortaUniqueDimensions)
# rm(sortaUniqueMeasures,uniqueDimensions,uniqueDimsMsrs,uniqueMeasures,uniqueMsrsDims)
# rm(masterDataFrameTemplate, masterMatrixTemplate)

#####################################################################

### 5. Calculate Matrix Deltas and write to file

# Grab all the To-BE full grain matrices
listFullGrains_AsIs <- apropos("full_GrainAsIs_", where = FALSE, mode = "any")

# cycle through the AS IS full grains
for (fullGrain_AsIs in listFullGrains_AsIs){
  
  # grab the current source file name
  myNameShort <- strsplit(fullGrain_AsIs,"[[:punct:]]")[[1]][(length(strsplit(fullGrain_AsIs,"[[:punct:]]")[[1]]))]
  
  # Find each RegionalAsIs minus each Regional Data Source and write the result to file
  # >> THIS TELLS US WHAT THEY NEED TO DO TO MEET THEIR OWN REQS
  asIsMinusDataSource <- get(fullGrain_AsIs) - get(paste("full_GrainDataSource_",myNameShort,sep=""))  
  exportPath <- paste("D:/00_Dev/GrainTransformer/output/",paste("result_AsIsMinusDataSource_",myNameShort,sep=""),".csv",sep="")
  write.csv(asIsMinusDataSource, exportPath)
  
  # Find each RegionalAsIs minus 5TD
  # >> THIS TELLS US WHAT THEY THINK THEY NEED THAT GLOBAL DON'T, AND VICE VERSA
  asIsMinus5TD <- get(fullGrain_AsIs) - full_Grain_5TD 
  exportPath <- paste("D:/00_Dev/GrainTransformer/output/",paste("result_AsIsMinus5TD_",myNameShort,sep=""),".csv",sep="")
  write.csv(asIsMinus5TD, exportPath)
  
  # Find 5TD plus the As IS
  # >> THIS GIVES US THE TO-BE GRAIN MATRIX
  toBeMatrix <-get(fullGrain_AsIs) + full_Grain_5TD 
  exportPath <- paste("D:/00_Dev/GrainTransformer/output/",paste("result_ToBeMatrix_",myNameShort,sep=""),".csv",sep="")
  write.csv(toBeMatrix, exportPath)
  
  # Find the 5TD minus the data source
  # >> THIS GIVES US WHAT THEY STILL NEED TO MEET 5TD
  fiveTDMinusDataSource <- full_Grain_5TD - get(paste("full_GrainDataSource_",myNameShort,sep=""))  
  exportPath <- paste("D:/00_Dev/GrainTransformer/output/",paste("result_5TDMinusDataSource_",myNameShort,sep=""),".csv",sep="")
  write.csv(fiveTDMinusDataSource, exportPath)
  
  # Find To Be Minus the data source
  # >> THIS GIVES US WHAT THEY NEED TO MEET 5TD AND THEIR OWN REQS
  toBeMinusDataSource <- get(fullGrain_AsIs) + full_Grain_5TD - get(paste("full_GrainDataSource_",myNameShort,sep=""))
  exportPath <- paste("D:/00_Dev/GrainTransformer/output/",paste("result_ToBeMinusDataSource_",myNameShort,sep=""),".csv",sep="")
  write.csv(toBeMinusDataSource, exportPath)
  
  # clean up in loop
  rm(asIsMinusDataSource, asIsMinus5TD, toBeMatrix, fiveTDMinusDataSource, toBeMinusDataSource, exportPath)
  rm(myNameShort, listFullGrains_AsIs, fullGrain_AsIs)
  
}



#####################################################################

### 6. Create the group by dimensional reports

# Create the function
groupByDimension <- function(myEdgeList, myName) 
{

  # Create, distinct and merge the group bys
  myDataListLeft <- aggregate(myEdgeList[3], by=list(myEdgeList$Dimension), c)
  myDataListRight <- aggregate(myEdgeList[2], by=list(myEdgeList$Dimension), c)  
  myGroupList <- merge(myDataListLeft, myDataListRight, by="Group.1")
  
  # rename the columns
  names(myGroupList) <- c("Dimension", "ReportsAppearsIn", "MeasuresUsedBy")
  
  # remove the junk from aggregate
  myGroupList$ReportsAppearsIn <- gsub("c\\(","", myGroupList$ReportsAppearsIn)
  myGroupList$ReportsAppearsIn <- gsub("\\)","", myGroupList$ReportsAppearsIn)
  myGroupList$MeasuresUsedBy <- gsub("c\\(","", myGroupList$MeasuresUsedBy)
  myGroupList$MeasuresUsedBy <- gsub("\\)","", myGroupList$MeasuresUsedBy)
  myGroupList$MeasuresUsedBy <- gsub("\"","", myGroupList$MeasuresUsedBy)
  
  # unique-ify the entries
  for(i in 1:nrow(myGroupList)){
    
    listOfRecordIds <- strsplit(myGroupList[i,2],", ")
    listOfRecordIds <- paste(unique(as.data.frame(listOfRecordIds)),sep=", ")
    myGroupList[i,2] <- paste(listOfRecordIds, sep="")
    listOfMeasures <- strsplit(myGroupList[i,3],", ")
    listOfMeasures <- paste(unique(as.data.frame(listOfMeasures))[[1]], collapse=", ")
    myGroupList[i,3] <- paste(listOfMeasures, sep="")
    
  }
  
  # remove the junk from aggregate. again. must be a better way.
  myGroupList$ReportsAppearsIn <- gsub("c\\(","", myGroupList$ReportsAppearsIn)
  myGroupList$ReportsAppearsIn <- gsub("\\)","", myGroupList$ReportsAppearsIn)
  myGroupList$ReportsAppearsIn <- gsub(":",", ", myGroupList$ReportsAppearsIn)
  myGroupList$MeasuresUsedBy <- gsub("c\\(","", myGroupList$MeasuresUsedBy)
  myGroupList$MeasuresUsedBy <- gsub("\\)","", myGroupList$MeasuresUsedBy)
  myGroupList$MeasuresUsedBy <- gsub("\"","", myGroupList$MeasuresUsedBy)
  
  # Write the result to output
  write.table(myGroupList
              ,file=paste("D:/00_Dev/GrainTransformer/output/",myName,".csv",sep="")
              ,quote=TRUE
              ,sep=","
              ,row.names=FALSE
              ,col.names=TRUE)
  
  # exit function
  return(myGroupList)
  
}

# Create the function for Measures
groupByMeasure <- function(myEdgeList, myName) 
{
  
  # Create, distinct and merge the group bys
  myDataListLeft <- aggregate(myEdgeList[3], by=list(myEdgeList$Measure), c)
  myDataListRight <- aggregate(myEdgeList[1], by=list(myEdgeList$Measure), c)  
  
  myGroupList <- merge(myDataListLeft, myDataListRight, by="Group.1")
  
  # rename the columns
  names(myGroupList) <- c("Measure", "ReportsAppearsIn", "DimensionsUsedIn")
  
  # remove the junk from aggregate
  myGroupList$ReportsAppearsIn <- gsub("c\\(","", myGroupList$ReportsAppearsIn)
  myGroupList$ReportsAppearsIn <- gsub("\\)","", myGroupList$ReportsAppearsIn)
  myGroupList$DimensionsUsedIn <- gsub("c\\(","", myGroupList$DimensionsUsedIn)
  myGroupList$DimensionsUsedIn <- gsub("\\)","", myGroupList$DimensionsUsedIn)
  myGroupList$DimensionsUsedIn <- gsub("\"","", myGroupList$DimensionsUsedIn)
  
  
  
  # unique-ify the entries
  for(i in 1:nrow(myGroupList)){
    
    listOfRecordIds <- strsplit(myGroupList[i,2],", ")
    listOfRecordIds <- paste(unique(as.list(listOfRecordIds[[1]])),collapse=", ")
    myGroupList[i,2] <- paste(listOfRecordIds, sep="")
    listOfMeasures <- strsplit(myGroupList[i,3],", ")
    listOfMeasures <- paste(unique(as.data.frame(listOfMeasures))[[1]], collapse=", ")
    myGroupList[i,3] <- paste(listOfMeasures, sep="")
    
  }
  
  # remove the junk from aggregate. again. must be a better way.
  myGroupList$ReportsAppearsIn <- gsub("c\\(","", myGroupList$ReportsAppearsIn)
  myGroupList$ReportsAppearsIn <- gsub("\\)","", myGroupList$ReportsAppearsIn)
  myGroupList$ReportsAppearsIn <- gsub(":",", ", myGroupList$ReportsAppearsIn)
  myGroupList$ReportsAppearsIn <- gsub("\"", "", myGroupList$ReportsAppearsIn)
  myGroupList$DimensionsUsedIn <- gsub("c\\(","", myGroupList$DimensionsUsedIn)
  myGroupList$DimensionsUsedIn <- gsub("\\)","", myGroupList$DimensionsUsedIn)
  myGroupList$DimensionsUsedIn <- gsub("\"","", myGroupList$DimensionsUsedIn) 
  
  # Write the result to output
  write.table(myGroupList
              ,file=paste("D:/00_Dev/GrainTransformer/output/",myName,".csv",sep="")
              ,quote=TRUE
              ,sep=","
              ,row.names=FALSE
              ,col.names=TRUE)
  
  # exit function
  return(myGroupList)
  
}

# Now create the group list for 5TD
groupDims_5TD <- groupByDimension(edges_5TD,"groupDimsList_5TD")

# Create a list of all the edge lists for Data Source and AS IS data frames 
listEdges_DataSource <- apropos("edgesDataSource_", where = FALSE, mode = "any")
listEdges_AsIs <- apropos("edgesAsIs_", where = FALSE, mode = "any")

# cycle through the Data Source list to produce the groupDimLists
for (edges_DataSource in listEdges_DataSource){
  
  myNameShort <- strsplit(edges_DataSource,"[[:punct:]]")[[1]][(length(strsplit(edges_DataSource,"[[:punct:]]")[[1]]))]
  myGroupDimListName <- paste("groupDimsListDataSource_",myNameShort,sep="")
  assign(myGroupDimListName,groupByDimension(get(edges_DataSource), myGroupDimListName))
  
}

# cycle through the AS-IS list to produce the groupDimLists
for (edges_AsIs in listEdges_AsIs){
  
  myNameShort <- strsplit(edges_AsIs,"[[:punct:]]")[[1]][(length(strsplit(edges_AsIs,"[[:punct:]]")[[1]]))]
  myGroupDimListName <- paste("groupDimsListAsIs_",myNameShort,sep="")
  assign(myGroupDimListName,groupByDimension(get(edges_AsIs), myGroupDimListName))
  
}

# Now do the same the other way using:: groupByMeasure

# Now create the group list for 5TD (by MSR)
groupMsr_5TD <- groupByMeasure(edges_5TD,"groupMsrsList_5TD")

# cycle through the Data Source list to produce the groupMsrLists
for (edges_DataSource in listEdges_DataSource){
  
  myNameShort <- strsplit(edges_DataSource,"[[:punct:]]")[[1]][(length(strsplit(edges_DataSource,"[[:punct:]]")[[1]]))]
  myGroupMsrListName <- paste("groupMssrListDataSource_",myNameShort,sep="")
  assign(myGroupMsrListName,groupByMeasure(get(edges_DataSource), myGroupMsrListName))
  
}

# cycle through the AS-IS list to produce the groupMsrLists
for (edges_AsIs in listEdges_AsIs){
  
  myNameShort <- strsplit(edges_AsIs,"[[:punct:]]")[[1]][(length(strsplit(edges_AsIs,"[[:punct:]]")[[1]]))]
  myGroupMsrListName <- paste("groupMsrsListAsIs_",myNameShort,sep="")
  assign(myGroupMsrListName,groupByMeasure(get(edges_AsIs), myGroupMsrListName))
  
}

#####################################################################

### 7. Creating JSON output from the graph objects

# Grab the edge list from myGraph and dedup it
myEdgeList <- get.edgelist(grp_grainAsIs_Romania)
myEdgeList <- unique(myEdgeList)

# Create the node list and append group ids and node ids
myNodeList <- unique(myEdgeList[,1])
myNodeList <- c(myNodeList,unique(myEdgeList[,2]))
myNodeList <- unique(myNodeList)

# create cut down node list for export with headers name group
myNodeList <- as.data.frame(myNodeList)
myNodeList <- cbind(myNodeList, 1)
myNodeList[,1] <- as.character(myNodeList[,1])

# NODES - Create the sequence to use in generating the pairings
seqNodeList <- seq(from = 3, to = (length(myNodeList[,1])), by = 2)
myNodePairList <- list(list(myNodeList[1,1], myNodeList[1,2]),list(myNodeList[2,1], myNodeList[2,2]))
names(myNodePairList[[1]]) <- c("name","group")
names(myNodePairList[[2]]) <- c("name","group")

#  NODES - Create the named pairs list 
for(i in seqNodeList){
   
   myTempNodePairList <- list(list(myNodeList[i,1], myNodeList[i,2]),list(myNodeList[(i+1),1], myNodeList[(i+1),2]))
   names(myTempNodePairList[[1]]) <- c("name","group")
   names(myTempNodePairList[[2]]) <- c("name","group")
   myNodePairList <- c(myNodePairList,myTempNodePairList)
   
}

# NODES - export the node list as nodes
jExportNodes <- toJSON(myNodePairList)
myPath <- c("D:/00_Dev/GrainTransformer/output/nodes.json")
write(jExportNodes, myPath)

# Copy the edge list changing node names for node ids and add weight column
myEdgeListDF <- as.data.frame(match(myEdgeList[,1],myNodeList[[1]])-1)
myEdgeListDF <- cbind(myEdgeListDF, as.data.frame(match(myEdgeList[,2],myNodeList[[1]])-1))
myEdgeListDF <- cbind(myEdgeListDF,1)

# EDGES - Create the sequence to use in generating the pairings
seqEdgeList <- seq(from = 3, to = (length(myEdgeListDF[,1])), by = 2)
myEdgePairList <- list(list(myEdgeListDF[1,1], myEdgeListDF[1,2], myEdgeListDF[1,3]),
                       list(myEdgeListDF[2,1], myEdgeListDF[2,2], myEdgeListDF[2,3]))
names(myEdgePairList[[1]]) <- c("source","target", "value")
names(myEdgePairList[[2]]) <- c("source","target", "value")

#  EDGES - Create the named pairs list 
for(i in seqEdgeList){
   
   myTempEdgePairList <- list(list(myEdgeListDF[i,1], myEdgeListDF[i,2], myEdgeListDF[i,3]),
                              list(myEdgeListDF[(i+1),1], myEdgeListDF[(i+1),2], myEdgeListDF[(i+1),3]))
   names(myTempEdgePairList[[1]]) <- c("source","target", "value")
   names(myTempEdgePairList[[2]]) <- c("source","target", "value")
   myEdgePairList <- c(myEdgePairList,myTempEdgePairList)
   
}

# EDGES - export the node list as nodes
jExportLinks <- toJSON(myEdgePairList)
myPath <- c("D:/00_Dev/GrainTransformer/output/links.json")
write(jExportLinks, myPath)


#####################################################################

# jExport <- toJSON()
# myPath <- c("D:/00_Dev/GrainTransformer/output/myJSON.json")
# write(jExport, myPath)
# 
# 
# # grab the node list
# get.edgelist(myGraph)
# 
# 
# get.vertex.attribute
# get.edge.attribute
# 
# 
# get.edge.ids
# vertices
# 
# 
# write.graph

# 
# myEdgeList[,1] <- as.character()
# myEdges[,2] <- as.character(normalisationTableMeasures[match(myEdges[,2],normalisationTableMeasures[,1]),2]) 
# 
# match(myEdgeList[,2],myNodeList[[1]])
# 
# match(myEdgeList[,1],myNodeList[[1]])
# match(myEdgeList[,2],myNodeList[[1]])

##################################################################END