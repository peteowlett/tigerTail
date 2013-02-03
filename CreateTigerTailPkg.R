

### Libraries
require(igraph)         # For generating adjacency matrices
require(Matrix)         # Req for igraph
require(lattice)        # Req for igraph



makeEdgeList <- function(mySourceData, myName){
	
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


makeGrainMatrix <- function(edgeList, myNameShort){
	
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


# Set up the full grain function
# NOTE - grainMatrix must be a numeric matrix and masterTemplate must be a data
# frame grainName is just a string for naming the returned object and file write.
# NOTE - the full grains have been normalise so all values are zero or 1
makeExpandedGrainMatrix <- function(grainMatrix, grainName, masterTemplate){
	
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




# Create the function
makeDimensionsListing <- function(myEdgeList, myName) 
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
makeMeasuresListing <- function(myEdgeList, myName) 
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

package.skeleton(list=c("makeEdgeList","makeGrainMatrix","makeExpandedGrainMatrix","makeDimensionsListing","makeMeasuresListing"), name="tigerTail", force = TRUE,
			path="/home/analyticsdev/Development/RPackages")
