notNaturalNumber <- function(x)
{
	if (class(x) != "numeric") 
        return(TRUE);
		
    if (ceiling(x) != floor(x)) 
        return(TRUE);
		
	if (x<=0) 
        return(TRUE);
		
	return(FALSE);
}

FLKMeans <- function(TableName, ObsIDColName, VarIDColName, ValueColName, WhereClause, Clusters, Iterations, Hypothesis, Note)
{
	#check TableName
    if (class(TableName) != "character") 
        return("First parameter is the table name and it should be a string");
	
	#check ObsIDColName
    if (class(ObsIDColName) != "character") 
        return("Second parameter is the table name and it should be a string");
		
	#check VarIDColName
    if (class(VarIDColName) != "character") 
        return("Third parameter is the table name and it should be a string");
		
	#check ValueColName
    if (class(ValueColName) != "character") 
        return("Fourth parameter is the table name and it should be a string");
		
	#check WhereClause
    if (class(WhereClause) != "character") 
        return("Fifth parameter is an Additional filter for restricting input data and it should be a string");
		
    #check notes
    if (class(Note) != "character") 
        return("Ninth parameter is the user defined comment and it should be a string");
	
	#check Clusters 
    if (notNaturalNumber(Clusters)) 
        return("Seventh parameter is Number of clusters and it should be an integer greater than zero");
		
	#check Iterations 
    if (notNaturalNumber(Iterations)) 
        return("Seventh parameter is maximum iterations and it should be an integer greater than zero");
		
	#check Hypothesis 
    if (notNaturalNumber(Hypothesis)) 
        return("Eightth parameter is Number of Hypotheses and it should be an integer greater than zero");
		
	#Generate SQL
	SQLStr <- paste("CALL FLKMeans('", TableName, "',", "'", ObsIDColName, "',", "'", VarIDColName, "',", "'", ValueColName, "','", WhereClause, "','", toString(Clusters), "','", toString(Iterations),"','", toString(Hypothesis),"',", "'", Note, "',",  "AnalysisID)", sep="")
	#run KMeans
    KMeansRes <- sqlQuery(DBConnection, SQLStr);
	
	#Getting AnalysisID, problems with concurrent acceses ? 
	AnalysisID <- sqlQuery(DBConnection, "SELECT TOP 1 AnalysisID FROM fzzlKMeansInfo ORDER BY RunStartTime DESC");
	AnalysisID <- toString(AnalysisID$ANALYSISID);
	
	SQLStr <- paste("SELECT HypothesisID,Level,ClusterID,VarID,Centroid FROM fzzlKMeansDendrogram WHERE AnalysisID = '", AnalysisID,"' ORDER BY 1,2,3,4,5",sep = "");
	KMeansDendrogram <- sqlQuery(DBConnection, SQLStr);
	
	SQLStr <- paste("SELECT HypothesisID,ObsID,ClusterID FROM fzzlKMeansClusterID WHERE AnalysisID = '", AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
	KMeansClusterID <- sqlQuery(DBConnection, SQLStr);
	
	RetData = list(AnalysisID = AnalysisID, Dendrogram = KMeansDendrogram, ClusterID = KMeansClusterID);
	return(RetData);
}