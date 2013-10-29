# Functions defined for transforming data
# =======================================

# tJSONarray converts dataframe into D3.js JSON objects
# Found at: http://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis/
toJSONarray = function(dtf){
  clnms <- colnames(dtf)
  
  name.value <- function(i){
    quote <- '';
    if(class(dtf[, i])!='numeric'){
      quote <- '"';
    }
    
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }
  
  objs <- apply(sapply(clnms, name.value), 1, function(x){paste(x, collapse=', ')})
  objs <- paste('{', objs, '}')
  
  res <- paste('[', paste(objs, collapse=', '), ']')
  
  return(res)
}

# Trim whitespaces
trim <- function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x)

# Load data from OpenSpending API (ukgov-finances-cra dataset used)
# =================================================================

library("rjson")

# COFOG1 breakdown data
a <- "http://openspending.org/api/2/aggregate?dataset=psoen-income&cut=time.year:2013&drilldown=source|from"
a_data <- fromJSON(file=a, method="C")

cofog1 <- data.frame(cbind(
  matrix(sapply(sapply(a_data$drilldown, "[", c(1)), "[", c(5))),
  matrix(sapply(sapply(a_data$drilldown, "[", c(3)), "[", c(5))),
  matrix(sapply(a_data$drilldown, "[", c(2)))
))
names(cofog1) <- c("source", "target", "value")

# COFOG2 breakdown data
b <- "http://openspending.org/api/2/aggregate?dataset=psoen-income&cut=time.year:2013&drilldown=from|target"
b_data <- fromJSON(file=b, method="C")
cofog2 <- data.frame(cbind(
  matrix(sapply(sapply(b_data$drilldown, "[", c(2)), "[", c(2))),
  "PSOE Navarra",
  matrix(sapply(b_data$drilldown, "[", c(1)))
))
names(cofog2) <- c("source", "target", "value")

# COFOG3 breakdown data
c <- "http://openspending.org/api/2/aggregate?dataset=psoen-spending&cut=time.year:2013&drilldown=source|from"
c_data <- fromJSON(file=c, method="C")
cofog3 <- data.frame(cbind(
  "PSOE Navarra",
  matrix(sapply(sapply(c_data$drilldown, "[", c(3)), "[", c(2))), 
  matrix(sapply(c_data$drilldown, "[", c(2)))
))
names(cofog3) <- c("source", "target", "value")

# COFOG4 breakdown data
d <- "http://openspending.org/api/2/aggregate?dataset=psoen-spending&cut=time.year:2013&drilldown=from|to"
d_data <- fromJSON(file=d, method="C")
cofog4 <- data.frame(cbind(
  matrix(sapply(sapply(d_data$drilldown, "[", c(3)), "[", c(2))),
  matrix(sapply(sapply(d_data$drilldown, "[", c(1)), "[", c(2))),
  matrix(sapply(d_data$drilldown, "[", c(2)))
))
names(cofog4) <- c("source", "target", "value")

# Producing nodes and links D3.js JSON data
# ===========================================
  
# Initialization of D3.js nodes dataframe
nodes <- union(cofog1$source,union(cofog1$target,union(cofog2$target, union(cofog3$target, cofog4$target))))
#nodes <- union(cofog1$source,union(cofog1$target,union(cofog2$target, cofog3$target)))
nodes <- data.frame(cbind(seq(1:length(nodes)), nodes))
names(nodes) <- c("cod", "name")
nodes$name <- trim(nodes$name)

# Initialization of D3.js links dataframe
links <- rbind(cofog1, cofog2, cofog3, cofog4)
#links <- rbind(cofog1, cofog2, cofog3)
links$source <- trim(links$source)
links$target <- trim(links$target)

# Here comes the magic: merging datasets for replacing names with codes
links <- merge(links, nodes, by.x="source", by.y="name")
links <- merge(links, nodes, by.x="target", by.y="name")
links <- links[,c("cod.x", "cod.y", "value")]
names(links) <- c("source","target","value")
links$source <- as.numeric(links$source)-1
links$target <- as.numeric(links$target)-1
links$value <- as.numeric(links$value)
links$value[links$value == 0]=1

nodes <- data.frame(nodes[,c("name")])
names(nodes) <- c("name")

# Output to JSON D3.js compatible format
output <- paste('{"nodes":', toJSONarray(nodes), ',')
output <- paste(output, '"links":', toJSONarray(links), '}')
write(output, "psoe-navarra-sankey.json")