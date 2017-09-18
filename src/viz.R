# --------------------------------------------------------------------------- #
# visualization
# --------------------------------------------------------------------------- #

# input
data = readRDS("data/eulaw.rds")
treaty = 1 # ecsc

# data prep ----------------------------------------------------------------- #

# drop txt variables
data = data[, -grep("^txt", names(data))]

# only selected treaty
data = data[substring(data$id_1951, 1, 1) == paste(treaty) |
            substring(data$id_1957, 1, 1) == paste(treaty) |  
            substring(data$id_1965, 1, 1) == paste(treaty) |
            substring(data$id_1986, 1, 1) == paste(treaty) |
            substring(data$id_1992, 1, 1) == paste(treaty), ]

data = data[rowSums(is.na(data)) != ncol(data),]

# get articles numbers from ids
data$id_1951 = viz_make_articles(data$id_1951) 
data$id_1957 = viz_make_articles(data$id_1957) 
data$id_1965 = viz_make_articles(data$id_1965) 
data$id_1986 = viz_make_articles(data$id_1986) 
data$id_1992 = viz_make_articles(data$id_1992) 

# make nodes and links ------------------------------------------------------ #

# make dfs and nodes
df = viz_make_dfs(data$id_1951, 0)
nodes = df
df = viz_make_dfs(data$id_1957, 1)
nodes = rbind(nodes, df)
df = viz_make_dfs(data$id_1965, 2)
nodes = rbind(nodes, df)
df = viz_make_dfs(data$id_1986, 3)
nodes = rbind(nodes, df)
df = viz_make_dfs(data$id_1992, 4)
nodes = rbind(nodes, df)

# make indices
index = nodes
index$index =  seq_len(nrow(index)) - 1

data = data.frame(data)

# merge index (source and target)
data = viz_merge_index(data, index, 0, "id_1951", "index_1951")
data = viz_merge_index(data, index, 1, "id_1957", "index_1957")
data = viz_merge_index(data, index, 2, "id_1965", "index_1965")
data = viz_merge_index(data, index, 3, "id_1986", "index_1986")
data = viz_merge_index(data, index, 4, "id_1992", "index_1992")

# drop ids
data = data[, -grep("^id", names(data))]

# make links
df = data[, 1:2]
df = df[complete.cases(df),]
names(df) = c("source", "target")
df = df[order(df$source, df$target), ]

links = df

df = data[, 2:3]
df = df[complete.cases(df),]
names(df) = c("source", "target")
df = df[order(df$source, df$target), ]

links = rbind(links, df)

df = data[, 3:4]
df = df[complete.cases(df),]
names(df) = c("source", "target")
df = df[order(df$source, df$target), ]

links = rbind(links, df)

df = data[, 4:5]
df = df[complete.cases(df),]
names(df) = c("source", "target")
df = df[order(df$source, df$target), ]

links = rbind(links, df)

# groups, value
nodes$group = treaty
links$group = treaty
links$value = 1

# sankey graph -------------------------------------------------------------- #

sankeyNetwork(Links = links, 
              Nodes = nodes, 
              NodeGroup = "group",
              LinkGroup = "group",
              Source = "source", 
              Target = "target",
              Value = "value", 
              NodeID = "art",
              NodePosX = "pos")

# --------------------------------------------------------------------------- #
# viz help functions
# --------------------------------------------------------------------------- #

# make article number from id
viz_make_articles = function(var) {
  var = unlist(
      lapply(
        str_split(var, pattern = "\\."), 
      last)
  )
  return(var)
}

# make dfs for nodes
viz_make_dfs = function(var, num) {
  df = data.frame(
    mixedsort(
      var[is.na(var) == F]
    )
  )
  names(df) = "art"
  df$pos = num
  
  return(df)
}

# merge index for link data
viz_merge_index = function(data, index_data, pos_num, id_num, index_num) {
  
  df = index[index$pos == pos_num, ]
  names(df)[names(df) == "art"] = id_num
  names(df)[names(df) == "index"] = index_num
  
  data = merge(data, df, by = id_num, all = TRUE)
  
  data = data[names(data) != "pos"]
  
  return(data)
  
}



# sankey visualization based on https://github.com/fbreitwieser/sankeyD3


#' Create a D3 JavaScript Sankey diagram
#'
#' @param Links a data frame object with the links between the nodes. It should
#' have include the \code{Source} and \code{Target} for each link. An optional
#' \code{Value} variable can be included to specify how close the nodes are to
#' one another.
#' @param Nodes a data frame containing the node id and properties of the nodes.
#' If no ID is specified then the nodes must be in the same order as the
#' \code{Source} variable column in the \code{Links} data frame. Currently only
#' grouping variable is allowed.
#' @param Source character string naming the network source variable in the
#' \code{Links} data frame.
#' @param Target character string naming the network target variable in the
#' \code{Links} data frame.
#' @param Value character string naming the variable in the \code{Links} data
#' frame for how far away the nodes are from one another.
#' @param NodeID character string specifying the node IDs in the \code{Nodes}.
#' data frame. Must be 0-indexed.
#' @param NodeGroup character string specifying the node groups in the
#' \code{Nodes}. Used to color the nodes in the network.
#' @param LinkGroup character string specifying the groups in the
#' \code{Links}. Used to color the links in the network.
#' @param NodePosX character specifying a column in the \code{Nodes} data
#' frame that specifies the 0-based ordering of the nodes along the x-axis.
#' @param NodeValue character specifying a column in the \code{Nodes} data
#' frame with the value/size of each node. If \code{NULL}, the value is 
#' calculated based on the maximum of the sum of incoming and outoging 
#' links
#' @param colourScale character string specifying the categorical colour
#' scale for the nodes. See
#' \url{https://github.com/mbostock/d3/wiki/Ordinal-Scales}.
#' @param fontSize numeric font size in pixels for the node text labels.
#' @param fontFamily font family for the node text labels.
#' @param nodeWidth numeric width of each node.
#' @param nodePadding numeric essentially influences the width height.
#' @param nodeStrokeWidth numeric width of the stroke around nodes.
#' @param nodeCornerRadius numberic Radius for rounded nodes.
#' @param numberFormat number format in toolstips - see https://github.com/d3/d3-format for options.
#' @param margin an integer or a named \code{list}/\code{vector} of integers
#' for the plot margins. If using a named \code{list}/\code{vector},
#' the positions \code{top}, \code{right}, \code{bottom}, \code{left}
#' are valid.  If a single integer is provided, then the value will be
#' assigned to the right margin. Set the margin appropriately
#' to accomodate long text labels.
#' @param height numeric height for the network graph's frame area in pixels.
#' @param width numeric width for the network graph's frame area in pixels.
#' @param title character Title of plot, put in the upper-left corner of the Sankey 
#' @param iterations numeric. Number of iterations in the diagramm layout for 
#' computation of the depth (y-position) of each node. Note: this runs in the 
#' browser on the client so don't push it too high.
#' @param align character Alignment of the nodes. One of 'right', 'left', 'justify', 'center', 'none'.
#' If 'none', then the labels of the nodes are always to the right of the node.
#' @param zoom logical value to enable (\code{TRUE}) or disable (\code{FALSE})
#' zooming
#' @param nodeLabelMargin numeric margin between node and label.
#' @param linkOpacity numeric Opacity of links.
#' @param linkGradient boolean Add a gradient to the links?
#' @param xScalingFactor numeric Scale the computed x position of the nodes by this value.
#' @param xAxisDomain character[] If xAxisDomain is given, an axis with those value is 
#' added to the bottom of the plot. Only sensible when also NodeXPos are given.
#' @param linkType character One of 'bezier', 'l-bezier', 'trapezoid', 'path1' and 'path2'.
#' @param highlightChildLinks boolean Highlight all the links going right from a node or 
#' link.
#' @param curvature numeric Curvature parameter for bezier links - between 0 and 1.
#' @param showNodeValues boolean Show values above nodes. Might require and increased node margin.
#' @param scaleNodeBreadthsByString Put nodes at positions relatively to string lengths - 
#' only work well currently with align='none'

sankeyNetwork <- function(Links, Nodes, Source, Target, Value, 
                          NodeID, NodeGroup = NodeID, LinkGroup = NULL, NodePosX, NodeValue = NULL,
                          fontSize = 7,  fontFamily = NULL, 
                          nodeWidth = 25, nodePadding = 5, nodeStrokeWidth = 1, nodeCornerRadius = 0,
                          margin = NULL, title = NULL,
                          numberFormat = ",.5g", highlightChildLinks  = TRUE,
                          xAxisDomain = NULL,
                          height = NULL, width = NULL, iterations = 32, zoom = FALSE, align = "left",
                          linkType = "bezier", curvature = .5,
                          nodeLabelMargin = 2, linkOpacity = .5, linkGradient = FALSE,
                          xScalingFactor = 1) 
{
  
  # colourScale # "d3.scaleOrdinal().range(d3.schemeCategory10)"
  colourScale <- 'd3.scaleOrdinal().range(["#6495ED"])'
  
  LinksDF <- data.frame(source=Links[, Source], target = Links[, Target])
  
  LinksDF$value <- Links[, Value]
  
  
  NodesDF <- data.frame(name=Nodes[, NodeID], stringsAsFactors = FALSE)
  
  
  # add node group if specified
  if (is.character(NodeGroup)) {
    NodesDF$group <- Nodes[, NodeGroup]
  }
  
  if (is.character(NodePosX)) {
    NodesDF$posX <- Nodes[, NodePosX]
  }
  
  if (is.character(NodeValue)) {
    NodesDF$value <- Nodes[, NodeValue]
  }
  
  if (is.character(LinkGroup)) {
    LinksDF$group <- Links[, LinkGroup]
  }
  
  margin <- margin_handler(margin)
  
  # create options
  options = list(NodeID = NodeID, NodeGroup = NodeGroup, LinkGroup = LinkGroup, 
                 colourScale = colourScale, fontSize = fontSize, fontFamily = fontFamily, 
                 nodeWidth = nodeWidth, nodePadding = nodePadding, nodeStrokeWidth = nodeStrokeWidth,
                 nodeCornerRadius = nodeCornerRadius, 
                 numberFormat = numberFormat,
                 margin = margin, iterations = iterations, 
                 zoom = zoom, linkType = linkType, curvature = curvature,
                 highlightChildLinks = highlightChildLinks, 
                 align = align, xAxisDomain = xAxisDomain,
                 title = title, nodeLabelMargin = nodeLabelMargin, 
                 linkOpacity = linkOpacity, linkGradient = linkGradient, 
                 xScalingFactor = xScalingFactor)
  
  # create widget
  htmlwidgets::createWidget(name = "sankeyNetwork", x = list(links = LinksDF, 
                                                             nodes = NodesDF, options = options), width = width, height = height, 
                            htmlwidgets::sizingPolicy(padding = 10, browser.fill = TRUE), 
                            package = "sankeyD3")
}

# output (https://github.com/fbreitwieser/sankeyD3)
sankeyNetworkOutput <- function(outputId, width = "100%", height = "500px") {
  htmlwidgets::shinyWidgetOutput(outputId, "sankeyNetwork", width, height, package = "sankeyD3")
}

# render network (https://github.com/fbreitwieser/sankeyD3)
renderSankeyNetwork <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) 
  {
    expr <- substitute(expr)
  }  
  htmlwidgets::shinyRenderWidget(expr, sankeyNetworkOutput, env, quoted = TRUE)
}

# handle margins (https://github.com/fbreitwieser/sankeyD3)
margin_handler <- function(margin){
  if(!is.null(margin) && length(margin) == 1 && is.null(names(margin))){
    margin <- list(
      top = NULL,
      right = margin,
      bottom = NULL,
      left = NULL
    )
  } else if(!is.null(margin)){
    if(!is.list(margin) && !is.null(names(margin))){
      margin <- as.list(margin)
    }
    margin <- utils::modifyList(
      list(top = NULL, right = NULL, bottom = NULL, left = NULL),
      margin
    )
  } else {
    margin <- list(top = NULL, right = NULL, bottom = NULL, left = NULL)
  }
}
