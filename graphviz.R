library("Rgraphviz")

set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, 0.2)
plot(g1)

rEG <- new("graphNEL", nodes=c("A", "B"), edgemode="directed")
rEG <- addEdge("A", "B", rEG, 1)
rEG <- addEdge("B", "A", rEG, 1)

plot(rEG)
plot(rEG, recipEdges="distinct")

removedEdges(g1)

sg1 <- subGraph(c("a","d","j","i"), g1)
sg1
sg2 <- subGraph(c("b","e","h"), g1)
sg3 <- subGraph(c("c","f","g"), g1)

subGList <- vector(mode="list", length=3)
subGList[[1]] <- list(graph=sg1)
subGList[[2]] <- list(graph=sg2, cluster=FALSE)
subGList[[3]] <- list(graph=sg3)
plot(g1, subGList=subGList)

sg1 <- subGraph(c("a","c","d","e","j"), g1)
sg2 <- subGraph(c("f","h","i"), g1)
plot(g1, subGList=list(list(graph=sg1), list(graph=sg2)))

edgeNames(g1)

defAttrs <- getDefaultAttrs()

plot(g1, attrs=list(node=list(label="foo", fillcolor="lightgreen"),
                    edge=list(color="cyan"),
                    graph=list(rankdir="LR")))


nAttrs <- list()
eAttrs <- list()

z <- strsplit(packageDescription("Rgraphviz")$Description, " ")[[1]]
z <- z[1:numNodes(g1)]
names(z) = nodes(g1)
nAttrs$label <- z
eAttrs$label <- c("a~h"="Label 1", "c~h"="Label 2")
attrs <- list(node=list(shape="ellipse", fixedsize=FALSE))
plot(g1, nodeAttrs=nAttrs, edgeAttrs=eAttrs, attrs=attrs)

ew <- as.character(unlist(edgeWeights(g1)))
ew <- ew[setdiff(seq(along=ew), removedEdges(g1))]
names(ew) <- edgeNames(g1)
eAttrs$label <- ew
attrs$edge$fontsize <- 27
plot(g1, nodeAttrs=nAttrs, edgeAttrs=eAttrs, attrs=attrs)


## Specify node drawing color
nAttrs$color <- c(a="red", b="red", g="green", d="blue")
## Specify edge drawing color
eAttrs$color <- c("a~d"="blue", "c~h"="purple")
## Specify node fill color
nAttrs$fillcolor <- c(j="yellow")
## label color
nAttrs$fontcolor <- c(e="green", f="red")
eAttrs$fontcolor <- c("a~h"="green", "a~b"="red")
nAttrs

attrs$node$shape <- "ellipse"
nAttrs$shape <- c(g="box", f="circle", j="box", a="plaintext")
plot(g1, attrs=attrs, nodeAttrs=nAttrs)

nodes <- buildNodeList(g1)
edges <- buildEdgeList(g1)

nodes[[1]]
edges[[1]]
edges[[1]]

for(j in c("a~e", "a~h"))
  edges[[j]]@attrs$arrowhead <- "open"

vv <- agopen(name="foo", nodes=nodes, edges=edges, attrs=attrs,edgeMode="undirected")
plot(vv)

data(graphExamples)
z <- graphExamples[[8]]
nNodes <- length(nodes(z))
nA <- list()
nA$fixedSize<-rep(FALSE, nNodes)
nA$height <- nA$width <- rep("1", nNodes)
nA$label <- rep("z", nNodes)
nA$color <- rep("green", nNodes)
nA$fillcolor <- rep("orange", nNodes)
nA$shape <- rep("circle", nNodes)
nA$fontcolor <- rep("blue", nNodes)
nA$fontsize <- rep(10, nNodes)
nA <- lapply(nA, function(x) { names(x) <- nodes(z); x})
plot(z, nodeAttrs=nA)

set.seed(123)
counts = matrix(rexp(numNodes(g1)*4), ncol=4)
g1layout <- agopen(g1, name="foo")
makeNodeDrawFunction <- function(x) {
  force(x)
  function(node, ur, attrs, radConv) {
    nc <- getNodeCenter(node)
    pieGlyph(x,
             xpos=getX(nc),
             ypos=getY(nc),
             radius=getNodeRW(node),
             col=rainbow(4))
    text(getX(nc), getY(nc), paste(signif(sum(x), 2)),
         cex=0.5, col="white", font=2)
  }
}

drawFuns <- apply(counts, 1, makeNodeDrawFunction)
plot(g1layout, drawNode=drawFuns, main="Example Pie Chart Plot")

cG <- new("clusterGraph", clusters=list(a=c(1:10), b=c(11:13),
                                        c=c(14:20), d=c(21, 22)))

set.seed(123)
nodes1 <- paste(0:7)
nodes2 <- letters[1:10]
ft <- cbind(sample(nodes1, 24, replace=TRUE),
            sample(nodes2, 24, replace=TRUE))
ft <- ft[!duplicated(apply(ft, 1, paste, collapse="")),]

g <- ftM2graphNEL(ft, edgemode='directed')
g

twocolors <- c("#D9EF8B", "#E0F3F8")
nodeType <- 1 + (nodes(g) %in% nodes1)
nA = makeNodeAttrs(g, fillcolor=twocolors[nodeType])
sg1 = subGraph(nodes1, g)
sgL = list(list(graph=sg1, cluster = FALSE, attrs = c(rank="sink")))
att = list(graph = list(rankdir = "LR", rank = ""))

plot(g, attrs = att, nodeAttrs=nA, subGList = sgL)

