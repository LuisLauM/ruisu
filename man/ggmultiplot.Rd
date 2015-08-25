\name{ggmultiplot}
\alias{ggmultiplot}
\docType{data}
\title{
Create array of ggplot objects.
}
\description{
Similar function of \code{par(mfrow = ...)} or \code{layout} for ggplot objects.
}
\usage{ggmultiplot(list(...))}
\arguments{
  \item{...}{ggplot objects can be passed in ..., or}
  \item{plotlist}{as a list of ggplot objects}
  \item{cols}{Number of columns in layout}
  \item{layout}{A matrix specifying the layout. If present, 'cols' is ignored.}
}
\format{
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Script extracted from: 
http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
# This example uses the ChickWeight dataset, which comes with ggplot2
# First plot
p1 <- 
    ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
    geom_line() +
    ggtitle("Growth curve for individual chicks")

# Second plot
p2 <- 
    ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
    geom_point(alpha=.3) +
    geom_smooth(alpha=.2, size=1) +
    ggtitle("Fitted growth curve per diet")

# Third plot
p3 <- 
    ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
    geom_density() +
    ggtitle("Final weight, by diet")

# Fourth plot
p4 <- 
    ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
    geom_histogram(colour="black", binwidth=50) +
    facet_grid(Diet ~ .) +
    ggtitle("Final weight, by diet") +
    theme(legend.position="none")        # No legend (redundant in this graph)
    
Once the plot objects are set up, we can render them with multiplot. This will make two columns of graphs:

multiplot(p1, p2, p3, p4, cols=2)
}
\keyword{datasets}
