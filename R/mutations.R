if (!(require("stringr"))){
  install.packages("stringr")
  library("stringr")
}
if (!(require("tidyverse"))){
  install.packages("tidyverse")
  library("tidyverse")
}
if (!(require("readxl"))){
  install.packages("readxl")
  library("readxl")
}
if (!(require("lubridate"))){
  install.packages("lubridate")
  library("lubridate")
}
if (!(require("ggalt"))){
  install.packages("ggalt")
  library("ggalt")
}
if (!(require("wesanderson"))){
  install.packages("wesanderson")
  library("wesanderson")
}

source("functions.plotting.R")

mutations <- read.csv("../data/metadata.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

new.files<-data.frame(
  Virus.name=NA,
  mutation=NA,
  location=NA,
  mutation.Ref=NA,
  mutation.Seq=NA
)
count <- 1
for(i in 1:nrow(mutations)){
  getMut <- str_split(mutations$NT.mutations[i], ", ")
  for(j in 1:length(getMut[[1]])){
    aux <- grep(">",  getMut[[1]][j])
    if (!(identical(aux, integer(0)))){
      new.files[count, ] <- NA
      new.files$Virus.name[count] <- mutations$name[i]
      letter <- str_split(gsub("[0-9]", "", getMut[[1]][j]), ">")
      new.files$mutation[count] <- paste0(letter[[1]][1], gsub("[^0-9]", "", getMut[[1]][j]), letter[[1]][2])
      new.files$location[count] <- gsub("[^0-9]", "", getMut[[1]][j])
      new.files$mutation.Ref[count] <- letter[[1]][1]
      new.files$mutation.Seq[count] <- letter[[1]][2]
      count <- count + 1
    }
  }
}

codeMutationTags<- function(count, thre){
  over<- names(count[which(as.numeric(count)>=(thre*.9))])
  over<- strsplit(over, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)
  over<- matrix(unlist(over),ncol=3,byrow=T)
  positions<- over[,2]
  over<- cbind(over[,2], ": ", over[,1], " to ", over[,3])
  over<- apply(over, MARG=1, FUN=function(X) { paste(X, collapse='') })
  over<- data.frame(x_pos=as.numeric(positions), y_pos=thre/2, tag=as.character(over), stringsAsFactors=FALSE)
  return(over)
}

Mutation_tags <- codeMutationTags(table(new.files$mutation), thre=length(table(new.files$Virus.name)))

Mutation_table <-table(new.files$location)
Mutation_table <- data.frame(count=as.numeric(Mutation_table),location=as.numeric(names(Mutation_table)))
nx<- 400
ss<- 2.4
panel <- ggplot(Mutation_table, aes(x = location, y=count)) +
  geom_bar(fill="black", col="black",stat="identity") +
  xlab("genome site")+ ylab("number of genomes")+ ggtitle("P.1 mutational map") +
  scale_x_continuous(limits = c(0,29903),breaks = seq(0, 29903, by = 5000))+
  geom_hline(yintercept=length(table(new.files$Virus.name)), linetype="dashed", color = "grey33") +
  ##sadly, this is the easier way
  annotate("text", fontface =2,x=Mutation_tags$x_pos[1]-nx, y=Mutation_tags$y_pos[1], label=Mutation_tags$tag[1], angle=90, size=ss,col='tomato3')+
  annotate("text", fontface =2,x=Mutation_tags$x_pos[2]-nx, y=Mutation_tags$y_pos[2], label=Mutation_tags$tag[2], angle=90, size=ss,col='tomato3')+
  annotate("text", fontface =2,x=Mutation_tags$x_pos[3]+nx, y=Mutation_tags$y_pos[3], label=Mutation_tags$tag[3], angle=90, size=ss,col='tomato3')+
  annotate("text", fontface =2,x=Mutation_tags$x_pos[4]-nx, y=Mutation_tags$y_pos[4], label=Mutation_tags$tag[4], angle=90, size=ss,col='tomato3')

panel

ggsave("../result/PlotMutation.pdf", width = 10, height = 4)
