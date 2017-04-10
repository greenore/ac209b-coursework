library("tidyr")
library("dplyr")
library("ggplot2")

data <- read.csv("data/tmdb_movie_info.txt", sep="\t", quote="", header=TRUE)
data$V13


total_genres<-c('Adventure','Fantasy','Animation','Drama','Horror','Action','Comedy','History','Western','Thriller','Crime','Documentary','Science Fiction','Mystery','Music','Romance','Family','War','TVMovie')
genres_id<-seq(2,20)
names(genres_id)<-total_genres

genres<-as.data.frame(setNames(replicate(20,numeric(0), simplify = F), letters[1:20]))

i <- 1
for (i in 1:dim(data)[1]){
  line<-rep(0,20)
  line[1]<-as.numeric(data[i,1])
  genres_line<-as.character(data[i, 13])
  if (is.na(genres_line)){
    next
  }
  
  genres_words<-strsplit(genres_line,",")[[1]]
  for (j in 1:length(genres_words)){
    w<-genres_words[j]
    if (w %in% total_genres){
      line[genres_id[[w]]]<-1
    }
  }
  genres<-rbind(genres,line)
}


colnames(genres)<-c('tmdb_id','Adventure','Fantasy','Animation','Drama','Horror','Action','Comedy','History','Western','Thriller','Crime','Documentary','Science Fiction','Mystery','Music','Romance','Family','War','TVMovie')

write.csv(genres,"G:/data/clean_data/genres_10k.txt",row.names = F,quote = F)
