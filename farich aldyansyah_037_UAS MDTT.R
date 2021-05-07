library(rvest)
library(stringr)
library(dplyr)
library(tidytext)
library(wordcloud2)
library(tibble)
library(gapminder)

url <-paste0("https://en.wikipedia.org/w/index.php?title=","Gun_violence_in_the_United_States_by_state","&direction=prev&oldid=810166167")
df_murder <-read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  setNames(c("Negara_Bagian", "Populasi", "Total", "Tingkat_Pembunuhan"))
head(df_murder)

#1A
# build a visualitazion
library(ggplot2)
gambar = ggplot(df_murder, aes(x=Populasi, y=Total, fill=Negara_Bagian)) #make a plot
gambar = gambar + geom_bar(width=1, stat="identity") #make a histogram
print(gambar)
gambar = gambar + ggtitle("jumlah populasi per negara") #make title
gambar = gambar + xlab("nama bagian negara") #make word in sb.x
gambar = gambar +ylab("jumlah populasi")
print(gambar)

#1B
ggplot(df_murder, aes(x = Tingkat_Pembunuhan, y = Populasi))+geom_point()

#1C 
library(pracma) #package untuk ukuran pemusatan data
median(df_murder$Negara_Bagian)
min(df_murder$Negara_Bagian)
max(df_murder$Negara_Bagian)

#1d
sum(str_detect(df_murder$Total, pattern =","))

gapminder <-as.data.frame(gapminder)
df_gap <-data.frame(country = unique(gapminder$country))
df_gap$continent <-gapminder[match(df_gap$country, gapminder$country), "continent"]
head(df_gap)
#2a
summary(length(df_gap$country))
#2b
hurufdepan <- substr(gapminder$country,1,1)
table(hurufdepan)

#2c
grep(pattern = "[and]", x="country")

#2d
library(bpa)
df_gap[grep(pattern="[^AaW.,]",x=basic_pattern_analysis(df_gap$country)),]


teks_gam <-"At length I returned from two weeks leave of absence to find that my patrons had arrived three days ago in Roulettenberg. I received fromthem a welcome quite different to that which I had expected. The General eyed me coldly, greeted me in rather haughty fashion, and dismissed me to pay my respects to his sister. It was clear that from SOMEWHERE money had been acquired. I thought I could even detect a certain shamefacedness in the General's glance. Maria Philipovna, too, seemed distraught, and conversed with me with an air of detachment. Nevertheless, she took the money which I handed to her, counted it, and listened to what I had to tell. To luncheon there were expected that day a Monsieur Mezentsov, a French lady, and an Englishman; for, whenever money was in hand, a banquet in Muscovite style was always given. Polina Alexandrovna, on seeing me, inquired why I had been solong away. Then, without waiting for an answer, she departed. Evidently this was not mere accident, and I felt that I must throw some light upon matters. It was high time that I did so. I was assigned a small room on the fourth floor of the hotel (for you must know that I belonged to the General's suite). So far as I could see, the party had already gained some notoriety in the place, which had come to look upon the General as a Russian noblemanof great wealth. Indeed, even before luncheon he charged me, among other things, to get two thousand-franc notes changed for himat the hotel counter, which put us in a position to be thought millionaires at all events for aweek! Later, I was about to take Mischa and Nadia for a walk when a summons reached me from the staircase that I must attend the General. He began bydeigning to inquire of me where I was going to take the children; 
and as he did so, I could see that he failed to look me in the eyes. He WANTED to do so, but each time was met by me with such a fixed, disrespectful stare that he desisted in confusion. In pompous language, however, which jumbled one sentence into another, and at length grew disconnected, he gave me to understand that I was to lead the children altogether away from the Casino, and out into the park. Finally his anger exploded, and he added sharply: \"I suppose you would like to take them to the Casino to play roulette? Well, excuse my speaking so plainly, but I know how addicted you are to gambling. Though I am not your mentor, nor wish to be, at least I have a right to require that you shall not actually compromise me.\" \"I have no money for gambling,\" I quietly replied."
#3a
library(tm)
#let us create the corpus
docs <- Corpus(VectorSource(teks_gam))
#clean our chat data
trans <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
teks_gam_clean <- tm_map(docs, trans, "/")
teks_gam_clean <- tm_map(docs, trans, "@")
teks_gam_clean <- tm_map(docs, trans, "\\|")
teks_gam_clean <- tm_map(docs, content_transformer(tolower))
teks_gam_clean <- tm_map(docs, removeNumbers)
teks_gam_clean <- tm_map(docs, removePunctuation)
print(teks_gam_clean)

#3b
teks_split <-unlist(str_split(teks_gam_clean, " "))
teks_tibble <-tibble(teks_split)
#3c
teks_gam_clean <- tm_map(teks_split, removeWords, stopwords("english"))
#create the document term matrix
dtm <- TermDocumentMatrix(teks_gam_clean)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat),decreasing=TRUE)

#3d
#Data frame
data <- data.frame(word = names(v),freq=v)


#generate the wordcloud
library(wordcloud)
set.seed(1056)
wordcloud(words = data$word, freq = data$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


#4 crawling twitter