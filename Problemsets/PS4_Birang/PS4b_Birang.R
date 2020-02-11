df <- createDataFrame(iris)
df1 <- iris

class(df)
class(df1)

head(select(df, df$Sepal_Length, df$Species)) 
head(filter(df, df$Sepal_Length>5.5))
head(select(df, df$Sepal_Length, df$Species), filter(df, df$Sepal_Length>5.5)))
head(summarize(groupBy(df, df$Species),mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))

