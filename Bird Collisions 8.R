library(tidyverse)
library(data.table)
set.seed(12)

bird_collisions <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
mp_light <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")

sbird <- bird_collisions[bird_collisions$locality == 'MP']

vizwk8 <- na.omit(left_join(mp_light, sbird, by = 'date'))

vizwk8 <- as.data.table(dt)

wk8v<- select(vizwk8, species, flight_call, light_score)

collisions<- wk8v %>%
  count(species, sort = TRUE)
collisions

wk8v1 <- na.omit(left_join(wk8v, collisions, by = 'species'))
wk8v1

wk8v2<-wk8v1[!duplicated(wk8v1$species), ]
wk8v2

wk8v3 <- wk8v2[order(-wk8v2$n),]
wk8v3

setnames(wk8v3, "n", "collisions")
wk8v3

wk8v3<-wk8v3 %>% mutate(flightcall = ifelse( flight_call == "No", "yes", "no" ))
wk8v3

wk8v3<-wk8v3 %>% mutate(flightcall2 = ifelse( flight_call == "Yes", "yes", "no" ))
wk8v3

wk8v3$Log_transformed_collisions <- log(wk8v3$collisions)
wk8v3

wk8v3$species <- factor(wk8v3$species, 
                      levels = wk8v3$species[order(wk8v3$collisions)])

AvgLS<- wk8v1 %>% 
  group_by(species) %>% 
  summarise(average = mean(light_score))

wk8v8 <- na.omit(left_join(wk8v3, AvgLS, by = 'species'))
wk8v8

setnames(wk8v8, "average", "AvgLight")
wk8v8

wk8v4 <- ggplot(wk8v3, aes(x = species, y = collisions, fill = flightcall))+
  geom_col(width = 0.7)+
  geom_bar(stat = "identity") +
  scale_fill_manual( values = c( "yes"="red", "no"="gray" ), guide = FALSE )
wk8v5<- wk8v4 + coord_flip()
print(wk8v5 + ggtitle("Migrant Birds that dont use Nocturnal Flight Calls Have Less Collisions in Chicago and Cleveland"))

wk8v9 <- ggplot(wk8v8, aes(x = AvgLight, y = Log_transformed_collisions, col = flightcall2))+
  geom_point(size = 2, shape = 19)+
  geom_smooth(se =FALSE)
print(wk8v9 + ggtitle("Migrant Birds with Nocturnal Flight Calls Collide More when Artificial Light is Present"))

#_OR_

wk8v9 <- ggplot(wk8v8, aes(x = AvgLight, y = Log_transformed_collisions, col = flightcall2))+
  geom_point(size = 2, shape = 19)+
  geom_smooth(se =FALSE)+
  geom_text(aes(label=species),hjust=0, vjust=0)
print(wk8v9 + ggtitle("Migrant Birds with Nocturnal Flight Calls Collide More when Artificial Light is Present"))


wk8v10 <- ggplot(wk8v8, aes(x = AvgLight, fill= flightcall2, col = flightcall2))+
  geom_histogram(position= "identity", alpha = 0.5)
print(wk8v10 + ggtitle("Migrant Birds with Nocturnal Flight Calls Collide More when Artificial Light is Present"))

