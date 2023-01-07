

library(readr)
library(ggplot2)
library(magrittr)
library(dplyr)
#csv file einlesen
pokemon_data <- read_csv("DHBW_lab/Rrrrr/data/pkm_database.csv")
#Alle Kategorischen Daten werden von "" eingeleitet 
# das ist blöd für weiter verarbeitung deswegen hiermit enfernen (regex etc.)
pokemon_data <- pokemon_data %>%
  mutate_if(is.character, gsub, pattern = "\\\"", replacement = "")
#spalten namen enthalten leerzeichen
# das ist blöd für weiter verarbeitung deswegen hiermit enfernen (regex etc.)
names(pokemon_data) <- make.names(names(pokemon_data))
#debug shit
head(pokemon_data)
colnames(pokemon_data)
View(pokemon_data)
#entfernen von den spalten die nicht nötig für die analyse sind
pokemon_filtered <- select(pokemon_data,  -contains("EV"), -contains("Egg"), -contains("Experience"), -contains("Ability"), -contains("Ratio"), -contains("Happiness"), -contains("Height"), -contains("Weight"), -contains("Classification"), -contains("Rate"))
#Manche Pokemons haben alternativeformen einige avon sind aktuell nicht spielbar wie Megas oder Gigantamax(Generational Gimmicks)
#oder formen die zu sehr ähnlich zu ihrer form
#deswegen werden diese formen raus gefiltert
pokemon_filtered <- filter(pokemon_filtered, !Alternate.Form.Name %in% c("Mega", "Mega X", "Mega Y", "Gigantamax", "Female", "Fan", "Galar Zen", "Low Key Gigantamax", 
                                                                                              "Lowkey", "Snowy", "Starter", "Zen", "Heat", "Wash", "Rainy", "Eternamax"))
#wir wollen die generationen betrachten die pokemons besitzen eigenschaft game of origin diese wird auf -
origin_mapping <- c("Red" = 1, "Gold" = 2, "Emerald" = 3, "Ruby" = 3, "Diamond" = 4, "Black" = 5, "Black 2" = 5, "X" = 6, "Sun" = 7, "Ultra Sun" = 7, "Pokemon GO" = 8, "Sword" = 8, "Scarlet" = 9, "Legends: Arceus" = 9)
pokemon_filtered <- mutate(pokemon_filtered, Generation = origin_mapping[match(pokemon_filtered$"Game.s..of.Origin", names(origin_mapping))])

generation_colours <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
#debugshit
colnames(pokemon_filtered)
View(pokemon_filtered)
pokemon_filtered <- select(pokemon_filtered, -"Game.s..of.Origin")

#Daten Vorbereitung abgeschlossen nun kann analyse begonnen werden

ggplot(pokemon_filtered, aes(x = Pokedex.Number, y = Base.Stat.Total, color = Base.Stat.Total)) + 
  geom_point() +
  labs(title = "Pokemon Base Stat Totals", y = "Base Stat Total") +
  scale_y_continuous(limits = c(100, max(pokemon_filtered$Base.Stat.Total))) +
  scale_color_gradient(low = "darkblue", high = "red")
# Aufällig: verdichtet zwischen 450-550
# Viele bei 600 Pseudo Legendary, das erjkläre glaub einfach ich :)
#kannst einfach als sagen wie geplotet und koloriert wird

#Legenddäre Pokemon sind besonders stark deswegen wird angeschaut wie die auf generationen verteilt werden
# Group the data by Generation and calculate the number of each type of Legendary.Type within each group
legendary_counts <- pokemon_filtered %>%
  group_by(Generation, Legendary.Type) %>%
  summarize(count = n())
#Auffälligkeiten 

# Create a stacked bar chart showing the share of each Legendary.Type in each Generation
ggplot(legendary_counts, aes(x = Generation, y = count, fill = Legendary.Type)) +
  geom_col() +
  scale_color_manual(values = generation_colours)


ggplot(data = pokemon_filtered, aes(x = factor(Generation), y = Health.Stat, fill = factor(Generation))) +
  geom_boxplot() +
  scale_color_manual(values = generation_colours)

ggplot(data = pokemon_filtered, aes(x = factor(Generation), y = Attack.Stat, fill = factor(Generation))) +
  geom_boxplot() +
  scale_color_manual(values = generation_colours)

ggplot(data = pokemon_filtered, aes(x = factor(Generation), y = Defense.Stat, fill = factor(Generation))) +
  geom_boxplot() +
  scale_color_manual(values = generation_colours)


ggplot(data = pokemon_filtered, aes(x = factor(Generation), y = Special.Attack.Stat, fill = factor(Generation))) +
  geom_boxplot() +
  scale_color_manual(values = generation_colours)

ggplot(data = pokemon_filtered, aes(x = factor(Generation), y = Special.Defense.Stat, fill = factor(Generation))) +
  geom_boxplot() +
  scale_color_manual(values = generation_colours)

ggplot(data = pokemon_filtered, aes(x = factor(Generation), y = Base.Stat.Total, fill = factor(Generation))) +
  geom_boxplot() +
  scale_color_manual(values = generation_colours)

pokemon_leg_filtered <- filter(pokemon_filtered, !Legendary.Type %in% c("Legendary", "Mythical","Sub-Legendary"))

ggplot(data = pokemon_leg_filtered, aes(x = factor(Generation), y = Base.Stat.Total, fill = factor(Generation))) +
  geom_boxplot() +
  scale_color_manual(values = generation_colours)
  

ggplot(data = pokemon_leg_filtered, aes(x = factor(Generation), y = Base.Stat.Total, fill = factor(Generation))) +
  stat_summary(fun.y = "median", geom = "bar") +
  scale_fill_manual(values = generation_colours)
  
                                                                    

