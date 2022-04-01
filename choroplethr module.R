# import load library

library(choroplethr)
library(choroplethrMaps)

# load dataset
data(df_pop_state)
#see dataset
df_pop_state
#
View(df_pop_state)
# customizing map
# map data
state_choropleth(df_pop_state)
#add titel and legend
state_choropleth(df_pop_state, 
                 title = "2012 state population estimates",
                 legend = "population")
# change scale 
state_choropleth(df_pop_state, num_colors = 7)
state_choropleth(df_pop_state, num_colors = 2)
state_choropleth(df_pop_state, num_colors = 1)
# if you zoom on some are
state_choropleth(df_pop_state,
                 zoom = c("california","oregon", "washington"))
# when you corelate your map with other map reference map
library(ggmap)
#register_google("your api key")

state_choropleth(df_pop_state,
                 num_colors = 1,
                 zoom = c("california","oregon", "washington"),
                 reference_map = TRUE)
#-------------------------------------------------------------------------------------
#Variable and vantage
?df_state_demographics
data(df_state_demographics)
df_state_demographics
View(df_state_demographics)

# add variable
df_state_demographics$value <- df_state_demographics$per_capita_income
df_state_demographics
# plot data
state_choropleth(df_state_demographics,
                 num_colors = 2,
                 title = "2013 state per capita income estimates",
                 legend = "Dollars")

# add more years

#https://api.census.gov/data/key_signup.html
#when you receive the key the
# api.key.install("your key here")
?get_state_demographics
df_2010 = get_state_demographics(2010)
View(df_2010)

df_2010$value <- df_2010$per_capita_income
state_choropleth(df_2010)
# if i set 2015 capita income
df_2015 <- get_state_demographics(2015)
df_2015$ value <- df_2015$per_capita_income
state_choropleth(2015)

# calculating percentage
?calculate_percent_change
df_chaneg <- calculate_percent_change(df_2010,df_2015)
View(df_chaneg)

df_chaneg <- df_chaneg[,c("region","value.x","value.y", "value")]
View(df_chaneg)

# mapping percentage
state_choropleth(df_chaneg)
state_choropleth(df_chaneg, num_colors = 0)
state_choropleth(df_chaneg, num_colors = 0,
                 title = "name",
                 legend = "percentage")

#----------------------------------------------------------
#module 4