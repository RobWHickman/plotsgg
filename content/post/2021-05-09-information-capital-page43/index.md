---
title: 'The Information Capital - Page 43'
author: 'Robert Hickman'
date: '2021-05-10'
slug: []
categories: ["the information capital", "london"]
tags: ["info capital", "london"]
---

 Code to make the plot for page 43 of The Information Capital
 <!--more-->


### Libraries

First we need to load some libraries. As per usual I'm doing most munging in tidy format just for ease of communication. {rgdal} and {sf} will be needed to do the spatial manipulation. {googleway} will come in useful to geocode the location of towns and then {ggthemes} and {cowplot} will be used to assemble the plots (made in {ggplot2}) together.

```{r libraries, warning=FALSE,message=FALSE}
# munging
library(tidyverse)
# spatial
library(rgdal)
library(sf)
# geocode areas
library(googleway)
# plotting
library(ggthemes)
library(cowplot)


```

### Load commuting data

First we load up the commuting data which can be found [at the government website here](https://www.statistics.digitalresources.jisc.ac.uk/dataset/wu03ew-2011-msoamsoa-location-usual-residence-and-place-work-method-travel-work). This can be downloaded as a .csv which we'll load up and glimpse

```{r load_data, echo = FALSE, warning=FALSE,message=FALSE}
# https://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_0
shapes <- readOGR("~/Desktop/plotsgg_files/infocapital_page43", layer = "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries", verbose = FALSE) %>%
  st_as_sf() %>%
  select(id = msoa11cd) %>%
  st_centroid()

# https://www.statistics.digitalresources.jisc.ac.uk/dataset/wu03ew-2011-msoamsoa-location-usual-residence-and-place-work-method-travel-work
wu03ew <- read_csv("~/Desktop/plotsgg_files/infocapital_page43/wu03ew_v2.csv", col_types = cols()) %>%
  select(
    from = `Area of residence`, 
    to = `Area of workplace`, n = `All categories: Method of travel to work`
  )

```

```{r load_commute_data_public, eval=FALSE}
# https://www.statistics.digitalresources.jisc.ac.uk/dataset/wu03ew-2011-msoamsoa-location-usual-residence-and-place-work-method-travel-work
wu03ew <- read_csv("~/myfolder/wu03ew_v2.csv", col_types = cols()) %>%
  select(
    from = `Area of residence`, 
    to = `Area of workplace`, 
    n = `All categories: Method of travel to work`
  )

# glimpse data
head(wu03ew)
```

```{r glimpse_commute_data, echo=FALSE,warning=FALSE,message=FALSE}
head(wu03ew)

```

Where we have a column of where people live ('from') and where they work ('to') and then the number of people that make that journey to work. The code for 'from' and 'to' are Medium Layer Super Output Areas which are census areas of a few thousand people. To geocode these, we're going to have to load the shape files for this level which can be found [here](https://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_0).

(E02000001	here is the MSOA for the City of London)

```{r load_shapes_public, eval = FALSE}
# https://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_0
shapes <- readOGR(
  dsn = "~/myfolder", 
  layer = "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries", 
  verbose = FALSE
) %>%
  # convert to sf
  st_as_sf() %>%
  # we only care about the id
  select(id = msoa11cd) %>%
  # we're going to assume everyone moves to/from the centre of each area
  st_centroid()

# plot the centre points of each shape
plot(shapes)
```

```{r glimpse_shapes, echo=FALSE,warning=FALSE,message=FALSE}
plot(shapes)
```

and we can see that the codes there should match those from the commute data.

### Convert to lines

We then need to convert this to/from into lines between centroids of the two MSOAs. First we assign an id for every start/end of a commuter journey then pivot to longer format and bind the location of either the start of end on.

Once we've done that, {sf} makes it fairly easy (though time consuming) to create lines by summarising the points by id to create a multipoint sf object and then casting this to a linestring between the two points.

```{r create_lines, eval = FALSE}
# join centroid point data
point_data <- wu03ew %>%
  # filter only journeys
  filter(from != to) %>%
  # filter only journeys done by >20 people
  filter(n > 20) %>%
  # init an id and pivot longer for casting to lines
  mutate(journey_id = 1:n()) %>%
  pivot_longer(cols = c(-journey_id, -n)) %>%
  left_join(shapes, by = c("value" = "id")) %>%
  st_as_sf()

# convert points to lines
line_data <- point_data %>%
  # regroup by id
  group_by(journey_id, n) %>%
  # summarise to multipoint then cast to line
  summarise() %>%
  filter(!map_lgl(geometry, st_is, 'POINT')) %>%
  st_cast('LINESTRING')

head(line_data)
```

```{r load_line_data, echo = FALSE}
point_data <- readRDS("~/Desktop/plotsgg_files/infocapital_page43/commuter_points.rds")
line_data <- readRDS("~/Desktop/plotsgg_files/infocapital_page43/all_commuter_lines.rds") %>%
  filter(n > 20)

head(line_data)
```

And now we have the most important parts of the plot and can start building it in a basic sense by just plotting the lines with 80% opacity. It already looks pretty cool and getting towards the final thing.

```{r make_first_plot, warning=FALSE,message=FALSE}
# plot lines
draft_plot <- ggplot() +
  geom_sf(data = line_data, alpha = 0.2)

# plot
draft_plot
```

### Inset plot

The plot on the page also includes an inset plot showing the extent of the main plot in the south of England and Wales. To create this, we're going to need shapefiles for the countries of the UK, which we can get from [Boundary Line](https://osdatahub.os.uk/downloads/open/BoundaryLine). I then also do a bit of munging of the names and groupings and also split out a separate London shapefile that we'll need.

```{r create_shapes_private, echo=FALSE,message=FALSE,warning=FALSE}
# boundary line nuts1 regions
# https://osdatahub.os.uk/downloads/open/BoundaryLine
nuts1 <- readOGR(
  dsn = "~/Desktop/boundary_line/Data/GB",
  layer = "european_region_region",
  verbose = FALSE
) %>%
  st_as_sf() %>%
  group_by(NAME) %>%
  summarise()

uk <- nuts1 %>%
  mutate(country = case_when(
    grepl("^Scotland", NAME) ~ "Scotland",
    grepl("^Wales", NAME) ~ "Wales",
    TRUE ~ "England"
  )) %>%
  group_by(country) %>%
  summarise()

london <- nuts1 %>%
  filter(grepl("^London", NAME))

```

```{r do_shapes, eval=FALSE}
# boundary line nuts1 regions
# https://osdatahub.os.uk/downloads/open/BoundaryLine
nuts1 <- readOGR(
  dsn = "~/Desktop/boundary_line/Data/GB",
  layer = "european_region_region",
  verbose = FALSE
) %>%
  st_as_sf() %>%
  group_by(NAME) %>%
  summarise()

# group countries together and rename
uk <- nuts1 %>%
  mutate(country = case_when(
    grepl("^Scotland", NAME) ~ "Scotland",
    grepl("^Wales", NAME) ~ "Wales",
    TRUE ~ "England"
  )) %>%
  group_by(country) %>%
  summarise()

# split out separate london shape
london <- nuts1 %>%
  filter(grepl("^London", NAME))

```

I can then make the bounding box for the main plot which will be shown on this inset. I did this by eye so its not perfect but I think it's close enough.

```{r create_bounding_box, warning=FALSE,message=FALSE}
# bounds of the main plot
bounding_box <- data.frame(
  x = c(105000, 105000, 675000, 675000),
  y = c(5000, 270000, 270000, 5000)
) %>%
  # cast to bbox
  st_as_sf(coords = c("x", "y"), crs = st_crs(uk)) %>%
  st_bbox()
```

We can now create the inset plot by plotting the UK countries + London and then layering this bounding box over them. The background of this inset also needs to be the same as the main plot so I'll set that here as `plot_bg`. The rest is just fiddling with annotation labels.

```{r create_inset_plot, warning=FALSE,message=FALSE}
plot_bg <- "#0b071c"

inset_plot <- ggplot() +
  # plot shapes
  geom_sf(data = uk, fill = "#005ac4", colour = plot_bg, size = 0.35) +
  geom_sf(data = london, fill = "orange", colour = NA) +
  # annotate to match plot
  annotate("text", x = 264000, y = 806000, label = "SCOTLAND", colour = "white", size = 3) +
  annotate("text", x = 504000, y = 456000, label = "ENGLAND", colour = "white", size = 3) +
  annotate("text", x = 280000, y = 206000, label = "WALES", colour = "white", size = 3) +
  annotate("text", x = 530000, y = 225000, label = "London", colour = "white", size = 2.5) +
  annotate("text", x = 535000, y = 20000, label = "AREA ENLARGED", colour = "grey", size = 2.25, fontface = "bold") +
  # add bounding box
  geom_sf(data = st_as_sfc(bounding_box), fill = NA, colour = "grey") +
  theme_map() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_sf(ylim = c(5000, 950000))

# plot
inset_plot
```
(Don't worry about the position/sizes of the labels, It'll get fixed when we save everything together with set dimensions)

### Making the plot

To test the inset, we first want to make out proper main plot (which we drafted earlier). There are 3 main points to the plot:

- the lines from MSOA to MSOA which are plotted as a slightly larger blue line behind a smaller white line to give the afterglow effect. Because there are so many lines, they have an alpha set proportionate to the log of the number of people taking that journey, which itself maxes at 10% opacity. These are easily added using geom_sf() for the lines data
- the overlay of london which is just a plotted polygon with a thick border and no fill
- the various theme elements which limit the extent of the plot to the bounding box and then add a nice background for everything to show up against

```{r make_unlabelled_plot, warning=FALSE,message=FALSE}
unlabelled_plot <- ggplot() +
  # plot underline as blue
  geom_sf(data = line_data, aes(alpha = log(n)), colour = "blue", size = 0.5) +
  # plot smaller overline as white
  geom_sf(data = line_data, aes(alpha = log(n)), colour = "white", size = 0.25) +
  scale_alpha_continuous(range = c(0.05, 0.1), guide = FALSE) +
  # plot outline of london
  geom_sf(data = london, fill = NA, colour = "orange", size = 1) +
  theme_map() +
  theme(panel.background = element_rect(fill = plot_bg)) +
  # limit extent to bounding box
  coord_sf(
    xlim = c(bounding_box[[1]], bounding_box[[3]]),
    ylim = c(bounding_box[[2]], bounding_box[[4]]),
    expand = FALSE, crs = 27700
  )

# plot
unlabelled_plot

```
From there we can add in the inset plot using {cowplot} to draw the first plot within this new plot

```{r plot_inset, warning=FALSE,message=FALSE}
plot_and_inset = ggdraw() +
  draw_plot(unlabelled_plot) +
  # add in the inset
  draw_plot(inset_plot, x = 0.7, y = 0.075, width = 0.375, height = 0.375) +
  panel_border(remove = TRUE)

# plot
plot_and_inset
```

(again, proportions will be fixed when we save it)

### Create labels

Finally, we want to create some labels for towns/cities of interest to show commutes between two of them. It would probably be easiest to filter these from a list of places using the bounding box, but I wanted to faithfully recreate the image, so I've typed them all out myself. They're split by type to make it easier to conditionally format them later (and easier for me to track that I'd added them all).

```{r get_places, warning=FALSE,message=FALSE}
london_name <- data.frame(
  name = "London",
  type = "capital"
)

city_names <- data.frame(
  name = c(
    "Truro",
    "Plymouth",
    "Exeter",
    "Swansea",
    "Cardiff",
    "Newport",
    "Bristol",
    "Hereford",
    "Bath",
    "Gloucester",
    "Worcester",
    "Salisbury",
    "Oxford",
    "Winchester",
    "Southampton",
    "Portsmouth",
    "St Albans",
    "Crawley",
    "Cambridge",
    "Chelmsford",
    "Brighton",
    "Canterbury"
  ),
  type = "city"
)

town_names <- data.frame(
  name = c(
    "Penzance",
    "Falmouth",
    "St Austell",
    "Barnstaple",
    "Haverfordwest",
    "Llanelli",
    "Carmarthen",
    "Barry",
    "Taunton",
    "Weymouth",
    "Cheltenham",
    "Bournemouth",
    "Swindon",
    "Banbury",
    "Newport, Isle of Wight",
    "Newbury",
    "Basingstoke",
    "Reading",
    "Aylesbury",
    "Milton Keynes",
    "Northampton",
    "Farnborough",
    "Guildford",
    "Luton",
    "Harlow",
    "Colchester",
    "Ipswich",
    "Felixstowe",
    "Clacton-on-Sea",
    "Southend-on-Sea",
    "Rochester",
    "Maidstone",
    "Tonbridge",
    "Eastbourne",
    "Hastings",
    "Ashford",
    "Folkestone",
    "Margate",
    "Ramsgate"
  ),
  type = "town"
)

```

Then we need to geocode these locations using the {googleway} package and the [Google maps API](https://developers.google.com/maps). You'll need to set up an account to get an API key 

```{r geocode_private, echo = FALSE,warning=FALSE,message=FALSE}
# small function to grab the lat and longs of a location
googleway_geocode <- function(place, key){
  data <- google_geocode(place, key = key)
  
  if(length(data$results) == 0) return(NULL)
  
  latlon <- data$results$geometry$location[1,] %>%
    mutate(location = place)
  # returns coordinates in the form latitude/longitude
  return(latlon)
}

key <- read_table("~/Desktop/maps/google_api_key.txt", col_names = FALSE)

# run over the nameset above
all_locations <- bind_rows(
  london_name,
  city_names,
  town_names
) %>%
  pull(name) %>%
  paste0(., ", UK") %>%
  # key for api
  map_df(googleway_geocode, key = key$X1) 

labels_data <- all_locations %>%
  mutate(
    label = gsub(", UK", "", location),
    type = case_when(
      label %in% london_name$name ~ "London",
      label %in% city_names$name ~ "city",
      label %in% town_names$name ~ "town"
    )
  ) %>%
  mutate(
    lng = case_when(
      label == "London" ~ lng,
      TRUE ~ lng + 0.035
    ),
    lat = case_when(
      label == "London" ~ lat,
      TRUE ~ lat - 0.035
    )
  ) %>%
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(4326)) %>%
  st_transform(crs = st_crs(london))

```


```{r geocode_public, eval=FALSE}
# small function to grab the lat and longs of a location
googleway_geocode <- function(place, key){
  data <- google_geocode(place, key = key)
  
  if(length(data$results) == 0) return(NULL)
  
  latlon <- data$results$geometry$location[1,] %>%
    mutate(location = place)
  # returns coordinates in the form latitude/longitude
  return(latlon)
}

# run over the nameset above
all_locations <- bind_rows(
  london_name,
  city_names,
  town_names
) %>%
  pull(name) %>%
  paste0(., ", UK") %>%
  # key for api
  map_df(googleway_geocode, key = "MY-API-KEY")

# bind in location info and position points
labels_data <- all_locations %>%
  mutate(
    label = gsub(", UK", "", location),
    type = case_when(
      label %in% london_name$type ~ "London",
      label %in% city_names$type ~ "city",
      label %in% town_names$type ~ "town"
    )
  ) %>%
  mutate(
    lng = case_when(
      label == "London" ~ lng,
      TRUE ~ lng + 0.035
    ),
    lat = case_when(
      label == "London" ~ lat,
      TRUE ~ lat - 0.035
    )
  ) %>%
  # convert to sf
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(4326)) %>%
  st_transform(crs = st_crs(london))

```

```{r label_and_plot, warning=FALSE,message=FALSE}
labelled_plot <- unlabelled_plot + 
  # add the labels split by type
  geom_sf_text(
    data = filter(labels_data, type == "London"),
    aes(label = toupper(label)),
    colour = "black", size = 4.5, fontface = "bold"
  ) +
  geom_sf_text(
    data = filter(labels_data, type == "city"),
    aes(label = toupper(label)), 
    colour = "white", size = 3.25, fontface = "bold"
  ) +
  geom_sf_text(
    data = filter(labels_data, type == "town"),
    aes(label = label), 
    colour = "grey60", size = 3
  ) +
  annotate(geom = "text", x = 160000, y = 150000, label = "UK Daily\nCommutes", fontface = "bold", colour = "white", size = 10) +
  annotate(geom = "text", x = 525000, y = 70000, label = "E n g l i s h  C h a n n e l", colour = "grey60", size = 6) +
  annotate(geom = "text", x = 260000, y = 160000, label = "B r i s t o l  C h a n n e l", colour = "grey60", size = 6) +
  annotate(geom = "text", x = 640000, y = 190000, label = "N o r t h  S e a", colour = "grey60", size = 6) +
  annotate(
    geom = "text", label = expression("data from 2011 English + Welsh Census; Recreated from"~italic("The Information Capital")~"page 43"),
    x = 250000, y = 10000, colour = "grey60", size = 4
  )
    
# add the inset plot to the labelled plot
plot_and_inset_labelled = ggdraw() +
  draw_plot(labelled_plot) +
  draw_plot(inset_plot, x = 0.725, y = 0.075, width = 0.375, height = 0.375) +
  panel_border(remove = TRUE)

# save the plot
ggsave(filename = "page43plot.png", 
       plot = plot_and_inset_labelled,
       width = 17, 
       height = 9,
       dpi = 450)

```
