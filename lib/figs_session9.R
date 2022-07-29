###############################################################################
# S E S S I O N  9
###############################################################################
# -------------------------------------------
# L I B R A R I E S
# -------------------------------------------

# Required libraries.
# remotes::install_github("kjhealy/covdata@main")
library(covdata)
library(tidyverse)
library(sf)
library(lubridate)
library(RcppRoll)

# -------------------------------------------
# D A T A
# -------------------------------------------

# Cases data by US county is downloaded with the covdata package.
# Healey notes that this data was originally collated by New York Times.
# Details here: https://kjhealy.github.io/covdata/articles/new-york-times.html

county_data <- nytcovcounty %>% 
  # New York City recode to geoid : 36061
  mutate(
    fips=case_when(
      county == "New York City" ~ as.character(36061),
      TRUE ~ fips
    )
  ) %>% 
  # We focus up to end of May 2020 (as per Washington Post graphic).
  filter(date< "2020-05-26", county!="Unknown") %>% 
  group_by(fips) %>% 
  mutate(
    cases_cum=cumsum(cases),
    # 7-day rolling cases (though we don't use this).
    cases_mov_avg_local=roll_mean(cases,7,align="right", fill=0),
    # Find cases on May 3.
    cases_start=if_else(date=="2020-05-03", cases,0)
  ) %>% ungroup %>% 
  filter(date> "2020-05-02") %>% 
  group_by(fips) %>% 
  # Remove those where <20 cases on May 3. 
  mutate(remove=max(cases_start)) %>% ungroup %>% 
  filter(remove>19) %>% 
  mutate(
    growth_rate=cases/remove,
    end_rate=if_else(date=="2020-05-25", growth_rate,0),
    end_cases=if_else(date=="2020-05-25", cases,0)
  ) %>% 
  group_by(fips) %>% 
  mutate(
    end_rate=max(end_rate),
    end_cases=max(end_cases),
    binned_growth_rate=
      case_when(
        max(end_rate) > 7 ~ 4,
        max(end_rate) > 4 ~ 3,
        max(end_rate) > 2 ~ 2,
        TRUE ~ 1
      ),
    day_num=row_number()
  ) %>% ungroup() 

# Boundary data.
# From : https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# Counties.
url <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_20m.zip"
download.file(url, "./data/county_boundaries.zip", mode="wb")
unzip("./data/county_boundaries.zip", exdir="./data/county_boundaries")
# Files have complex names that vary on download, so record and use on reading-in.
temp_boundary_file <- list.files("./data/county_boundaries", pattern=".shp")
county_boundaries <- st_read(paste0("./data/county_boundaries/", temp_boundary_file[1]))
# Delete directory with large shapefile.
unlink("./data/county_boundaries", recursive=TRUE)
# States.
url <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip"
download.file(url, "./data/state_boundaries.zip", mode="wb")
unzip("./data/state_boundaries.zip", exdir="./data/state_boundaries")
# Files have complex names that vary on download, so record and use on reading-in.
temp_boundary_file <- list.files("./data/state_boundaries", pattern=".shp")
state_boundaries <- st_read(paste0("./data/state_boundaries/", temp_boundary_file[1]))
# Delete directory with large shapefile.
unlink("./data/state_boundaries", recursive=TRUE)

# Load in county population data - for calculating relative case rates.
# From: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902
county_pop <- 
  read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv") %>% 
  select(STATE, COUNTY, POPESTIMATE2019) %>% 
  transmute(GEOID=paste0(STATE,COUNTY), pop=POPESTIMATE2019) 

# Join state data to counties.
county_boundaries <- county_boundaries %>% 
  inner_join(state_boundaries %>% st_drop_geometry() %>%  select(STATEFP, NAME_STATE=NAME))

# Filter only on mainland states 
state_boundaries <- state_boundaries %>% filter(!NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))
# Use Albers Equal Area.
state_boundaries <- st_transform(state_boundaries, crs=5070)
st_crs(state_boundaries)
county_boundaries <- county_boundaries  %>% filter(!NAME_STATE %in% c("Alaska", "Hawaii", "Puerto Rico"))
# Use Albers Equal Area.
county_boundaries <- st_transform(county_boundaries, crs=5070)

# Calculate centroids of counties and states.
county_centroids <- county_boundaries %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename("x"="X", "y"="Y") %>% 
  add_column(GEOID = county_boundaries %>% pull(GEOID))
county_boundaries <- county_boundaries %>% left_join(county_centroids)   
state_centroids <- state_boundaries %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename("x"="X", "y"="Y") %>% 
  add_column(GEOID = state_boundaries %>% pull(GEOID))
state_boundaries <- state_boundaries %>% left_join(state_centroids)     

# Fix geog exceptions.
# Detailed in Healey's pages for covdata package: https://kjhealy.github.io/covdata/articles/new-york-times.html
# Merging NYC counties -- New York, Kings, Queens, Bronx and Richmond -- into New York.
county_pop <- county_pop %>% 
  inner_join(county_boundaries %>% st_drop_geometry() %>% select(GEOID, NAME, NAME_STATE)) %>% 
  mutate(
    GEOID=case_when(
      NAME_STATE=="New York" & NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") ~ "36061",
      TRUE ~ GEOID
    )
  ) %>% 
  group_by(GEOID) %>% 
  summarise(pop=sum(pop)) %>% ungroup 

# Join county population.
county_boundaries <- county_boundaries %>% inner_join(county_pop) 


# ------------------------
# W R I T E  C O U N T Y  D A T A
# --------------------------

write_csv(county_data, "county_covid_wp.csv")

# -------------------------------------------
# G R A P H I C
# -------------------------------------------

# Identify counties growth rates thresholds for highlighting.
find_filters <- county_data %>% inner_join(county_boundaries %>% select(GEOID, x, y, pop), by=c("fips"="GEOID")) %>% 
  mutate(binned_growth_rate=factor(binned_growth_rate))  %>% 
  group_by(fips, county, state) %>% 
  mutate(
    case_rate=end_cases/pop,
    binned_case_rate=
      case_when(
        case_rate < 0.00208 ~ 1,
        case_rate < 0.00437 ~ 2,
        case_rate < .00862 ~ 3,
        case_rate < 0.0132 ~ 4,
        TRUE ~ 5
      ),
    growth_rate_cont=end_rate,
  ) %>% ungroup %>% mutate(binned_case_rate=factor(binned_case_rate)) %>% 
  select(fips, county, state, case_rate, end_rate, end_cases) %>% distinct()

# Counties to annotate.  
annotate <- find_filters %>% filter(
  county == c("Huntingdon") & state=="Pennsylvania" |
    county == c("Lenawee") & state=="Michigan" |
    county == c("Crawford") & state=="Iowa" |
    county == c("Wapello") & state=="Iowa" |
    county == c("Lake") & state=="Tennessee" |
    state == c("Oklahoma") & county=="Texas" |
    county == c("Duplin") & state=="North Carolina" |
    county == c("Santa Cruz") & state=="Arizona"|
    county == c("Titus") & state=="Texas"|
    county == c("Yakima") & state=="Washington"
) %>% 
  inner_join(county_boundaries %>% st_drop_geometry() %>% select(STATEFP, GEOID, x,y), by=c("fips"="GEOID")) %>% 
  mutate(
    state_abbr=case_when(
      state == "Pennsylvania" ~ "Penn.",
      state == "Iowa" ~ "Iowa",
      state == "Tennessee" ~ "Tenn.",
      state == "Oklahoma" ~ "Okla.",
      state == "Texas" ~ "Texas",
      state == "North Carolina" ~ "N.C.",
      state == "Washington" ~ "Wash.",
      state == "Michigan" ~ "Mich.",
      state == "Arizona" ~ "Arizona",
      TRUE ~ ""
    ),
    end_rate_round = round(end_rate,0)
  )

# Annotated legend -- following that used in original Washington Post graphic.
legend <- county_data %>% 
  filter(
    county == "Dubois" & state=="Indiana" |
      county == "Androscoggin" & state=="Maine" |
      county == "Fairfax" & state=="Virginia" |
      county == "Bledsoe" & state=="Tennessee"
  ) %>% 
  mutate(
    x=bbox$xmax-.25*width,y=bbox$ymax+.05*height, case_rate=.01, 
    binned_growth_rate=factor(binned_growth_rate), 
    end_rate_round = round(end_rate,0),
    label=case_when(
      county == "Dubois" ~ "7x more cases than on May 3",
      county == "Androscoggin" ~ "4x",
      county == "Fairfax" ~ "2x",
      county == "Bledsoe" ~ "About the same as on May 3"
    )
  )

# Bounding box for mainland US.
bbox <- st_bbox(state_boundaries)
width <- bbox$xmax-bbox$xmin
height <- bbox$ymax-bbox$ymin

legend_thickness <- county_data %>% 
  filter(
    county == "Kings" & state=="California" ) %>% 
  mutate(
    x=bbox$xmax-.88*width,y=bbox$ymax+.05*height, case_rate=.01, 
    binned_growth_rate=factor(binned_growth_rate)
  ) %>% 
  select(x, y, day_num, growth_rate, binned_growth_rate, case_rate, fips) %>% 
  mutate(
    low=.001, mid=.009, high=.015,
    ) %>%
  pivot_longer(cols=c(low, mid, high), names_to="offset", values_to="offset_rate") %>% 
  mutate(
    offset_day= case_when(
      offset == "low" ~ 0,
      offset == "mid" ~ .04,
      offset == "high" ~ .08
    )
  )


geom_path(
  data=legend_thickness %>% mutate(case_rate=.001),
  aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
  lineend="round"
) +
  geom_path(
    data=legend_thickness %>% mutate(case_rate=.009),
    aes(x=x+(day_num*6000)-6000+.04*width, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  geom_path(
    data=legend_thickness %>% mutate(case_rate=.015),
    aes(x=x+(day_num*6000)-6000+.08*width, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +


plot <- county_data %>% inner_join(county_boundaries %>% select(GEOID, x, y, pop), by=c("fips"="GEOID")) %>% 
  mutate(binned_growth_rate=factor(binned_growth_rate))  %>% 
  group_by(fips, county, state) %>% 
  mutate(
    case_rate=end_cases/pop,
    binned_case_rate=
      case_when(
        case_rate < 0.00208 ~ 1,
        case_rate < 0.00437 ~ 2,
        case_rate < .00862 ~ 3,
        case_rate < 0.0132 ~ 4,
        TRUE ~ 5
      ),
    growth_rate_cont=end_rate,
  ) %>% ungroup %>% mutate(binned_case_rate=factor(binned_case_rate)) %>%
  ggplot()+
  geom_sf(data=state_boundaries, fill="#eeeeee", colour="#bcbcbc", size=0.2)+ 
  coord_sf(crs=5070, datum=NA, clip="off")+
  geom_point(data=.%>% filter(date=="2020-05-03"), aes(x=x, y=y, colour=binned_growth_rate, alpha=binned_growth_rate, size=case_rate))+
  # Plot case data.
  geom_path(
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  # Plot legend.
  geom_path(
    data=legend,
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  geom_path(
    data=legend_thickness %>% mutate(case_rate=.001),
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  geom_path(
    data=legend_thickness %>% mutate(case_rate=.009),
    aes(x=x+(day_num*6000)-6000+.04*width, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  geom_path(
    data=legend_thickness %>% mutate(case_rate=.015),
    aes(x=x+(day_num*6000)-6000+.08*width, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  # Annotate.
  geom_text(data=state_boundaries, aes(x=x,y=y,label=STUSPS), family="Avenir Book", alpha=.8)+ 
  geom_text(data=annotate, aes(x=x,y=y-20000,label=paste0(county,", ",state_abbr)), family="Avenir Heavy", alpha=1, size=3)+ 
  geom_text(data=annotate, aes(x=x,y=y-65000,label=paste0(end_rate_round,"X more cases")), family="Avenir Book", alpha=1, size=2.5)+ 
  geom_text(data=legend %>% filter(day_num == max(county_data$day_num), county!="Bledsoe"), aes(x=x+(day_num*6000)+10000,y=y+(growth_rate*50000)-50000,label=str_wrap(label, 15)), 
            family="Avenir Book", alpha=1, size=2.5, hjust=0, vjust=0)+
  geom_text(data=legend %>% filter(day_num == max(county_data$day_num), county=="Bledsoe"), aes(x=x+(day_num*6000)+10000,y=y+(growth_rate*50000)-50000,label=str_wrap(label, 15)), 
            family="Avenir Book", alpha=1, size=2.5, hjust=0, vjust=1)+ 
  annotate("text", x=bbox$xmax-.5*width, y=bbox$ymax+.15*height, label="Change in reported cases since May 3", 
           family="Avenir Heavy", alpha=1, size=5)+ 
  annotate("text", x=bbox$xmax-.25*width, y=bbox$ymax+.08*height, label=str_wrap("Line height and colour show change in reported cases relative to May 3",35), 
           family="Avenir Medium", alpha=1, size=3.5, hjust=1)+
  annotate("text", x=bbox$xmax-.75*width, y=bbox$ymax+.08*height, label=str_wrap("Line thickness shows current number relative to county population",35), 
           family="Avenir Medium", alpha=1, size=3.5, hjust=0)+
  annotate("text", x=bbox$xmax-.88*width, y=bbox$ymax+.04*height, label="Less", family="Avenir Book", alpha=1, size=2.5, hjust=0.5)+
  annotate("text", x=bbox$xmax-.8*width, y=bbox$ymax+.04*height, label="More", family="Avenir Book", alpha=1, size=2.5, hjust=0.5)+
  scale_colour_manual(values=c("#fa9fb5", "#dd3497", "#7a0177", "#49006a"))+
  scale_size(range=c(.1,2.5))+
  scale_alpha_ordinal(range=c(.2,1))+
  guides(colour=FALSE, size=FALSE, alpha=FALSE)+
  labs(x="",y="")

ggsave("./docs/img/wp.png", plot, width=14, height=10, dpi=300)







plot <- county_data %>% 
  ggplot()+
  geom_sf(data=state_boundaries, fill="#eeeeee", colour="#bcbcbc", size=0.2)+ 
  geom_text(data=state_boundaries, aes(x=x,y=y,label=STUSPS), family="Avenir Book", alpha=.8)+ 
  coord_sf(crs=5070, datum=NA, clip="off")+
  geom_point(data=.%>% filter(date=="2020-05-03"), aes(x=x, y=y, colour=binned_growth_rate, alpha=binned_growth_rate, size=case_rate))+
  # Plot case data.
  geom_path(
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  scale_colour_manual(values=c("#fa9fb5", "#dd3497", "#7a0177", "#49006a"))+
  scale_size(range=c(.1,2.5))+
  scale_alpha_ordinal(range=c(.2,1))+
  guides(colour=FALSE, size=FALSE, alpha=FALSE)+
  theme_void()


grid.draw.gg <- function(x){
  print(x)
}

ggsave(filename=here("static","class","09-class_files","wp-basic.png"), plot=plot,width=14, height=10, dpi=300)



# Counties to annotate.  
annotate <- county_data %>% filter(
    date=="2020-05-03",
    county==c("Huntingdon") & state=="Pennsylvania" |
    county==c("Lenawee") & state=="Michigan" |
    county==c("Crawford") & state=="Iowa" |
    county==c("Wapello") & state=="Iowa" |
    county==c("Lake") & state=="Tennessee" |
    county=="Texas" & state == c("Oklahoma") |
    county==c("Duplin") & state=="North Carolina" |
    county==c("Santa Cruz") & state=="Arizona"|
    county==c("Titus") & state=="Texas"|
    county==c("Yakima") & state=="Washington"
    ) %>% 
  mutate(
    state_abbr=case_when(
      state=="Pennsylvania" ~ "Penn.",
      state=="Iowa" ~ "Iowa",
      state=="Tennessee" ~ "Tenn.",
      state=="Oklahoma" ~ "Okla.",
      state=="Texas" ~ "Texas",
      state=="North Carolina" ~ "N.C.",
      state=="Washington" ~ "Wash.",
      state=="Michigan" ~ "Mich.",
      state=="Arizona" ~ "Arizona",
      TRUE ~ ""
    ),
    end_rate_round = round(end_rate,0)
  ) 



# Bounding box for mainland US.
bbox <- st_bbox(state_boundaries)
width <- bbox$xmax-bbox$xmin
height <- bbox$ymax-bbox$ymin

# Legend : rate
legend_growth <- county_data %>% 
  filter(
    county=="Dubois" & state=="Indiana" |
    county=="Androscoggin" & state=="Maine" |
    county=="Fairfax" & state=="Virginia" |
    county=="Bledsoe" & state=="Tennessee"
  ) %>% 
  mutate(
    x=bbox$xmax-.25*width,y=bbox$ymax+.05*height, case_rate=.01, 
    end_rate_round = round(end_rate,0),
    label=case_when(
      county == "Dubois" ~ "7x more cases than on May 3",
      county == "Androscoggin" ~ "4x",
      county == "Fairfax" ~ "2x",
      county == "Bledsoe" ~ "About the same as on May 3"
    )
  )




# Bounding box for mainland US.
bbox <- st_bbox(state_boundaries)
width <- bbox$xmax-bbox$xmin
height <- bbox$ymax-bbox$ymin

legend_case <- county_data %>% 
  filter(
    county == "Kings" & state=="California" ) %>% 
  mutate(
    x=bbox$xmax-.88*width,y=bbox$ymax+.05*height,  
    binned_growth_rate=factor(binned_growth_rate)
  ) %>% 
  select(x, y, day_num, growth_rate, binned_growth_rate, fips) %>% 
  mutate(
    low=.001, mid=.009, high=.015,
  ) %>%
  pivot_longer(cols=c(low, mid, high), names_to="offset", values_to="offset_rate") %>% 
  mutate(
    offset_day= case_when(
      offset == "low" ~ 0,
      offset == "mid" ~ .04,
      offset == "high" ~ .08
    )
  )


plot <- county_data %>% 
  ggplot()+
  geom_sf(data=state_boundaries, fill="#eeeeee", colour="#bcbcbc", size=0.2)+ 
  geom_text(data=state_boundaries, aes(x=x,y=y,label=STUSPS), family="Avenir Book", alpha=.8)+ 
  geom_text(data=annotate, aes(x=x,y=y-20000,label=paste0(county,", ",state_abbr)), family="Avenir Heavy", alpha=1, size=3)+ 
  geom_text(data=annotate, aes(x=x,y=y-65000,label=paste0(end_rate_round,"X more cases")), family="Avenir Book", alpha=1, size=2.5)+
  coord_sf(crs=5070, datum=NA, clip="off")+
  geom_point(data=.%>% filter(date=="2020-05-03"), aes(x=x, y=y, colour=binned_growth_rate, alpha=binned_growth_rate, size=case_rate))+
  # Plot case data.
  geom_path(
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  # Plot growth lines.
  geom_path(
    data=legend_growth,
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  # Plot case lines.
  geom_path(
    data=legend_case,
    aes(x=x+(day_num*6000)-6000+offset_day*width, y=y+(growth_rate*50000)-50000, group=paste0(fips,offset), colour=binned_growth_rate, size=offset_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  # Text labels for legend lines.
  geom_text(data=legend_growth %>% filter(day_num == max(county_data$day_num)) %>% 
              mutate(growth_rate=if_else(county=="Bledsoe", -1,growth_rate)), 
            aes(x=x+(day_num*6000)+10000,y=y+(growth_rate*50000)-50000,label=str_wrap(label, 15)), 
            family="Avenir Book", alpha=1, size=2.5, hjust=0, vjust=0)+
  annotate("text", x=bbox$xmax-.88*width, y=bbox$ymax+.04*height, label="Less", family="Avenir Book", alpha=1, size=2.5, hjust=0.5)+
  annotate("text", x=bbox$xmax-.8*width, y=bbox$ymax+.04*height, label="More", family="Avenir Book", alpha=1, size=2.5, hjust=0.5)+
  annotate("text", x=bbox$xmax-.25*width, y=bbox$ymax+.08*height, label=str_wrap("Line height and colour show change in reported cases relative to May 3",35), 
           family="Avenir Medium", alpha=1, size=3.5, hjust=1)+
  annotate("text", x=bbox$xmax-.75*width, y=bbox$ymax+.08*height, label=str_wrap("Line thickness shows current number relative to county population",35), 
           family="Avenir Medium", alpha=1, size=3.5, hjust=0)+
  annotate("text", x=bbox$xmax-.5*width, y=bbox$ymax+.15*height, label="Change in reported cases since May 3", 
            family="Avenir Medium", alpha=1, size=5)+ 
  scale_colour_manual(values=c("#fa9fb5", "#dd3497", "#7a0177", "#49006a"))+
  scale_size(range=c(.1,2.5))+
  scale_alpha_ordinal(range=c(.2,1))+
  guides(colour=FALSE, size=FALSE, alpha=FALSE)+
  theme_void()

ggsave(filename=here("static","class","09-class_files","wp-growth.png"), plot=plot,width=14, height=10, dpi=300)
ggsave(filename=here("static","class","09-class_files","wp-case.png"), plot=plot,width=14, height=10, dpi=300)


  