###############################################################################
# S E S S I O N  8
###############################################################################

library(tidyverse)
library(tidymodels) # For bootstrapping
library(here)
library(sf)
library(lubridate)
library(gganimate)
#install.packages("ggdist")
library(ggdist)
install.packages("distributional")
library(distributional)
library(patchwork)

url <- "https://www.roger-beecham.com/datasets/ped_veh.fst"
download.file(url, here("data", "ped_veh.fst"))
ped_veh_fst <- fst::read_fst(here("data", "ped_veh.fst"))



ped_veh_fst %>%
  mutate(
    is_ksi=if_else(accident_severity=="Slight", FALSE, TRUE),
    year=lubridate::year(date)
  ) %>% 
  filter(year==2019, local_authority_district %in% c("Bristol, City of", "Sheffield")) %>%
  group_by(local_authority_district) %>% 
  summarise(size=n(), ratio=mean(is_ksi)) %>% View



array_data <- tibble(
  row=rep(1:10, times=1, each=10),
  col=rep(1:10, times=10, each=1),
  Oxford=sample(c(FALSE,TRUE), size=100, replace=TRUE, prob=c(.83,.17)),
  Fareham=sample(c(FALSE,TRUE), size=100, replace=TRUE, prob=c(.59,.41))
)

plot <- array_data %>% 
  pivot_longer(cols=c(Oxford,Fareham), names_to="la", values_to="is_ksi") %>% 
  ggplot(aes(x=row,y=col, fill=is_ksi)) +
  geom_tile(colour="#ffffff", size=1) +
  scale_fill_manual(values=c("#fee0d2","#de2d26"), guide=FALSE)+
  facet_wrap(~la) +
  labs(
    title="Icon arrays of KSI rates for pedestrian-vehicle crashes",
    subtitle="--Stats19 crashes 2019",
    caption="Stats19 data accessed via `stats19` package") +
  theme(
    axis.text.x=element_blank(),axis.title.x=element_blank(),
    axis.text.y=element_blank(),axis.title.y=element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor = element_blank())

ggsave(filename="./static/class/08-class_files/icon-arrays-technical.png", plot=plot,width=5.5, height=3.8, dpi=300)




array_data <- tibble(
  row=rep(1:10, times=1, each=10),
  col=rep(1:10, times=10, each=1),
  Bristol=sample(c(FALSE,TRUE), size=100, replace=TRUE, prob=c(.85,.15)),
  Sheffield=sample(c(FALSE,TRUE), size=100, replace=TRUE, prob=c(.5,.5))
)


plot <- array_data %>% 
  pivot_longer(cols=c(Bristol,Sheffield), names_to="la", values_to="is_ksi") %>% 
  ggplot(aes(x=row,y=col, fill=is_ksi)) +
  geom_tile(colour="#ffffff", size=1) +
  scale_fill_manual(values=c("#fee0d2","#de2d26"), guide=FALSE)+
  facet_wrap(~la) +
  labs(
    title="Icon arrays of KSI rates for pedestrian-vehicle crashes",
    subtitle="--Stats19 crashes 2019",
    caption="Stats19 data accessed via `stats19` package") +
  theme(
    axis.text.x=element_blank(),axis.title.x=element_blank(),
    axis.text.y=element_blank(),axis.title.y=element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor = element_blank())


array_data <- tibble(
  row=rep(1:16, times=1, each=16),
  col=rep(1:16, times=16, each=1),
  Bristol=c(sample(c(0,1), size=228, replace=TRUE, prob=c(.85,.15)), rep(3,28)),
  Sheffield=c(sample(c(0,1), size=248, replace=TRUE, prob=c(.5,.5)),rep(3,8))
)

plot <- array_data %>% 
  pivot_longer(cols=c(Bristol,Sheffield), names_to="la", values_to="is_ksi") %>%
  mutate(is_ksi=factor(is_ksi)) %>% 
  ggplot(aes(x=row,y=col, fill=is_ksi)) +
  geom_tile(colour="#ffffff", size=1) +
  scale_fill_manual(values=c("#fee0d2","#de2d26", "#f0f0f0"), guide=FALSE)+
  facet_wrap(~la) +
  labs(
  title="Icon arrays of KSI rates for pedestrian-vehicle crashes",
  subtitle="--Stats19 crashes 2019",
  caption="Stats19 data accessed via `stats19` package") +
  theme(
    axis.text.x=element_blank(),axis.title.x=element_blank(),
    axis.text.y=element_blank(),axis.title.y=element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor = element_blank())

ggsave(filename="./static/class/08-class_files/icon-arrays.png", plot=plot,width=7, height=4.5, dpi=300)



ped_veh_fst %>%
  mutate(
    is_ksi=if_else(accident_severity=="Slight", FALSE, TRUE),
    year=lubridate::year(date)
  ) %>% 
  filter(year==2019, local_authority_district=="Bristol, City of") %>%
  select(local_authority_district, is_ksi) %>% 
  bootstraps(times=100, apparent=TRUE) %>% 
  mutate(
    #mean=map(splits, ~mean(as.data.frame(.x)$is_ksi)),
    #la=map(splits, ~first(as.data.frame(.x)$local_authority_district))
    is_ksi=map(splits, ~ analysis(.) %>% pull(is_ksi)),
    ksi_rate=map_dbl(is_ksi, ~mean(.x)),
    la=map(splits, ~ analysis(.) %>% pull(local_authority_district)),
    la_name=map_chr(la, ~first(.x))
  ) %>% 
  select(-c(splits, la, is_ksi)) %>% 
  unnest(mean)  %>%  View


rate_boots_selected <- ped_veh_fst %>%
  mutate(
    is_ksi=accident_severity!="Slight",
    year=lubridate::year(date)
  ) %>% 
  filter(year==2019, 
         local_authority_district %in% c("Bristol, City of", "Sheffield", "Bromsgrove", "Cotswold")
         ) %>%
  select(local_authority_district, is_ksi) %>%
  nest(-local_authority_district) %>%
  mutate(la_boot=map(data, bootstraps, times=1000, apparent=TRUE)) %>%
  select(-data) %>%
  unnest(la_boot) %>%
  mutate(
    is_ksi=map(splits, ~ analysis(.) %>% pull(is_ksi)),
    ksi_rate=map_dbl(is_ksi, ~mean(.x)),
    sample_size=map_dbl(is_ksi, ~length(.x))
  ) %>% 
  select(-c(splits, is_ksi)) 


plot <- rate_boots_selected %>% 
  group_by(local_authority_district) %>% 
  mutate(std.error=sd(ksi_rate)/sqrt(sample_size), lower=quantile(ksi_rate,probs=.025), upper=quantile(ksi_rate,probs=.975)) %>% 
  filter(id=="Apparent") %>% 
  ggplot(
    # aes(x=reorder(local_authority_district, ksi_rate),
    #     y=ksi_rate,ymin=ksi_rate-1.96*std.error, ymax=ksi_rate+1.96*std.error)) +
   aes(x=reorder(local_authority_district, ksi_rate),
      y=ksi_rate,ymin=lower, ymax=upper)) +
  geom_pointrange(colour=site_colours$primary) +
  labs(y="estimated ksi rate", x="local authority",
       title="KSI rates for pedestrian-vehicle crashes with 95% CIs",
       subtitle="--Stats19 crashes 2019",
       caption="Stats19 data accessed via `stats19` package") +
  coord_flip()


ggsave(filename="./static/class/08-class_files/bootstrap-selected.png", plot=plot,width=7, height=3.5, dpi=300)


half_eye <- rate_boots_selected %>% 
  group_by(local_authority_district) %>% 
  mutate(std.error=sd(ksi_rate), lower=quantile(ksi_rate,probs=.025), upper=quantile(ksi_rate,probs=.975)) %>% 
  filter(id=="Apparent") %>% 
  ggplot(aes(x=reorder(local_authority_district, ksi_rate), y=ksi_rate)) +
    # aes(x=reorder(local_authority_district, ksi_rate),
    #     y=ksi_rate,ymin=ksi_rate-1.96*std.error, ymax=ksi_rate+1.96*std.error)) +
    # aes(x=reorder(local_authority_district, ksi_rate),
    #     y=ksi_rate,ymin=lower, ymax=upper)) +
  #geom_point(colour=site_colours$primary) +
  stat_dist_halfeye(
    aes(dist = dist_normal(mu = ksi_rate, sigma = std.error)), 
    point_size = 1.5, colour=site_colours$primary) +
  labs(y="estimated ksi rate", x="local authority", subtitle="Half-eyes")+
  coord_flip()

gradient <- rate_boots_selected %>% 
  group_by(local_authority_district) %>% 
  mutate(
    std.error=sd(ksi_rate), 
    lower=quantile(ksi_rate,probs=.025), 
    upper=quantile(ksi_rate,probs=.975),
    std.error=(upper-lower)/2
    ) %>% 
  filter(id=="Apparent") %>% 
  ggplot(aes(x=reorder(local_authority_district, ksi_rate), y=ksi_rate)) +
  stat_dist_gradientinterval(
    aes(dist = dist_normal(mu=ksi_rate, sigma=std.error.int)), 
    point_size = 1.5, colour=site_colours$primary) +
  labs(y="estimated ksi rate", x="local authority", subtitle="Gradient bars")+
  coord_flip()

plot <- half_eye + gradient + plot_layout(nrow=2) + plot_annotation(
  title="KSI rates for pedestrian-vehicle crashes with uncertainty estimates",
  subtitle="--Stats19 crashes 2019",
  caption="Stats19 data accessed via `stats19` package") 


ggsave(filename="./static/class/08-class_files/selected-uncertainty.png", plot=plot,width=6, height=5, dpi=300)



rate_boots_temporal <- ped_veh_fst %>%
  mutate(
    is_ksi=if_else(accident_severity=="Slight", FALSE, TRUE),
    year=lubridate::year(date)
  ) %>% 
  filter(local_authority_district %in% c("Bristol, City of", "Sheffield", "Bromsgrove", "Cotswold")
  ) %>%
  select(local_authority_district, is_ksi, year) %>%
  nest(-c(local_authority_district, year)) %>%
  mutate(la_boot = map(data, bootstraps, times=10, apparent=TRUE)) %>%
  select(-data) %>%
  unnest(la_boot) %>%
  mutate(
    is_ksi=map(splits, ~ analysis(.) %>% pull(is_ksi)),
    ksi_rate=map_dbl(is_ksi, ~mean(.x)),
    sample_size=map_dbl(is_ksi, ~length(.x))
  ) %>% 
  select(-c(splits, is_ksi)) 


rate_boots_temporal <- ped_veh_fst %>%
  mutate(
    is_ksi=if_else(accident_severity=="Slight", FALSE, TRUE),
    year=lubridate::year(date)
  ) %>% 
  filter(local_authority_district %in% c("Bristol, City of", "Sheffield", "Bromsgrove", "Cotswold")
  ) %>%
  select(local_authority_district, is_ksi, year) %>%
  nest(-c(local_authority_district, year)) %>%
  mutate(la_boot = map(data, bootstraps, times=50, apparent=TRUE)) %>%
  select(-data) %>%
  unnest(la_boot) %>%
  mutate(
    is_ksi=map(splits, ~ analysis(.) %>% pull(is_ksi)),
    ksi_rate=map_dbl(is_ksi, ~mean(.x)),
    sample_size=map_dbl(is_ksi, ~length(.x))
  ) %>% 
  select(-c(splits, is_ksi)) 





plot <- rate_boots_temporal %>% 
  mutate(
    year=as.character(year),
    local_authority_district=factor(
      local_authority_district, levels=c("Cotswold", "Sheffield", "Bromsgrove", "Bristol, City of")
      )
    ) %>% 
  ggplot(aes(x=year, y=ksi_rate)) +
  geom_line(
    data=. %>%  filter(id=="Apparent"), 
    aes(group=id), colour=site_colours$primary, size=1
    ) +
  geom_line(
    data=. %>%  filter(id!="Apparent"), 
    aes(group=id), colour=site_colours$primary, alpha=.03, size=.2
  ) +
  facet_wrap(~local_authority_district) +
  labs(
  title="Year-on-year KSI rates for pedestrian-vehicle crashes, bootstrap resamples superimposed ",
   subtitle="--Stats19 crashes 2019",
caption="Stats19 data accessed via `stats19` package")

ggsave(filename="./static/class/08-class_files/temporal-uncertainty.png", plot=plot,width=9, height=7, dpi=300)


hop <- rate_boots_temporal %>% 
  mutate(
    year=as.character(year),
    local_authority_district=factor(
      local_authority_district, levels=c("Cotswold", "Sheffield", "Bromsgrove", "Bristol, City of")
    )
  ) %>% 
  ggplot(aes(x=year, y=ksi_rate)) +
  geom_line(
    data=. %>% filter(id!="Apparent"), aes(group=id), colour=site_colours$primary, size=.3, alpha=.7
  ) +
  geom_line(
    data=. %>% filter(id=="Apparent") %>% mutate(id_apparent=id) %>% select(-id), aes(group=id_apparent), colour=site_colours$primary, size=.7
  ) + 
  facet_wrap(~local_authority_district) +
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank()
  ) +
  transition_states(id, 0,1)+
  labs(
    title="HOP of year-on-year KSI rates for pedestrian-vehicle crashes",
    subtitle="--Stats19 crashes 2010-2019",
    caption="Stats19 data accessed via `stats19` package")


animate(hop, duration=10, fps=5, start_pause=5, width=1100, height=1000, res=150, renderer=gifski_renderer("./static/class/08-class_files/temporal-uncertainty.gif"))

# Calculate national KSI rate
ksi_nat <- ped_veh_fst %>% 
  mutate(is_ksi=accident_severity!="Slight", year=lubridate::year(date)) %>% 
  filter(year==2019) %>% 
  summarise(ksi=mean(is_ksi)) %>% pull(ksi)

# Takes a few seconds to execute as generating and summarising over 380*100 resamples.
rr_boots_lad <- ped_veh_fst %>%
  mutate(
    is_ksi=accident_severity!="Slight",
    year=lubridate::year(date)
  ) %>% 
  filter(year==2019) %>%
  select(local_authority_district, is_ksi) %>%
  nest(-c(local_authority_district)) %>%
  mutate(la_boot = map(data, bootstraps, times=100, apparent=TRUE)) %>%
  select(-data) %>%
  unnest(la_boot) %>%
  mutate(
    is_ksi=map(splits, ~ analysis(.) %>% pull(is_ksi)),
    ksi_rate=map_dbl(is_ksi, ~mean(.x)),
    rr=ksi_rate/ksi_nat,
    sample_size=map_dbl(is_ksi, ~length(.x))
  ) %>% 
  select(-c(splits, is_ksi)) %>% 
  group_by(local_authority_district) %>% 
  mutate(lower=quantile(rr,probs=.05), upper=quantile(rr,probs=.95)) %>% 
  filter(id=="Apparent") %>% 
  mutate(is_sig=!between(1,lower, upper)) %>% 
  ungroup


# Plot at lad-level
# LAD boundaries
Local_Authority_Districts_December_2015_Super_Generalised_Clipped_Boundaries_in_Great_Britain.shp

url <- "/Users/roger/Dropbox (Personal)/-/git/multiple-comparison-corrections/data/boundaries_gb/Local_Authority_Districts_December_2015_Super_Generalised_Clipped_Boundaries_in_Great_Britain.shp"
lad_boundaries <- ms_simplify(lad_boundaries, keep=.2)

url <- "https://www.roger-beecham.com/datasets/lad_boundaries.geojson"

lad_boundaries <- st_read(url) %>% 
  st_transform(glad_boundaries, crs=27700)

# LAD centroids
lad_centroids <- lad_boundaries %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% rename("east"="X", "north"="Y")
# Add to df
lad_boundaries <- lad_boundaries %>%
  mutate(east=lad_centroids$east, north=lad_centroids$north)

# Convert degrees to radians.
get_radians <- function(degrees) {
  (degrees * pi) / (180)
}
# Rescaling function.
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}
# Position subclass for centred geom_spoke as per --
# https://stackoverflow.com/questions/55474143/how-to-center-geom-spoke-around-their-origin
position_center_spoke <- function() PositionCenterSpoke
PositionCenterSpoke <- ggplot2::ggproto('PositionCenterSpoke', ggplot2::Position,
                                        compute_panel = function(self, data, params, scales) {
                                          data$x <- 2*data$x - data$xend
                                          data$y <- 2*data$y - data$yend
                                          data$radius <- 2*data$radius
                                          data
                                        }
)


rr_boots_lad <- rr_boots_lad %>%
  mutate(
    # Variable for colouring according to sig type.
    sig_type = case_when(
      !is_sig ~ "non",
      rr > 1 ~ "greater",
      rr < 1 ~ "less"
    ),
    # Recode non-matcing lad names.
    local_authority_district=
      case_when(
        local_authority_district=="Edinburgh, City of" ~ "City of Edinburgh",
        local_authority_district=="St. Edmundsbury" ~ "St Edmundsbury",
        local_authority_district=="St. Albans" ~ "St Albans",
        local_authority_district=="Stratford-upon-Avon" ~ "Stratford-on-Avon",
        local_authority_district=="The Vale of Glamorgan" ~ "Vale of Glamorgan",
        local_authority_district=="Rhondda, Cynon, Taff" ~ "Rhondda Cynon Taf",
        TRUE ~ local_authority_district
      )
  )
  
max_rr <- max(rr_boots_lad %>% pull(rr))
min_rr <- min(rr_boots_lad %>% pull(rr))
plot <- lad_boundaries %>% 
  left_join(rr_boots_lad, by=c("lad15nm"="local_authority_district")) %>% 
  ggplot()+
  geom_sf(data=. %>% summarise(), colour="#636363", alpha=.5, size=.05)+
  geom_sf(data=. %>% filter(ksi_rate!=0), aes(fill=sig_type), alpha=.2, size=0)+
  # Greater 
  geom_spoke(
    data=. %>% filter(ksi_rate!=0, rr>1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr,1,max_rr,90,45)), colour=sig_type),
    radius=8000, position="center_spoke"
  )+
  # Less 
  geom_spoke(
    data=. %>% filter(ksi_rate!=0, rr<1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr,min_rr,1,135,90)), colour=sig_type),
    radius=8000, position="center_spoke"
  )+
  coord_sf(crs=27700, datum=NA)+
  scale_colour_manual(values=c("#99000d","#084594", "#bdbdbd")) +
  scale_fill_manual(values=c("#99000d","#084594", "#bdbdbd")) +
  theme(
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    legend.position="right"
  ) +
  labs(
    title="Risk Ratios comparing local authority KSI rates to the GB average",
    subtitle="--Stats19 crashes in 2019. Significance based on 90% bootstrap CI.",
    caption="Stats19 data accessed via `stats19` package")
  
ggsave(filename="./static/class/08-class_files/rrs.png", plot=plot,width=8, height=10, dpi=300)

  