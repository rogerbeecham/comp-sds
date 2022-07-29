# Filter in on central
# load in via mapshapr
# clip bbox=-0.136608,51.494141,-0.102619,51.514443 
# filter 'building != undefined || man_made == "bridge"'
# o 'buildings_filter.json' format=geojson drop-table

# filter 'leisure == "park"'
# o 'parks.json' format=geojson drop-table

# filter 'waterway == "river" && name == "River Thames"' 
# o 'rivers.json' format=geojson drop-table


# -0.136608,51.494141,-0.102619,51.514443

# p1 <- st_point(c(-0.136608,51.514443))
# p2 <- st_point(c(-0.102619,51.514443))
# p3 <- st_point(c(-0.102619,51.494141))
# p4 <- st_point(c(-0.136608,51.494141))
# p5 <- st_point(c(-0.136608,51.514443))

# p1 <- st_point(c(-0.136608,51.494141))
# p2 <- st_point(c(-0.136608,51.514443))
# p3 <- st_point(c(-0.102619,51.514443))
# p4 <- st_point(c(-0.102619,51.494141))
# box <- st_bbox(st_sf(geom = st_sfc(p1, p2, p3, p4), crs = 4326) %>%  st_transform(crs=27700))

filter_container <- st_sfc(st_polygon(list(rbind(p1, p2, p3, p4, p5))), crs=4326) %>% st_transform(crs=27700)

st_intersects(st_sfc(st_point(c(-0.126608,51.5)), crs=4326), filter_container)

            
od_trajectories_bezier_filter <- od_trajectories_bezier_filter %>% 


  
  st_sfc(st_point(c(easting,northing)), crs=27700)  
    
stations_lon %>% 
  filter(st_intersects(st_sfc(st_point(c(easting,northing)), crs=27700)))

buildings_filter <- st_read("./data/buildings_filter.json") %>%
  st_transform(crs=27700) 

stations_filter <- stations_lon %>% mutate(x=easting, y=northing) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(27700) %>% 
  st_filter(st_sf(filter_container))

od_trajectories_bezier_filter <- od_trajectories_bezier %>% group_by(od_pair) %>% 
  mutate(stage=row_number()) %>% filter(stage==3) %>% ungroup %>%
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(27700) %>% 
  st_filter(st_sf(filter_container))

od_trajectories_bezier_filter <- od_trajectories_bezier %>% 
  inner_join(od_trajectories_bezier_filter %>% st_drop_geometry() %>%  select(od_pair))
  
od_trajectories <- od_trajectories_bezier_filter %>% 
  inner_join(sample_lo %>% 
              mutate(od_pair=paste0(start_station_id,"-",end_station_id)) %>%
              group_by(od_pair) %>%
              summarise(count=n())) 

od_trajectories <- od_trajectories %>% mutate(f_od=((count/max(count))^0.7))

t <- stations_filter %>% inner_join(
  sample_lo %>%  group_by(end_station_id) %>% summarise(count=n()), by=c("stn_id"="end_station_id"))

filter_outline <- ggplot() +
  geom_sf(data=buildings_filter, fill="#d7ccc8",  colour="#d7ccc8")+
  geom_sf(data=parks_filter, fill="#a5d6a7",  colour="#a5d6a7")+
  geom_sf(data=rivers_filter, fill="#90caf9",  colour="#90caf9", size=12)+
  coord_sf(crs=st_crs(parks_filter), datum=NA)+
  geom_point(data=t, aes(x=easting,y=northing, size=count), pch=21, colour="#003c8f", fill="#003c8f", alpha=.5)+
  #ggforce::geom_bezier0(data=od_trajectories %>% filter(count>50), aes(x=x, y=y, group=od_pair, alpha=f_od, colour=f_od))+
  scale_size_continuous(range=c(5,25))+
  guides(colour=FALSE, alpha=FALSE, size=FALSE)+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  theme_void() 
ggsave(filter_outline, filename="./filter_outline.png", width=3, height=3, dpi=300)

d <- tibble(text=c("vis-for-gds"),
              x=c(0),
              y=c(2))
p <- d %>% ggplot()+
  geom_text(aes(x=x,y=y,label=text), size=6, angle=45, family="Avenir Book", colour="#ffffff")+
  theme_void() 

img<-magick::image_read("filter_outline.png")

s <- hexSticker::sticker(filter_outline, package="",
                         s_x=1, s_y=1, s_width = 2.3, s_height = 2.3,
                         h_color="#8e8e8e", h_fill="#EEEEEE", h_size=4,
                         white_around_sticker = FALSE) 
ggsave(s, filename="./vis-for-gds.png", width=10, height=10, dpi=300)

s <- hexSticker::sticker(filter_outline, package="",
                         s_x=1, s_y=1.38, s_width = 8.9, s_height = 8.9,
                         h_color="#616161", h_fill="#8e8e8e", h_size=4,
                         white_around_sticker = FALSE) 
ggsave(s, filename="./vis-for-gds.png", width=10, height=10, dpi=300)


filter_outline <- ggplot() +
  geom_sf(data=buildings_filter, fill="#bdbdbd",  colour="#bdbdbd")+
  geom_sf(data=parks_filter, fill="#bdbdbd",  colour="#bdbdbd")+
  geom_sf(data=rivers_filter, fill="#bdbdbd",  colour="#bdbdbd", size=15)+
  coord_sf(crs=st_crs(parks_filter), datum=NA)+
  geom_point(data=t %>% filter(id < 1248), aes(x=easting,y=northing, size=count), pch=21, colour="#002171", fill="#0d47a1", alpha=.7, stroke=2)+
  geom_point(data=t %>% filter(id > 1248), aes(x=easting,y=northing, size=count), pch=21, colour="#7f0000", fill="#b71c1c", alpha=.7, stroke=2)+
  #ggforce::geom_bezier0(data=od_trajectories %>% filter(count>50), aes(x=x, y=y, group=od_pair, alpha=f_od, colour=f_od))+
  #ggforce::geom_bezier0(data=od_trajectories %>% filter(count>100), aes(x=x, y=y, group=od_pair, alpha=f_od), colour="#001970")+
  scale_size_continuous(range=c(5,35))+
  guides(colour=FALSE, alpha=FALSE, size=FALSE)+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  theme_void() 

s <- hexSticker::sticker(filter_outline, package="",
                         s_x=1, s_y=1, s_width = 2.3, s_height = 2.3,
                         h_color="#8e8e8e", h_fill="#bdbdbd", h_size=0,
                         white_around_sticker = FALSE) 
ggsave(s, filename="./vis-for-gds.png", width=10, height=10, dpi=300)




s <- hexSticker::sticker(filter_outline, package="",
                         s_x=1, s_y=1, s_width = 2.3, s_height = 2.3,
                         h_color="#8e8e8e", h_fill="#eeeeee", h_size=0,
                         white_around_sticker = FALSE) 
ggsave(s, filename="./vis-for-gds.png", width=10, height=10, dpi=300)

#d7d0c8

s <- hexSticker::sticker(filter_outline, package="",
                         s_x=1, s_y=1.38, s_width = 8.9, s_height = 8.9,
                         h_color="#8e8e8e", h_fill="#bdbdbd", h_size=4,
                         white_around_sticker = FALSE) 
ggsave(s, filename="./vis-for-gds.png", width=10, height=10, dpi=300)




parks_filter <- st_crop(parks, box)
rivers_filter <- st_crop(rivers, box)
buildings_filter <- st_crop(buildings, box)
