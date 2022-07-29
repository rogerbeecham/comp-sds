

london_las <- pct_regions_lookup %>% filter(region_name=="london") %>% 
  pull(lad16nm) 


london_grid_real <- st_read("https://www.roger-beecham.com/datasets/london_grid_real.geojson")

edges <- od_pairs %>% 
  # Filter only *within* London.
  filter(la_1 %in% london_las, la_2 %in% london_las) %>% 
  group_by(la_1, la_2) %>% 
  summarise(
    across(c(all:other), sum)
  ) %>% 
  ungroup %>% 
  transmute(
    o_bor=la_1, d_bor=la_2, all=all,public_transport=train+bus+light_rail, 
    car=car_driver+car_passenger, active=bicycle+foot
  ) 


nodes_d <- od_pairs %>% 
  # Filter only *within* London.
  filter(la_1 %in% london_las, la_2 %in% london_las) %>% 
  group_by(la_2) %>% 
  summarise(
    across(c(all:other), sum)
  ) %>% 
  ungroup %>% 
  rename(bor = la_2) %>% 
  transmute(
    bor=bor, type="destination", all=all, public_transport=train+bus+light_rail, 
    car=car_driver+car_passenger, active=bicycle+foot
  ) 

nodes_o <- od_pairs %>% 
  # Filter only *within* London.
  filter(la_1 %in% london_las, la_2 %in% london_las) %>% 
  group_by(la_1) %>% 
  summarise(
    across(c(all:other), sum)
  ) %>% 
  ungroup %>% 
  rename(bor = la_1) %>% 
  transmute(
    bor=bor, type="origin", all=all, public_transport=train+bus+light_rail, 
    car=car_driver+car_passenger, active=bicycle+foot
  ) 

nodes  <- nodes_o %>% bind_rows(nodes_d) 

# pt #4daf4a - green, active #377eb8 - blue, car #e41a1c - red
# green - #006d2c, red - #a50f15, blue - #08519c


bor_orders <- nodes %>% 
  filter(type=="destination") %>%  
  arrange(-all) %>% pull(bor)

bars <- nodes %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(bor=factor(bor, levels=bor_orders)) %>% 
  ggplot() +
  geom_col(aes(x=bor, y=count, fill=mode)) +
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  guides(fill=FALSE)+
  labs(x="", y="count") +
  facet_wrap(~type, ncol=2)


bars <- nodes %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(bor=factor(bor, levels=bor_orders)) %>% 
  ggplot() +
  geom_col(aes(x=bor, y=count, fill=mode)) +
  #scale_fill_manual(values=c("#377eb8","#e41a1c", "#4daf4a"))+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  guides(fill=FALSE)+
  labs(x="", y="count") +
  facet_wrap(~type, ncol=2)+
  theme(axis.text.x = element_text(angle=90))

bars_filled <- nodes %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(bor=factor(bor, levels=bor_orders)) %>% 
  ggplot() +
  geom_col(aes(x=bor, y=count, fill=mode), position="fill") +
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  labs(x="", y="count") +
  facet_wrap(~type, nrow=2)+
  theme(legend.position="right",
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.y = element_blank(), axis.title.x = element_blank()) +
  coord_flip()

bars_filled <- nodes %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(bor=factor(bor, levels=bor_orders)) %>% 
  ggplot() +
  geom_col(aes(x=bor, y=count, fill=mode), position="fill") +
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  labs(x="", y="count") +
  facet_wrap(~type, ncol=2)+
  theme(
    axis.text.x = element_blank(), axis.text.y = element_blank(), 
    axis.title.y = element_blank(), axis.title.x = element_blank()) 

plot <- bars + bars_filled + (dest_plot/origin_plot)+ plot_layout(widths=c(.3,.25,.45)) +
  plot_annotation(
    title="Frequencies of commutes into- (destination) and out of- (origin) London boroughs by travel mode",
    subtitle="--Bars, filled bars (showing proportion by mode) and relaxed spatial arrangement",
    caption="2011 Census data accessed via `pct` package"
  )

dest_origin_plot <- nodes %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% filter(type=="grid") %>% 
              select(authority, BOR,x,y),by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(type_abbr=str_sub(type,1,1)) %>% 
  group_by(bor) %>%
  mutate(bor_total=sum(count)) %>% 
  group_by(bor,type) %>% 
  mutate(type_total=sum(count)) %>% 
  ggplot() +
  geom_col(aes(x=type_abbr, y=count/bor_total, fill=mode))+
  geom_text(data=. %>% filter(mode=="active", type=="destination"), aes(x=1.5, y=1, label=BOR), vjust="top", hjust="centre")+
  geom_text(data=. %>% filter(mode=="active", type=="destination"), aes(x=1.4, y=.85, label="dest"), vjust="top", hjust="right", size=3, family="Roboto Light")+
  geom_text(data=. %>% filter(mode=="active", type=="origin"), aes(x=1.6, y=.85, label="origin"), vjust="top", hjust="left", size=3, family="Roboto Light")+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  scale_alpha_continuous(range=c(.4,1))+
  facet_grid(y~x)+
  guides(fill=FALSE, alpha=FALSE)+
  theme(
    panel.spacing.y=unit(.2, "lines"), panel.spacing.x=unit(.2, "lines"), 
    panel.background = element_rect(fill="#ffffff", colour="#ffffff"),
    axis.title.x = element_blank(),axis.title.y = element_blank(),
    axis.text.x=element_blank(), axis.text.y = element_blank(),
    strip.text.x=element_blank(), strip.text.y = element_blank(),
    panel.grid=element_blank(),
  )


nodes %>% 
  left_join(london_grid_real %>% st_drop_geometry() %>% filter(type=="grid") %>% 
              select(authority, BOR,x,y),by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  group_by(bor) %>%
  mutate(bor_total=sum(count)) %>% 
  ggplot() +
  geom_col(aes(x=type, y=count/bor_total, fill=mode))+
  geom_text(data=. %>% filter(mode=="active", type=="destination"), aes(x=1.5, y=1, label=BOR), vjust="top", hjust="centre")+
  geom_text(data=. %>% filter(mode=="active", type=="destination"), aes(x=1.4, y=.85, label="dest"), vjust="top", hjust="right", size=3)+
  geom_text(data=. %>% filter(mode=="active", type=="origin"), aes(x=1.6, y=.85, label="origin"), vjust="top", hjust="left", size=3)+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  facet_grid(y~x)+
  guides(fill=FALSE, alpha=FALSE)+
  theme(
    panel.spacing.y=unit(.2, "lines"), panel.spacing.x=unit(.2, "lines"), 
    panel.background = element_rect(fill="#ffffff", colour="#ffffff"),
    axis.title.x = element_blank(),axis.title.y = element_blank(),
    axis.text.x=element_blank(), axis.text.y = element_blank(),
    strip.text.x=element_blank(), strip.text.y = element_blank(),
    panel.grid=element_blank()
  )



dest_plot <- nodes %>% 
  filter(type=="destination") %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% filter(type=="grid") %>% 
              select(authority, BOR,x,y),by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  group_by(bor,type) %>% 
  mutate(total_commutes=sum(count)) %>% 
  ggplot() +
  geom_col(aes(x=0, y=count, fill=mode, alpha=total_commutes), position="fill")+
  geom_text(data=. %>% filter(mode=="active"), aes(x=0, y=.5, label=BOR))+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  scale_alpha_continuous(range=c(.4,1))+
  facet_grid(y~x)+
  labs(subtitle="destination")+
  guides(fill=FALSE, alpha=FALSE)+
  theme(
    panel.spacing.y=unit(-.1, "lines"), panel.spacing.x=unit(-.2, "lines"), 
    axis.title.x = element_blank(),axis.title.y = element_blank(),
    axis.text.x=element_blank(), axis.text.y = element_blank(),
    strip.text.x=element_blank(), strip.text.y = element_blank(),
    panel.grid=element_blank()
  )

origin_plot <- nodes %>% 
  filter(type=="origin") %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% filter(type=="grid") %>% 
              select(authority, BOR,x,y),by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  group_by(bor,type) %>% 
  mutate(total_commutes=sum(count)) %>% 
  ggplot() +
  geom_col(aes(x=0, y=count, fill=mode, alpha=total_commutes), position="fill")+
  geom_text(data=. %>% filter(mode=="active"), aes(x=0, y=.5, label=BOR))+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  scale_alpha_continuous(range=c(.4,1))+
  guides(fill=FALSE, alpha=FALSE)+
  labs(subtitle="origin")+
  facet_grid(y~x)+
  theme(
    panel.spacing.y=unit(-.1, "lines"), panel.spacing.x=unit(-.2, "lines"), 
    axis.title.x = element_blank(),axis.title.y = element_blank(),
    axis.text.x=element_blank(), axis.text.y = element_blank(),
    strip.text.x=element_blank(), strip.text.y = element_blank(),
    panel.grid=element_blank()
  )

ggsave(filename="./static/class/05-class_files/mode-plots.png", plot=plot,width=16, height=12, dpi=300)

plot <- bars / bars_filled / (dest_plot|origin_plot)+ plot_layout(heights=c(.35,.15,.5)) +
  plot_annotation(
    title="Frequencies of commutes into- (destination) and out of- (origin) London boroughs by travel mode",
    subtitle="--Bars, filled bars (showing proportion by mode) and relaxed spatial arrangement",
    caption="2011 Census data accessed via `pct` package"
  )

ggsave(filename="./static/class/05-class_files/mode-plots.png", plot=plot,width=12, height=13, dpi=300)




layout <- "
AAAAAAAA
BBBBBBBB
#CCCCCC#"


plot <- bars + bars_filled + dest_origin_plot + plot_layout(design = layout, heights=c(.2,.1,.7)) +
  plot_annotation(
    title="Frequencies of commutes into- (destination) and out of- (origin) London boroughs by travel mode",
    subtitle="--Bars, filled bars (showing proportion by mode) and relaxed spatial arrangement",
    caption="2011 Census data accessed via `pct` package"
  )

ggsave(filename="./static/class/05-class_files/mode-plots.png", plot=plot,width=12, height=13, dpi=300)



# Temporary plot object of data joined to geom_sf geometries. 
# DO map so geometries join on origin.
plot_data_temp <- grid_real_sf %>% filter(type=="grid") %>% right_join(edges, by=c("authority"="o_bor")) %>% 
  mutate(o_bor=authority) %>% 
  rename(o_fx=x, o_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("d_bor"="authority")) %>%
  rename(d_fx=x, d_fy=y) %>% 
  # Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
  mutate(bor_label=if_else(o_bor==d_bor,d_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))

# width
width <- plot_data_temp %>% summarise(width=max(east)-min(east))  %>% pull(width)
# height
height <- plot_data_temp %>% summarise(height=max(north)-min(north)) %>% pull(height)

bbox_grid <- st_bbox(grid_real_sf %>% filter(type=="grid"))

plot_data_temp %>% mutate(count=public_transport) %>% 
  ggplot()+
  geom_sf(aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=. %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=. %>% filter(bor_focus==1),  aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=1.0, size=2, show.legend=FALSE, hjust="centre", vjust="middle")+
  geom_text(data=. %>% filter(bor_focus==1), aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )



d_od_global_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=1.0, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )




# Temporary plot object of data joined to geom_sf geometries. DO map so geometries join on origin.
plot_data_temp <- grid_real_sf %>% filter(type=="grid") %>% right_join(edges, by=c("authority"="o_bor")) %>% 
  mutate(o_bor=authority) %>% 
  rename(o_fx=x, o_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("d_bor"="authority")) %>%
  rename(d_fx=x, d_fy=y) 

# Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp %>% mutate(bor_label=if_else(o_bor==d_bor,d_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))

# width
width <- plot_data_temp %>% summarise(width=max(east)-min(east))  %>% pull(width)
# height
height <- plot_data_temp %>% summarise(height=max(north)-min(north)) %>% pull(height)

#373737
#ffffff
bbox_grid <- st_bbox(grid_real_sf %>% filter(type=="grid"))



d_od_global_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport),# count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=1.0, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


d_od_global_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active), #count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_global_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car),# count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_local_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            group_by(d_bor) %>% 
            mutate(count=public_transport,# count=if_else(o_bor==d_bor,0,count),
                   count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


d_od_global_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active), #count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_local_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active) %>% # count=if_else(o_bor==d_bor,0,count)) %>% 
            group_by(d_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), aes(fill=count_rescaled), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_global_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car),# count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )



d_od_local_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car)#, count=if_else(o_bor==d_bor,0,count)) 
          %>%  group_by(d_bor) %>% 
            group_by(d_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

# Temporary plot object of data joined to geom_sf geometries. DO map so geometries join on origin.
plot_data_temp <- grid_real_sf %>% filter(type=="grid") %>% right_join(edges, by=c("authority"="d_bor")) %>% 
  mutate(d_bor=authority) %>% 
  rename(d_fx=x, d_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("o_bor"="authority")) %>%
  rename(o_fx=x, o_fy=y) 

# Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp %>% mutate(bor_label=if_else(o_bor==d_bor,o_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))



o_od_global_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport),#, count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


o_od_local_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport, count=if_else(o_bor==d_bor,0,count)) %>% 
            group_by(o_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


o_od_global_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active), # count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

o_od_local_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active) %>%  #, count=if_else(o_bor==d_bor,0,count)) %>% 
            group_by(o_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )



o_od_global_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car), #count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

o_od_local_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car) %>%  #, count=if_else(o_bor==d_bor,0,count)) %>% 
            group_by(o_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))),
          aes(fill=count_rescaled), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#fffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

plot <- 
  (d_od_global_pt | o_od_global_pt)/
  (d_od_global_active | o_od_global_active)/
  (d_od_global_car | o_od_global_car) +
  plot_annotation(
    title="OD maps of commutes into- and out of- London boroughs separately by travel mode: Global colour scaling is used",
    subtitle="--The left column focuses on destinations (D-OD map); the right on origins (O-OD map)",
    caption="2011 Census data accessed via `pct` package"
  )

ggsave(filename="./static/class/05-class_files/od-maps-mode.png", plot=plot,width=10, height=13, dpi=300)

plot <- 
  (d_od_global_pt | o_od_global_pt) /
  (d_od_local_pt | o_od_local_pt) +
  plot_annotation(
    title="OD maps of commutes by public transport",
    subtitle="--Left focuses on destinations (D-OD map); right on origins (O-OD map) | Top row global scaling; Bottom row local scaling",
    caption="2011 Census data accessed via `pct` package"
  )

ggsave(filename="./static/class/05-class_files/od-maps-mode.png", plot=plot,width=11, height=10.4, dpi=300)


plot <- 
  (d_od_local_pt | o_od_local_pt)/
  (d_od_local_active | o_od_local_active)/
  (d_od_local_car | o_od_local_car) +
  plot_annotation(
    title="OD maps of commutes into- and out of- London boroughs separately by travel mode: Local colour scaling is used",
    subtitle="--The left column focuses on destinations (D-OD map); the right on origins (O-OD map)",
    caption="2011 Census data accessed via `pct` package"
  )


ggsave(filename="./static/class/05-class_files/od-maps-mode-local.png", plot=plot,width=10, height=13, dpi=300)

ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car, count=if_else(o_bor==d_bor,0,count)), aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="global scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active, count=if_else(o_bor==d_bor,0,count)), aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="global scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_global_local <- plot_data_temp %>%
  group_by(d_bor) %>% 
  mutate(
    count=public_transport, count=if_else(o_bor==d_bor,0,count), 
    count_rescaled=(count-min(count))/(max(count)-min(count))) %>% ungroup %>% 
  ggplot()+
  geom_sf(aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="local scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


plot_data_temp %>%
  group_by(d_bor) %>% 
  mutate(
    count=car, count=if_else(o_bor==d_bor,0,count), 
    count_rescaled=(count-min(count))/(max(count)-min(count))) %>% ungroup %>% 
  ggplot()+
  geom_sf(aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="local scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


plot_data_temp %>%
  group_by(d_bor) %>% 
  mutate(
    count=active, count=if_else(o_bor==d_bor,0,count), 
    count_rescaled=(count-min(count))/(max(count)-min(count))) %>% ungroup %>% 
  ggplot()+
  geom_sf(aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="local scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )