###############################################################################
# S E S S I O N  6
###############################################################################

# install.packages("parlitools")
library(parlitools)
install.packages("tidymodels")

# Contituency boundaries -- simplified using mapshapr From -- https://geoportal.statistics.gov.uk/
url <- "https://www.roger-beecham.com/datasets/cons_outline.geojson"
cons_outline <- st_read(url, crs=27700)

explanatory <- census_11 %>% 
  transmute(
    ons_const_id=ons_const_id, constituency_name=constituency_name, region=region,
    population=population, population_density=population_density,
    younger=age_20_to_24+age_25_to_29+age_30_to_44,
    own_home=house_owned,
    no_car=cars_none, white=ethnicity_white_british+ethnicity_white_irish,eu_born=born_other_eu, christian,
    professional=nssechigher_manager+nssechigher_professional, degree=qual_level_4,
    not_good_health=health_fair+health_bad+health_very_bad, heavy_industry=industry_manufacturing+industry_transport
    )
outcome <- leave_votes_west %>% 
  select(ons_const_id, constituency_name, leave=figure_to_use) %>% 
  inner_join(explanatory %>% select(ons_const_id, region)) %>% 
  mutate(resid_unform=leave-gb_leave)

max_resid <-max(abs(outcome$resid_unform))
map <- cons_outline %>%
  inner_join(outcome, by=c("pcon19cd"="ons_const_id")) %>%
  ggplot() +
  geom_sf(aes(fill=leave-gb_leave), colour="#757575", size=0.01)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#757575", fill="transparent", size=0.08)+
  coord_sf(crs=27700, datum=NA) +
  theme(legend.position = "right") +
  scale_fill_distiller(palette="RdBu", direction=1, 
                       limits=c(-max_resid, max_resid), guide=FALSE)


bars <- outcome %>%
  ggplot(aes(x=reorder(constituency_name,-leave), y=resid_unform, fill=resid_unform))+
  geom_col(width = 1)+
  scale_fill_distiller(palette = "RdBu", type="div", direction=1, 
                       limits=c(-max_resid, max_resid), guide=FALSE)+
  scale_x_discrete(breaks=c("Hackney North and Stoke Newington","East Worthing and Shoreham","Boston and Skegness"), labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(limits=c(-max_resid, max_resid))+
  geom_hline(aes(yintercept=0), colour="#757575")+
  labs(x="Constituencies by Leave", y="GB 52% Leave")+
  coord_flip()


layout <- "
###BBBBBBB
###BBBBBBB
AAABBBBBBB
AAABBBBBBB
AAABBBBBBB
AAABBBBBBB
AAABBBBBBB
AAABBBBBBB
AAABBBBBBB
AAABBBBBBB"

plot <-  bars+map + plot_layout(design = layout) + 
  plot_annotation(
    title="Residuals from uniform model for GB",
    subtitle="--Assumes each constituency votes in same proportion as GB.",
    caption="Data published by House of Commons Library, accessed via `parlitools`")

ggsave(filename="./static/class/06-class_files/map-uniform.png", plot=plot,width=7, height=7, dpi=300)


explanatory %>% select(-c(population, population_density)) %>% 
  inner_join(outcome %>% select(ons_const_id, leave, region)) %>% 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) %>% 
pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="prop") %>% 
  ggplot(aes(prop))+
  geom_histogram()+
  facet_wrap(~expl_var)


order_vars <- 
  explanatory %>% select(-c(population, population_density)) %>% 
  inner_join(outcome %>% select(ons_const_id, leave, region)) %>% 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) %>% 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="prop") %>% 
  group_by(expl_var) %>% 
  summarise(cor=cor(leave,prop)) %>% ungroup %>%  arrange(cor) %>% 
  pull(expl_var)

scatters <- explanatory %>% select(-c(population, population_density)) %>% 
  inner_join(outcome %>% select(ons_const_id, leave, region)) %>% 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) %>% 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="prop") %>% 
  group_by(expl_var) %>% 
  mutate(cor=cor(leave,prop)) %>% ungroup() %>% 
  mutate(expl_var=factor(expl_var, levels=order_vars)) %>%  
  ggplot(aes(y=leave,x=prop))+
  geom_point(alpha=.3, colour=site_colours$primary) +
  #geom_smooth(method = "lm", se = FALSE, colour=site_colours$primary) +
  geom_text(data=. %>% filter(constituency_name=="Aberavon"), aes(y=.8,x=4.5, label=paste0("cor: ",round(cor,2))), size=3)+
  facet_wrap(~expl_var) +
  labs(x="z-score", y="share of leave", 
       title="Scatterplots of share of leave vote by candidate explanatory variable",
       subtitle="--Constituencies of GB.",
       caption="Data published by House of Commons Library, accessed via `parlitools`"
       )


ggsave(filename="./static/class/06-class_files/scatters.png", plot=scatters,width=10, height=7, dpi=300)


plot <-  bars+map + plot_layout(design = layout) + 
  plot_annotation(
    title="Residuals from uniform model for GB",
    subtitle="--Assumes each constituency votes in same proportion as GB.",
    caption="Data published by House of Commons Library, accessed via `parlitools`")

ggsave(filename="./static/class/06-class_files/map-uniform.png", plot=plot,width=7, height=7, dpi=300)
  


# fitted models, used in last section
single_model_fits <- explanatory %>% select(-c(population, population_density)) %>% 
  inner_join(outcome %>% select(ons_const_id, leave, region)) %>% 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) %>% 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="z_score") %>% 
  nest(data=-expl_var) %>%  # nest to generate list-column by expl_var. 
  mutate(
    # Use map() to iterate over the list of datasets to fit a model to each nested dataset.
    fit = map(data, ~lm(leave ~ z_score, data = .x)),
    # use map to apply glance() to each model fit
    outputs = map(fit, glance)
  ) %>%
  unnest(cols = outputs) %>% # unnest output from glance
  select(-data, -fit)           # remove columns data and fit
  
  
  penguins %>%
  nest(data = -species) %>%     # nest the data table by species
  mutate(
    # use map() to fit a model to each nested data table
    fit = map(data, ~lm(bill_length_mm ~ body_mass_g, data = .x)),
    # use map to apply glance() to each model fit
    glance_out = map(fit, glance)
  ) %>%
  unnest(cols = glance_out) %>% # unnest output from glance
  select(-data, -fit)           # remove columns data and fit

  
scatters <- explanatory %>% select(-c(population, population_density)) %>% 
    inner_join(outcome %>% select(ons_const_id, leave, region)) %>% 
    mutate(
      across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
    ) %>% 
    pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="prop") %>% 
    group_by(expl_var) %>% 
    mutate(cor=cor(leave,prop)) %>% ungroup() %>% 
    mutate(expl_var=factor(expl_var, levels=order_vars)) %>%  
    ggplot(aes(y=leave,x=prop))+
    geom_point(alpha=.3, colour=site_colours$primary) +
    #geom_smooth(method = "lm", se = FALSE, colour=site_colours$primary) +
    geom_text(data=. %>% filter(constituency_name=="Aberavon"), aes(y=.8,x=4.5, label=paste0("cor: ",round(cor,2))), size=3)+
    facet_wrap(~expl_var) +
    labs(x="z-score", y="share of leave", 
         title="Scatterplots of share of leave vote by candidate explanatory variable",
         subtitle="--Constituencies of GB.",
         caption="Data published by House of Commons Library, accessed via `parlitools`"
    )  
  
  



vif(
  lm(leave ~
       degree + professional + younger + eu_born + 
       no_car + white + own_home + 
       christian + not_good_health + heavy_industry, data = outcome %>% inner_join(explanatory) )
)



vif(
  lm(leave ~
       degree +  eu_born + 
       no_car + white + 
       christian + not_good_health + heavy_industry, data = outcome %>% inner_join(explanatory) )
)


summary(lm(leave ~
             degree  + eu_born + white  + no_car +
             christian + not_good_health + heavy_industry, 
           data = outcome %>% inner_join(explanatory)))


######## PCP #########

# Majority variable
brexit <- brexit %>% mutate(majority=ifelse(margin_leave>0, "leave", "remain"))

# Break into quintiles : describe most Leave and most Remain areas.
temp <- brexit %>% 
  mutate(rank_leave=row_number(share_leave),
         quintile_leave=ntile(share_leave,5),
         quintile_leave=recode_factor(quintile_leave, `1` = "most Remain", `2` = "quite Remain", `3` = "ambivalent", `4` = "quite Leave",  `5` = "most Leave")) %>%
  group_by(quintile_leave) %>%
  mutate(min_point=round(min(rank_leave)),
         mid_point=round(median(rank_leave)),
         max_point=round(max(rank_leave))) %>%
  ungroup()
st_geometry(temp) <- NULL

degree_educated  trans 
income           trans
prof_fin_info    trans
manufacturing 
rural_resources  
foreign_born    trans
older_adults    trans
net_inflow      trans
rural           
urban           trans

# Z-score for LAs on key variables for PCP. 
temp <- temp %>% 
  #filter(lad15nm!="City of London") %>% # city is a massive income outlier so remove this
  # ceiling on household income set to c.99th percentile
  group_by(lad15nm) %>%
  mutate(household_income=min(household_income,42000)) %>%
  ungroup() %>%
  mutate(manufacturing_jobs_loss=manufacturing_1971-manufacturing_share) %>%
  rename(income=household_income, 
         knowledge_jobs=professional_finance_information_share,
         net_inflow=net_inflow_share,
         urban=large_urban) %>%
  mutate_at(vars(share_leave, degree_educated, income, knowledge_jobs, manufacturing_jobs_loss, older_adults,
                 net_inflow, urban, rural),
            funs(zscore=(.-mean(.))/sd(.) )) %>% 
  mutate(degree_educated_zscore=-degree_educated_zscore,
         income_zscore=-income_zscore,
         knowledge_jobs_zscore=-knowledge_jobs_zscore,
         net_inflow_zscore=-net_inflow_zscore,
         urban_zscore=-urban_zscore) %>%
  select(lad15nm, Region, majority, share_leave, rank_leave, share_leave_zscore:rural_zscore, quintile_leave, mid_point, min_point, max_point, quintile_leave) %>%
  gather(c(share_leave_zscore:rural_zscore), key = "demographic", value="zscore", -rank_leave) %>%
  mutate(demographic=as_factor(demographic),
         demographic=gsub("\\_", " ", demographic),
         demographic=gsub("\\szscore","",demographic),
         demographic=forcats::fct_relevel(demographic,c(
           "rural",
           "urban",
           "net inflow",
           "older adults",
           "manufacturing jobs loss",
           "knowledge jobs",
           "income",
           "degree educated",
           "share leave")))


majority_colours <- c("#8c510a","#01665e")

pcp_brexit <-
  ggplot()+
  geom_path(data=subset(temp, rank_leave>20 & rank_leave<361), 
            aes(x=demographic, y=zscore, group=lad15nm), colour="#525252", alpha=0.05)+
  geom_path(data=subset(temp, rank_leave<21 | rank_leave>360), 
            aes(x=demographic, y=zscore, group=lad15nm, colour=majority), alpha=0.3)+
  geom_path(data=subset(temp, rank_leave==1 | rank_leave==380), 
            aes(x=demographic, y=zscore, group=lad15nm, colour=majority), alpha=0.6)+
  geom_text(data=subset(temp, demographic=="share leave" & (rank_leave==1 | rank_leave==380)),
            aes(x=demographic, y=zscore, label=lad15nm, colour=majority), angle=0, size=2.5,hjust = 0.5, vjust=-0.5)+
  scale_colour_manual(values=majority_colours, guide="none")+
  coord_flip()
pcp_brexit <- pcp_brexit +
  theme_ipsum_rc()+
  labs( 
    x="",
    title="Parallel coordinate plots of Local Authority demographics ",
    subtitle="The top 20 LAs for Remain | Leave are highlighed.",
    caption="Data from 2011 Census.")+
  theme(plot.title = element_text(face="plain", size=16))

ggsave("./figures/pcp_brexit.png",plot = pcp_brexit, width=30, height=18, units="cm")




+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6))






max_shift <- max(abs(data_gb$swing_con_lab))
min_shift <- -max_shift
# Calculate bounding boxes for use in annotation_custom().
london_bbox <- st_bbox(data_gb %>% filter(region=="London"))
london_width <- unname(london_bbox$xmax)-unname(london_bbox$xmin) 
london_height <- unname(london_bbox$ymax)-unname(london_bbox$ymin) 
london_aspect <- london_width/london_height
uk_bbox <- st_bbox(data_gb)
uk_width <- unname(uk_bbox$xmax)-unname(uk_bbox$xmin) 
uk_height <- unname(uk_bbox$ymax)-unname(uk_bbox$ymin) 

# Annotate constituencies that *really* defied expectation (discussed in the EPA paper).
bassetlaw <- data_gb %>% filter(constituency_name == "Bassetlaw") 
redcar <-  data_gb %>% filter(constituency_name == "Redcar")
sedgefield <- data_gb %>% filter(constituency_name == "Sedgefield")
stoke <- data_gb %>% filter(constituency_name == "Stoke-On-Trent Central")

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


party_colours <- c(con, lab, other)
names(party_colours) <- c("Conservative", "Labour", "Other")

gb <- data_gb %>%
  filter(region!="London") %>%
  mutate(is_flipped=seat_change_1719 %in% c("Conservative gain from Labour","Labour gain from Conservative"),
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         elected=if_else(!winner_19 %in% c("Conservative", "Labour"), "Other", as.character(winner_19))
  ) %>% 
  ggplot()+
  geom_sf(aes(fill=elected), colour="#636363", alpha=0.2, size=0.01)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#eeeeee", fill="transparent", size=0.08)+
  coord_sf(crs=27700, datum=NA, 
           xlim = c(unname(uk_bbox$xmin), unname(uk_bbox$xmax)+6*london_width), 
           ylim = c(unname(uk_bbox$ymin), unname(uk_bbox$ymax)-0.22*uk_height)
  )+
  geom_spoke(
    aes(x=bng_e, y=bng_n, angle=get_radians(map_scale(swing_con_lab,min_shift,max_shift,135,45)), colour=elected, size=is_flipped), 
    radius=7000, position="center_spoke", lineend="round"
  )+
  scale_size_ordinal(range=c(.3,.9))+
  scale_colour_manual(values=party_colours)+
  scale_fill_manual(values=party_colours)+
  annotate(geom="segment", xend=bassetlaw$bng_e, yend=bassetlaw$bng_n, x=bassetlaw$bng_e+0.15*uk_width, y=bassetlaw$bng_n, size=.2)+
  annotate(geom="text", x=bassetlaw$bng_e+0.16*uk_width, y=bassetlaw$bng_n, hjust="left", label=paste0(bassetlaw$constituency_name), family="Roboto Condensed", size=3.5)+ 
  annotate(geom="segment", xend=sedgefield$bng_e, yend=sedgefield$bng_n, x=sedgefield$bng_e+0.06*uk_width, y=sedgefield$bng_n+0.02*uk_height, size=.2)+
  annotate(geom="text", x=sedgefield$bng_e+0.07*uk_width, y=sedgefield$bng_n+0.02*uk_height, hjust="left", label=paste0(sedgefield$constituency_name), family="Roboto Condensed", size=3.5)+ 
  annotate(geom="segment", xend=redcar$bng_e, yend=redcar$bng_n, x=redcar$bng_e+0.05*uk_width, y=redcar$bng_n, size=.2)+
  annotate(geom="text", x=redcar$bng_e+0.06*uk_width, y=redcar$bng_n, hjust="left", label=paste0(redcar$constituency_name), family="Roboto Condensed", size=3.5)+ 
  annotate(geom="segment", xend=stoke$bng_e, yend=stoke$bng_n, x=stoke$bng_e-.15*uk_width, y=stoke$bng_n+0.05*uk_height, size=.2)+
  annotate(geom="text", x=stoke$bng_e-0.16*uk_width, y=stoke$bng_n+0.05*uk_height, hjust="right", label=paste0(stoke$constituency_name), family="Roboto Condensed", size=3.5)+
  guides(colour=FALSE, fill=FALSE, size=FALSE)+
  theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank())

data_gb %>%
  mutate(
    is_flipped=seat_change_1719 %in% c("Conservative gain from Labour","Labour gain from Conservative"),
    elected=if_else(!winner_19 %in% c("Conservative", "Labour"), "Other", as.character(winner_19))
  ) %>%
  ggplot()+
  geom_sf(aes(fill=elected), colour="#636363", alpha=0.2, size=0.01)+
  geom_spoke(
    aes(x=bng_e, y=bng_n, angle=get_radians(map_scale(swing_con_lab,min_shift,max_shift,135,45)), colour=elected, size=is_flipped),
    radius=7000, position="center_spoke", lineend="round"
  )+
  coord_sf(crs=27700, datum=NA)+
  scale_size_ordinal(range=c(.3,.9))+
  scale_colour_manual(values=party_colours)+
  scale_fill_manual(values=party_colours)+
  theme_v_gds() 

london <- data_gb %>%
  filter(region=="London") %>%
  mutate(is_flipped=seat_change_1719 %in% c("Conservative gain from Labour","Labour gain from Conservative"),
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         elected=if_else(!winner_19 %in% c("Conservative", "Labour"), "Other", as.character(winner_19))
  ) %>% 
  ggplot()+
  geom_sf(aes(fill=elected), colour="#636363", alpha=0.2, size=0.01)+
  coord_sf(datum=NA)+
  geom_spoke(
    aes(x=bng_e, y=bng_n, angle=get_radians(map_scale(swing_con_lab,min_shift,max_shift,135,45)), colour=elected, size=is_flipped), 
    radius=7000/5, position="center_spoke", lineend="round"
  )+
  scale_size_ordinal(range=c(.3,.9))+
  scale_colour_manual(values=party_colours)+
  scale_fill_manual(values=party_colours)+
  guides(colour=FALSE, fill=FALSE, size=FALSE)+
  theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank())



# Use of angle to encode swing.
swing <-  ggplot()+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(90)),radius=0.55, size=0.2, colour="#636363", lineend="round")+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(135)),radius=0.55, size=0.2,colour="#636363", linetype = "dashed", lineend="round")+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(45)),radius=0.55,size=0.2,colour="#636363",linetype = "dashed", lineend="round")+
  geom_text(aes(label="+18% to \n Con",x=.5, y=0), angle=45,hjust="right", family="Roboto Condensed", size=3, colour="#636363")+
  geom_text(aes(label="+18% to \n Lab",x=-.5, y=0), angle=315,hjust="left", family="Roboto Condensed", size=3, colour="#636363")+
  geom_curve(aes(x=-.04, y=.2, xend=-.3, yend=.08), size=0.3, curvature = 0.2, arrow=arrow(type="closed", length = unit(.03, "inches")), colour="#636363")+
  geom_curve(aes(x=.04, y=.2, xend=.3, yend=.08), size=0.3, curvature = -0.2, arrow=arrow(type="closed", length = unit(.03, "inches")), colour="#636363")+
  xlim(-0.5,0.5)+
  ylim(-0.35,0.35)+
  coord_equal() +
  theme_void()
theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank(), axis.text = element_blank())
# Use colour to encode party.
temp_dat <-tibble(
  elected=names(party_colours),
  y=c(3,2,1),
  x=c(1,1,1)
) 

# Use thickness to show flips.
line <-  ggplot()+
  geom_spoke(aes(x=-0.2, y=-.35,angle=get_radians(90)),radius=0.55, size=0.2, lineend="round")+
  geom_spoke(aes(x=0.2, y=-.35,angle=get_radians(90)),radius=0.55, size=0.8, lineend="round")+
  xlim(-0.5,0.5)+
  ylim(-0.35,0.35)+
  theme_void()


theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank(), axis.text = element_blank())

# Party colours for legend
party <- temp_dat %>%
  ggplot()+
  geom_spoke(aes(x=x, y=y,angle=get_radians(90), colour=elected),radius=0.7, size=1, lineend="round")+
  scale_colour_manual(values=party_colours)+
  geom_text(aes(label=elected,x=x+0.03, y=y+0.2),hjust="left",vjust="middle", family="Roboto Condensed", size=4, colour="#636363")+
  guides(colour=FALSE)+
  xlim(1,5)+
  ylim(-4,4)+
  theme_void()
theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank(), axis.text = element_blank())


# Use annotation_custom to organise grobs in legend.
legend <- ggplot()+
  geom_text(aes(label="Each constituency is a line -- Butler Con-Lab swing",x=0, y=6), hjust="left", vjust="top", family="Roboto Condensed", size=5)+
  geom_text(aes(label="Colour hue -- winning party",x=0, y=5), hjust="left", vjust="top", family="Roboto Condensed", size=4)+
  geom_text(aes(label="Thick line -- \n constituency flipped \n winning party \n from 2017",x=4.5, y=5), hjust="left", vjust="top", family="Roboto Condensed", size=4)+
  geom_text(aes(label="Line angle -- \n Butler % swing \n in vote share \n from 2017 -- \n Con-Lab",x=4.5, y=2.5), hjust="left", vjust="top", family="Roboto Condensed", size=4)+
  annotation_custom(grob=ggplotGrob(swing),xmin=7,xmax=10,ymin=0,ymax=2.5)+
  annotation_custom(ggplotGrob(line),xmin=7,xmax=10,ymin=4.2,ymax=3.3)+
  annotation_custom(ggplotGrob(party),xmin=0,xmax=6,ymin=0,ymax=4.9)+
  xlim(0,10)+
  ylim(0,6.25) +
  theme_void() +
  theme(plot.background = element_rect(fill="#eeeeee", colour = NA))

# Assemble with annotation_custom.
map <- gb +
  annotation_custom(
    grob=ggplotGrob(london),
    xmin=unname(uk_bbox$xmax +1*london_width),
    xmax=unname(uk_bbox$xmax) + 6*london_width,
    ymin=unname(uk_bbox$ymin) +1*london_height,
    ymax=unname(uk_bbox$ymin) + 6*london_height
  ) 

ggsave(filename="./static/class/03-class_files/spoke-legend.png", plot=legend,width=8, height=4, dpi=300)

ggsave(filename="./static/class/03-class_files/spoke-map.png", plot=map,width=11, height=7, dpi=300)



# Map Swing

plot <- data_gb %>% 
  mutate(margin_leave=leave_hanretty-50) %>% 
  ggplot() +
  geom_sf(aes(fill=margin_leave), colour="#eeeeee", size=0.01)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#636363", fill="transparent", size=0.04)+
  geom_sf(data=. %>% summarise(), colour="#636363", fill="transparent", size=0.08)+
  coord_sf(crs=27700, datum=NA) +
  guides(fill=guide_legend(title="Majority Leave:Remain")) +
  theme_v_gds() +
  theme(legend.position = "right") +
  scale_fill_distiller(palette="BrBG", limits=c(-30,30), labels=c("Heavy Remain", "", "", "No majority", "","",  "Heavy Leave")) +
  labs(
    title="Vote margin for Leave:Remain in Great Britain",
    subtitle="-- Estimated by Constituency via Hanretty 2017.",
    caption="Data published by House of Commons Library, accessed via `parlitools`"
  )

ggsave(filename="./static/class/03-class_files/referendum-map.png", plot=plot,width=8.5, height=7, dpi=300)

