###############################################################################
# S E S S I O N  6
###############################################################################

# install.packages("parlitools")
library(parlitools)
#install.packages("tidymodels")
library(tidymodels)
library(sf)


single_model_fits <- data_for_models %>% 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="z_score") %>% 
  mutate(region=as.factor(region)) %>% 
  nest(data=-expl_var) %>%  # nest to generate list-column by expl_var. 
  mutate(
    # Use map() to iterate over the list of datasets to fit a model to each nested dataset.
    model = map(data, ~lm(leave ~ region + z_score -1, data = .x)),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map(model, augment),
  ) 

fitted_values <- single_model_fits %>% 
  unnest(c(values)) %>% 
  select(expl_var, leave, z_score, region, .fitted, .resid)

cons <-  single_model_fits %>% 
  unnest(data) %>% select(constituency_name)

coefs <- single_model_fits %>% 
  unnest(cols = coefs) %>% # unnest output from glance
  select(expl_var, term, estimate) %>% 
  filter(term=="z_score")

fits <- single_model_fits %>% 
  unnest(cols = fits) %>%  # unnest output from glance
  select(expl_var, adj.r.squared, AIC)  

scatter_data <- fitted_values %>% 
  left_join(fits) %>% 
  left_join(coefs) %>% 
  mutate(expl_var=factor(expl_var, levels=order_vars), is_scotland=region=="Scotland") %>% 
  add_column(cons)

scatters <- scatter_data %>%  
  ggplot()+
  geom_point(aes(y=leave,x=z_score, colour=is_scotland), alpha=.1) +
  geom_smooth(aes(y=.fitted, x=z_score, group=region, colour=is_scotland), method = "lm", se = FALSE, size=.4) +
  #geom_smooth(aes(y=leave,x=z_score), method = "lm", se = FALSE, colour="#1b1b1b", size=.6) +
  scale_colour_manual(values=c(site_colours$primary, site_colours$secondary), guide=FALSE) +
  geom_text(data=. %>% filter(constituency_name=="Aberavon"), aes(y=.8,x=5, label=paste0("AIC: ",round(AIC,0))), size=2.5, hjust=1)+
  geom_text(data=. %>% filter(constituency_name=="Aberavon"), aes(y=.75,x=5, label=paste0("coef: ",round(estimate,2))), size=2, hjust=1)+
  facet_wrap(~expl_var) +
  # scale_fill_distiller(palette="RdBu", direction=1, limits=c(-max(abs(residuals$resid)), max(abs(residuals$resid)))) + 
  labs(x="z-score", y="share of leave", 
       title="Scatterplots of share of leave vote by candidate explanatory variable",
       subtitle="--Constituencies of GB. Annotated with AIC and `virtual' FE lines on region",
       caption="Data published by House of Commons Library, accessed via `parlitools`"
  )  

ggsave(filename="./static/class/07-class_files/scatters.png", plot=scatters,width=10, height=7, dpi=300)




model <- data_for_models %>% 
  mutate(type="full_dataset", region=as.factor(region)) %>% 
  nest(data=-type) %>% 
  mutate(
    # Include `-1` to eliminate the constant term and include a dummy for every area
    model=map(data, ~lm(leave ~ region +  degree  + eu_born + white  + no_car + 
                          not_good_health + heavy_industry -1, data=.x)),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map(model, augment)
  ) 

model %>% 
  unnest(cols = fits) %>% # unnest output from glance
  select(-c(data, model, coefs, model, values)) %>% View # remove other list-columns


plot_outputs <- model %>% 
  unnest(cols = coefs) %>% # unnest output from glance
  select(-c(data, model, fits, model, values)) %>% 
  mutate(
    is_fe=!str_detect(term, "region"),
    term=str_remove(term,"region"),
    coef_type=if_else(!is_fe, "FE constants", "coefficients"),
    coef_type=factor(coef_type, levels=c("FE constants", "coefficients")),
    yintercept=0
  ) %>% 
  #filter(term != "(Intercept)") %>% 
  ggplot(
    aes(x=reorder(term, -estimate), 
        y=estimate,ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
  geom_pointrange(colour=site_colours$primary) +
  geom_hline(aes(yintercept=yintercept, alpha=is_fe), size=.2)+
  scale_alpha_discrete(c(0,1), guide=FALSE)+
  facet_wrap(~coef_type, scales="free", shrink=TRUE) +
  coord_flip() +
  labs(y="", x="",
       title="Outputs from multivariate model with FE grouping variable",
       subtitle="--Leave vote by demographic composition variables. AIC: -2351",
       caption="Data published by House of Commons Library, accessed via `parlitools`")
       
ggsave(filename="./static/class/07-class_files/plot-outputs-fe.png", plot=plot_outputs,width=7, height=4, dpi=300)



single_model_fits <- data_for_models %>% 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="z_score") %>% 
  mutate(region=as.factor(region), cons=1) %>% 
  nest(data=-expl_var) %>%  # nest to generate list-column by expl_var. 
  mutate(
    # Use map() to iterate over the list of datasets to fit a model to each nested dataset.
    model = map(data, ~lm(leave ~ 0 + (cons + z_score):(region), data = .x)),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map(model, augment),
  ) 


fitted_values <- single_model_fits %>% 
  unnest(c(values)) %>% 
  select(expl_var, leave, z_score, region, .fitted, .resid)

cons <-  single_model_fits %>% 
  unnest(data) %>% select(constituency_name)

coefs <- single_model_fits %>% 
  unnest(cols = coefs) %>% # unnest output from glance
  select(expl_var, term, estimate) %>% 
  filter(term=="z_score")

fits <- single_model_fits %>% 
  unnest(cols = fits) %>% # unnest output from glance
  select(expl_var, adj.r.squared, AIC) 

scatter_data <- fitted_values %>% 
  left_join(fits) %>% 
  left_join(coefs) %>% 
  mutate(expl_var=factor(expl_var, levels=order_vars), is_scotland=region=="Scotland") %>% 
  add_column(cons)

scatters <- scatter_data %>%  
  ggplot()+
  geom_point(aes(y=leave,x=z_score, colour=is_scotland), alpha=.1) +
  geom_smooth(aes(y=.fitted, x=z_score, group=region, colour=is_scotland), method = "lm", se = FALSE, size=.4) +
  #geom_smooth(aes(y=leave,x=z_score), method = "lm", se = FALSE, colour="#1b1b1b", size=.6) +
  scale_colour_manual(values=c(site_colours$primary, site_colours$secondary), guide=FALSE) +
  geom_text(data=. %>% filter(constituency_name=="Aberavon"), aes(y=.8,x=5, label=paste0("AIC: ",round(AIC,0))), size=2.5, hjust=1)+
  #geom_text(data=. %>% filter(constituency_name=="Aberavon"), aes(y=.75,x=5, label=paste0("coef: ",round(estimate,2))), size=2, hjust=1)+
  facet_wrap(~expl_var) +
  # scale_fill_distiller(palette="RdBu", direction=1, limits=c(-max(abs(residuals$resid)), max(abs(residuals$resid)))) + 
  labs(x="z-score", y="share of leave", 
       title="Scatterplots of share of leave vote by candidate explanatory variable",
       subtitle="--Constituencies of GB. Annotated with AIC and FE lines with region as interaction term",
       caption="Data published by House of Commons Library, accessed via `parlitools`"
  )  

ggsave(filename="./static/class/07-class_files/scatters-interaction.png", plot=scatters,width=10, height=7, dpi=300)





model <- data_for_models %>% 
  mutate(type="full_dataset", region=as.factor(region), cons=1) %>% 
  nest(data=-type) %>% 
  mutate(
    # `:` Notation implies interaction variables
    model=map(data, ~lm(leave ~ 0 +  (cons + degree  + eu_born + white  + no_car + 
                          not_good_health + heavy_industry):(region), data=.x)),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map(model, augment)
  ) 

model %>% 
  unnest(cols = fits) %>% View


plot_outputs <- model %>% 
  unnest(cols = coefs) %>% # unnest output from glance
  select(-c(data, model, fits, model, values)) %>% 
  separate(term, into= c("term", "region"), sep=":") %>% 
  mutate(
    region=str_remove(region,"region")
  ) %>% 
  filter(term!="cons") %>% 
  ggplot() +
  geom_col(aes(x=reorder(term, -estimate), y=estimate), fill=site_colours$primary, alpha=.3)+
  geom_pointrange(aes(x=reorder(term, -estimate), 
                      y=estimate,ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), colour=site_colours$primary) +
  geom_hline(yintercept = 0, size=.2)+
  facet_wrap(~region) +
  coord_flip()+
labs(y="estimated coefficient", x="explanatory variable",
     title="Outputs from multivariate model with FE interaction vaiable",
     subtitle="--Leave vote by demographic composition variables. AIC: -2569.",
     caption="Data published by House of Commons Library, accessed via `parlitools`")
     
ggsave(filename="./static/class/07-class_files/plot-outputs-interaction-fe.png", plot=plot_outputs,width=10, height=6, dpi=300)



model %>% 
  unnest(cols = coefs) %>% # unnest output from glance
  select(-c(data, model, fits, values)) %>% View # remove other list-columns


model %>% 
  unnest(cols = c(data, resids)) %>% 
  select(-model) 


max_resid <- max(abs(model_values$.resid))