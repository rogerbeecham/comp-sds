london_squared <- readRDS("/Users/roger/Dropbox (Personal)/-/git/od-vis/london_squared.Rds")
london_boundaries <- readRDS("/Users/roger/Dropbox (Personal)/-/git/od-vis/london_boundaries.Rds")

london_centroids <- london_boundaries %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename("east"="X", "north"="Y") %>%
  add_column(ladcd=london_boundaries %>% pull(lad15cd),
             ladnm=london_boundaries %>% pull(lad15nm)) %>% 
  left_join(london_squared, by=c("ladnm"="authority"))

# Helper function for rescaling
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}

# Make sf object of LondonSquared layout
grid_sf = st_sf(geom=st_make_grid(london_boundaries, n=c(8,7), what="polygons")) %>% mutate(id=row_number())
# Store gid cell locations and add as fields.
x <- rep(1:8,7)
y <- vector(length=length(x))
for(i in 1:7) {
  for(j in 1:8) {
    index=(i-1)*8+j
    y[index] <- map_scale(i,1,7,7,1)
  }  
}
grid_sf = grid_sf %>% add_column(x=x, y=y) %>% 
  inner_join(london_centroids %>% select(BOR, fX, fY), by=c("x"="fX", "y"="fY")) %>% 
  st_cast(to="MULTIPOLYGON") %>% 
  rename("geometry"="geom") %>%
  arrange(id) 
# Calculate grid centroids for relabelling.
grid_centroids <- grid_sf %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% rename("east"="X", "north"="Y")
# Add to grid_sf
grid_sf <- grid_sf %>%
  mutate(east=grid_centroids$east, north=grid_centroids$north, type="grid")
rm(grid_centroids)
# Calculate real centroids for relabelling.
real_centroids <- london_boundaries %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% rename("east"="X", "north"="Y")
# Add to grid_sf
real_sf <- london_boundaries %>%
  mutate(east=real_centroids$east, north=real_centroids$north) %>%
  left_join(london_centroids %>% select(ladcd, fX, fY, BOR), by=c("lad15cd"="ladcd"))
#real_sf = ms_simplify(real_sf)
# Update so can be bound for lerp.
t <- grid_sf %>% select(id, x, y, BOR)
st_geometry(t) = NULL
real_sf <- real_sf %>% 
  left_join(t, by=c("BOR"="BOR")) %>% 
  mutate(type="real") %>%
  arrange(id) %>%
  select(id, x,y,BOR, east, north, type)
# rbind() in order to lerp between layouts.
grid_real_sf <- rbind(grid_sf,real_sf) %>% mutate(type=fct_relevel(as_factor(type), "real","grid"))

st_write(grid_real_sf,"./lib/grid_real.geojson")
