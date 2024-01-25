# -------------------------------------------------------------------------
# Save the centroids of the gemeinden
# -------------------------------------------------------------------------

op_dir = rajudas::makePath("~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/centroids/")
if(!dir.exists(op_dir)){
  dir.create(op_dir, recursive = T)
}

oe_gem_data = rajudas::oe_gem_data(current_year_gemeinden)
oe_gem_data = oe_gem_data %>% st_transform(crs=4326) %>% st_make_valid()
oe_gem_data_centroids = st_centroid(oe_gem_data)
walk(1:nrow(oe_gem_data_centroids), function(r){
  row = oe_gem_data_centroids[r, ] %>%
    mutate(
      x = st_coordinates(.)[,1],
      y = st_coordinates(.)[,2]
    ) %>% st_drop_geometry()
  op = glue("{op_dir}/{row$id}.csv")
  write_csv(row, op)
})
