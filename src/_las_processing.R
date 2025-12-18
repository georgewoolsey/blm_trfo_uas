# ## remotes helps us get packages hosted on github
# install.packages("remotes")
# ## get cloud2trees
# remotes::install_github(repo = "georgewoolsey/cloud2trees", upgrade = F)

# bread-and-butter
library(tidyverse) # the tidyverse
library(viridis) # viridis colors
library(harrypotter) # hp colors
library(RColorBrewer) # brewer colors
library(scales) # work with number and plot scales
library(latex2exp)

# visualization
library(mapview) # interactive html maps
library(kableExtra) # tables
library(patchwork) # combine plots

# spatial analysis
library(terra) # raster
library(sf) # simple features
library(lidR) # lidar data
library(cloud2trees) # the cloud2trees


pj_stand_boundary <- 
  sf::st_read("c:/data/usfs/slash_pile_detecting/data/Dawson_data/units/units.shp", quiet=T) %>% 
  dplyr::rename_with(tolower) %>% 
  dplyr::rename_with(stringr::str_squish) %>% 
  dplyr::rename_with(make.names) %>% 
  dplyr::rename_with(~stringr::str_replace_all(.x, "\\.{2,}", ".")) %>% 
  dplyr::rename_with(~stringr::str_remove(.x, "\\.$")) %>% 
  dplyr::rename_with(~stringr::str_replace_all(.x, "\\.", "_")) %>% 
  dplyr::mutate(site = "TRFO-BLM Pinyon-Juniper Site")

# directory with the downloaded .las|.laz files
f <- "d:/BLM_CO_SWDF_DawsonFuelsTreatment/Final/PointCloud/Tiles"
# f <- "d:/BLM_CO_SWDF_DawsonFuelsTreatment/Final/PointCloud/Full/"
# is there data?
list.files(f, pattern = ".*\\.(laz|las)$") %>% length()
# what files are in here?
list.files(f, pattern = ".*\\.(laz|las)$")[1:3]

ctg_temp <- lidR::readLAScatalog(f)
ctg_temp

ctg_temp@data %>%
  mapview::mapview(popup = F, layer.name = "point cloud tile")

ctg_temp@data %>%
  dplyr::mutate(
    nm =
      basename(filename) %>%
      stringr::str_remove("\\.[^.]+$") %>%
      stringr::str_remove("BLM_CO_SWDF_DawsonFuelsTreatment_PointCloud_")
  ) %>%
  ggplot2::ggplot() + 
    ggplot2::geom_sf(fill=NA, color = "red")
    # ggplot2::geom_sf_label(mapping = ggplot2::aes(label = nm),size=2) +
    # ggplot2::theme_void()
## wtf????
ctg_temp %>% 
  cloud2trees:::check_las_ctg_empty() %>% 
  purrr::pluck("data") %>% 
  dplyr::mutate(
    nm =
      basename(filename) %>%
      stringr::str_remove("\\.[^.]+$") %>%
      stringr::str_remove("BLM_CO_SWDF_DawsonFuelsTreatment_PointCloud_")
  ) %>%
  ggplot2::ggplot() + 
    ggplot2::geom_sf(fill=NA, color = "red") +
    ggplot2::geom_sf_label(mapping = ggplot2::aes(label = nm),size=2) +
    ggplot2::theme_void()
  
# outdir
c2t_output_dir <- "../data"
c2t_process_dir <- file.path(c2t_output_dir, "point_cloud_processing_delivery")
c2t_tracking_fnm <- file.path(c2t_process_dir, "processed_tracking_data.csv")
##############################################################
# cloud2trees::cloud2raster
##############################################################
if(
  !file.exists( file.path(c2t_process_dir, "chm_0.1m.tif") )
  || !file.exists( file.path(c2t_process_dir, "dtm_0.25m.tif") )
){
  # time it
  st_temp <- Sys.time()
  # run it
  # cloud2trees
  cloud2raster_ans <- cloud2trees::cloud2raster(
    output_dir = c2t_output_dir
    , input_las_dir = f
    , accuracy_level = 2
    , keep_intrmdt = T
    , dtm_res_m = 0.25
    , chm_res_m = 0.1
    , min_height = 0 # effectively generates a DSM based on non-ground points
  )
  
  # timer
  mins_temp <- difftime(Sys.time(),st_temp,units = "mins") %>% as.numeric()
  # save tracking
  dplyr::tibble(
    timer_cloud2raster_mins = mins_temp
  ) %>% 
    write.csv(
      file = c2t_tracking_fnm
      , row.names = F, append = F
    )
}else{
  dtm_temp <- terra::rast( file.path(c2t_process_dir, "dtm_0.25m.tif") )
  chm_temp <- terra::rast( file.path(c2t_process_dir, "chm_0.1m.tif") )
  
  cloud2raster_ans <- list(
    "dtm_rast" = dtm_temp
    , "chm_rast" = chm_temp
  )
}

remove(list = ls()[grep("_temp",ls())])
remove(f)
gc()

##############################################################
# prep raster for cloud2trees::raster2trees
##############################################################
# we executed `cloud2trees::cloud2raster()` with the settings `min_height = 0` to effectively generate a DSM based on non-ground points for use in the slash pile detection process
# cloud2trees::raster2trees

# plot to check out the fine-resolution DSM raster
cloud2raster_ans$chm_rast %>% terra::plot()

cloud2raster_ans$chm_rast %>% 
  terra::crop(
    pj_stand_boundary %>% 
      dplyr::filter(
        tolower(unit)=="u10"
        & stringr::str_remove_all(tolower(tretmnt),"\\s") == "uniform-high"
      ) %>% 
      sf::st_union() %>% 
      sf::st_buffer(15) %>% 
      sf::st_transform(terra::crs(cloud2raster_ans$chm_rast)) %>% 
      terra::vect()
  ) %>% 
  terra::plot(col = viridis::plasma(100), axes = F)
terra::plot(
  pj_stand_boundary %>% 
      dplyr::filter(
        tolower(unit)=="u10"
        & stringr::str_remove_all(tolower(tretmnt),"\\s") == "uniform-high"
      ) %>% 
      sf::st_transform(terra::crs(cloud2raster_ans$chm_rast)) %>% 
      terra::vect()
  , add = T, border = "black", col = NA, lwd = 1.2
)

##################################################################
# aggregate to make raster more coarse for tree detection
##################################################################
  # first, we'll borrow from the `cloud2trees` codebase to get a function to change the resolution of a raster exactly
  ###___________________________________________###
  # adjust the resolution of a raster to be in exactly the target resolution
  ###___________________________________________###
  if(
    !file.exists( file.path(c2t_process_dir, "chm_0.25m.tif") )
  ){
    agg_chm_rast <- cloud2trees:::adjust_raster_resolution(
      cloud2raster_ans$chm_rast
      , target_resolution = 0.25
      , fun = max
      , resample_method = "max"
      , ofile = file.path(c2t_process_dir, "chm_0.25m.tif")
    )
  }else{
    agg_chm_rast <- terra::rast( file.path(c2t_process_dir, "chm_0.25m.tif") )
  }
  
  agg_chm_rast
  cloud2raster_ans$chm_rast
  
  agg_chm_rast %>% 
    terra::crop(
      pj_stand_boundary %>% 
        dplyr::filter(
          tolower(unit)=="u10"
          & stringr::str_remove_all(tolower(tretmnt),"\\s") == "uniform-high"
        ) %>% 
        sf::st_union() %>% 
        sf::st_buffer(15) %>% 
        sf::st_transform(terra::crs(agg_chm_rast)) %>% 
        terra::vect()
    ) %>% 
    terra::plot(col = viridis::plasma(100), axes = F)
  terra::plot(
    pj_stand_boundary %>% 
        dplyr::filter(
          tolower(unit)=="u10"
          & stringr::str_remove_all(tolower(tretmnt),"\\s") == "uniform-high"
        ) %>% 
        sf::st_transform(terra::crs(agg_chm_rast)) %>% 
        terra::vect()
    , add = T, border = "black", col = NA, lwd = 1.2
  )
##############################################################
# cloud2trees::itd_tuning
##############################################################
  ###___________________________________________###
  # itd_tuning
  ###___________________________________________###
  # run it
  set.seed(99)
  itd_tuning_ans <- cloud2trees::itd_tuning(input_chm_rast = agg_chm_rast, min_height = 1.37)
  itd_tuning_ans %>% names()
  itd_tuning_ans$plot_samples
  ggplot2::ggsave(
    filename = file.path(c2t_process_dir, "itd_tuning_plot_samples.jpg")
    , plot = itd_tuning_ans$plot_samples
    , height = 7, width = 7
    , dpi = "print"
  )
  itd_tuning_ans$plot_sample_summary
  ggplot2::ggsave(
    filename = file.path(c2t_process_dir, "itd_tuning_plot_sample_summary.jpg")
    , plot = itd_tuning_ans$plot_sample_summary
    , height = 7, width = 9.5
    , dpi = "print"
  )  
  
  # the exp_fn results in too few trees with crown diameters generally similar to the total tree height which is not expected
  # also, how does a 3m tall tree have a >10m crown diameter?
  
  # let's plot the functions to see what it might be
   # we can see what this function looks like for window size
  # itd_tuning_ans$ws_fn_list$log_fn
  plt_ws_fn <- ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = itd_tuning_ans$ws_fn_list$exp_fn
      , mapping = ggplot2::aes(color = "exp_fn")
      , lwd = 2
    ) +
    ggplot2::geom_function(
      fun = itd_tuning_ans$ws_fn_list$lin_fn
      , mapping = ggplot2::aes(color = "lin_fn")
      , lwd = 2
    ) +
    ggplot2::geom_function(
      fun = itd_tuning_ans$ws_fn_list$log_fn
      , mapping = ggplot2::aes(color = "log_fn")
      , lwd = 2
    ) +
    ggplot2::xlim(-5,60) +
    ggplot2::labs(x = "heights", y = "ws", color = "") +
    ggplot2::scale_color_manual( values = c(viridis::viridis(3), "gray") ) +
    ggplot2::theme_light()
  # itd_tuning_ans$ws_fn_list$log_fn
  # lets try out a new function
  my_lin_fn <- function (x) {
      y <- dplyr::case_when(
        is.na(x) ~ 0.001
        , x < 0 ~ 0.001
        , x > (5.3-0.51)/0.21 ~ 5.3
        , TRUE ~ 0.51 + (x * 0.21)
      )
      return(y)
  }
  
  plt_ws_fn +
    ggplot2::geom_function(
      fun = my_lin_fn
      , mapping = ggplot2::aes(color = "my_lin_fn")
      , lwd = 2
    )
    
    itd_tuning_ans2 <- cloud2trees::itd_tuning(
      input_chm_rast = agg_chm_rast
      , min_height = 1.37
      , ws_fn_list = list(
        lin_fn = itd_tuning_ans$ws_fn_list$lin_fn
        , log_fn = itd_tuning_ans$ws_fn_list$log_fn
        , my_lin_fn = my_lin_fn
      )
    )
    itd_tuning_ans2$plot_samples
    itd_tuning_ans2$plot_sample_summary
    itd_tuning_ans2$crowns %>% dplyr::glimpse()
    itd_tuning_ans2$crowns %>% sf::st_drop_geometry() %>% dplyr::count(sample_number, ws_fn)
    ggplot2::ggsave(
      filename = file.path(c2t_process_dir, "itd_tuning_plot_samples2.jpg")
      , plot = itd_tuning_ans2$plot_samples
      , height = 7, width = 7
      , dpi = "print"
    )
    ggplot2::ggsave(
      filename = file.path(c2t_process_dir, "itd_tuning_plot_sample_summary2.jpg")
      , plot = itd_tuning_ans2$plot_sample_summary
      , height = 7, width = 9.5
      , dpi = "print"
    )  
    

  #### plot with RGB data
  pj_rgb_rast <- terra::rast("d:/BLM_CO_SWDF_DawsonFuelsTreatment/Final/Ortho/BLM_CO_SWDF_DawsonFuelsTreatment_Ortho_202504.tif") %>% 
    terra::subset(c(1,2,3))
  
  crp_rgb_rast_temp <- pj_rgb_rast %>% 
    terra::crop(
      itd_tuning_ans2$crowns %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(sample_number == 1) %>% 
        sf::st_union() %>% 
        sf::st_bbox() %>% 
        sf::st_as_sfc() %>% 
        sf::st_buffer(0.5) %>% 
        sf::st_transform(terra::crs(pj_rgb_rast)) %>% 
        terra::vect()
    )
  crp_chm_rast_temp <- agg_chm_rast %>% 
    terra::crop(
      itd_tuning_ans2$crowns %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(sample_number == 1) %>% 
        sf::st_union() %>% 
        sf::st_bbox() %>% 
        sf::st_as_sfc() %>% 
        sf::st_buffer(0.5) %>% 
        sf::st_transform(terra::crs(agg_chm_rast)) %>% 
        terra::vect()
    )
  # # ?terra::plotRGB
  # # ?terra::ifel
  # terra::plotRGB(crp_rgb_rast_temp, stretch = "lin", axes = F)
  # terra::plot(
  #   crp_chm_rast_temp
  #   # terra::ifel(crp_chm_rast_temp<1.37,NA,crp_chm_rast_temp)
  #   , add = T
  #   , col = viridis::plasma(100), alpha = 0.3
  #   , legend = F
  #   , axes = F
  # )
  # terra::plot(
  #   itd_tuning_ans2$crowns %>% 
  #     cloud2trees::simplify_multipolygon_crowns() %>% 
  #     dplyr::ungroup() %>% 
  #     dplyr::filter(sample_number == 1 & ws_fn == "my_lin_fn") %>% 
  #     sf::st_transform(terra::crs(rgb_rast_temp)) %>% 
  #     terra::vect()
  #   , add = T, border = "brown", col = NA, lwd = 1.6
  # )
  # terra::plot(
  #   itd_tuning_ans2$crowns %>% 
  #     cloud2trees::simplify_multipolygon_crowns() %>% 
  #     dplyr::ungroup() %>% 
  #     dplyr::filter(sample_number == 1 & ws_fn == "lin_fn") %>% 
  #     sf::st_transform(terra::crs(rgb_rast_temp)) %>% 
  #     terra::vect()
  #   , add = T, border = "gold", col = NA, lwd = 1.2
  # )
  # terra::plot(
  #   itd_tuning_ans2$crowns %>% 
  #     cloud2trees::simplify_multipolygon_crowns() %>% 
  #     dplyr::ungroup() %>% 
  #     dplyr::filter(sample_number == 1 & ws_fn == "log_fn") %>% 
  #     sf::st_transform(terra::crs(rgb_rast_temp)) %>% 
  #     terra::vect()
  #   , add = T, border = "navy", col = NA, lwd = 0.8
  # )
  
  # xxx <-
  #   itd_tuning_ans2$crowns %>%
  #     dplyr::ungroup() %>%
  #     cloud2trees::simplify_multipolygon_crowns() %>%
  #     sf::st_make_valid() %>%
  #     dplyr::filter(sample_number == 1 & ws_fn == "my_lin_fn")
  # 
  # mapview::mapview(xxx, zcol = "tree_height_m")
  # remove(xxx)
  # gc()
  
  # ctg_temp <- lidR::readLAScatalog(
  #   file.path(c2t_output_dir, "point_cloud_processing_temp", "02_normalize")
  # )
  # las_temp <- lidR::clip_roi(
  #   ctg_temp
  #   , itd_tuning_ans2$crowns %>% 
  #     dplyr::ungroup() %>% 
  #     cloud2trees::simplify_multipolygon_crowns() %>% 
  #     sf::st_make_valid() %>% 
  #     dplyr::filter(sample_number == 1 & ws_fn == "my_lin_fn") %>% 
  #     dplyr::filter(
  #       # treeID %in% c("68_708803.7_4193239.3", "67_708804.9_4193239.9" , "69_708804.9_4193237.9", "66_708803.9_4193240.3")
  #       # treeID %in% c("6_708811.3_4193258.3")
  #       treeID %in% c("71_708809.1_4193235.3", "76_708807.3_4193233.9", "72_708805.5_4193234.7")
  #     ) %>% 
  #     sf::st_union() %>% 
  #     sf::st_buffer(1) %>% 
  #     sf::st_transform(lidR::st_crs(ctg_temp))
  # )
  # las_temp
  # lidR::plot(
  #   las_temp, color = "RGB", bg = "white", legend = F
  # )
  # lidR::plot(
  #   las_temp, color = "Z", bg = "white", legend = T
  # )
  
  # convert raster to a data frame and create hex colors
  # ?grDevices::rgb
  rgb_df_temp <-
    crp_rgb_rast_temp %>% 
    terra::as.data.frame(xy = TRUE) %>%
    dplyr::rename(
      red = 3, green = 4, blue = 5
    ) %>% 
    dplyr::mutate(
      hex_col = grDevices::rgb(
        red
        , green
        , blue
        , maxColorValue = 255
      )
    )
    # dplyr::glimpse()
  
  # plt
  ggplot2::ggplot() +
    # add rgb base map
    ggplot2::geom_raster(data = rgb_df_temp, mapping = ggplot2::aes(x = x, y = y, fill = hex_col)) +
    # use identity scale so the hex codes are used directly
    ggplot2::scale_fill_identity() +
    # overlay polygons
    # ggplot2::geom_sf(data = polys, fill = NA, color = "red", linewidth = 0.5) +
    ggplot2::geom_sf(
      data = itd_tuning_ans2$crowns %>% 
        dplyr::filter(sample_number==1) %>% 
        sf::st_transform(terra::crs(crp_rgb_rast_temp))
      , mapping = ggplot2::aes(color = ws_fn)
      , fill = NA
      , lwd = 0.8
      , inherit.aes = F
    ) +
    ggplot2::facet_grid(cols = dplyr::vars(ws_fn)) +
    ggplot2::scale_color_viridis_d(name = "") +
    ggplot2::coord_sf(expand = F) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none"
      , strip.text = ggplot2::element_text(face = "bold", color = "black", margin = ggplot2::margin(t = 4, b = 4))
      , strip.background = ggplot2::element_rect(fill = "gray88", color = "gray88")
      , panel.spacing = ggplot2::unit(1,"lines")
    )
  
  
  remove(list = ls()[grep("_temp",ls())])
  gc()
  
  # make a function to do this
  plt_rgb_rast_itd_crowns <- function(sample_nmbr = 1, rgb_rast, itd_crowns, plt_lwd = 0.7) {
    # crop
    crp_rgb_rast_temp <- rgb_rast %>% 
      terra::crop(
        itd_crowns %>% 
          dplyr::ungroup() %>% 
          dplyr::filter(sample_number == sample_nmbr) %>% 
          sf::st_union() %>% 
          sf::st_bbox() %>% 
          sf::st_as_sfc() %>% 
          sf::st_buffer(0.2) %>% 
          sf::st_transform(terra::crs(rgb_rast)) %>% 
          terra::vect()
      )
    # convert raster to a data frame and create hex colors
    # ?grDevices::rgb
    rgb_df_temp <-
      crp_rgb_rast_temp %>% 
      terra::as.data.frame(xy = TRUE) %>%
      dplyr::rename(
        red = 3, green = 4, blue = 5
      ) %>% 
      dplyr::mutate(
        hex_col = grDevices::rgb(
          red
          , green
          , blue
          , maxColorValue = 255
        )
      )
      # dplyr::glimpse()
    
    # plt
    plt <- ggplot2::ggplot() +
      # add rgb base map
      ggplot2::geom_raster(data = rgb_df_temp, mapping = ggplot2::aes(x = x, y = y, fill = hex_col)) +
      # use identity scale so the hex codes are used directly
      ggplot2::scale_fill_identity() +
      # overlay polygons
      # ggplot2::geom_sf(data = polys, fill = NA, color = "red", linewidth = 0.5) +
      ggplot2::geom_sf(
        data = itd_crowns %>% 
          dplyr::filter(sample_number==sample_nmbr) %>% 
          cloud2trees::simplify_multipolygon_crowns() %>% 
          sf::st_make_valid() %>% 
          dplyr::filter(sf::st_is_valid(.)) %>% 
          sf::st_transform(terra::crs(crp_rgb_rast_temp))
        , mapping = ggplot2::aes(color = ws_fn)
        , fill = NA
        , lwd = plt_lwd
        , inherit.aes = F
      ) +
      ggplot2::facet_grid(cols = dplyr::vars(ws_fn)) +
      ggplot2::scale_color_viridis_d(name = "") +
      ggplot2::coord_sf(expand = F) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none"
        , strip.text = ggplot2::element_text(face = "bold", color = "black", margin = ggplot2::margin(t = 4, b = 4))
        , strip.background = ggplot2::element_rect(fill = "gray88", color = "gray88")
        , panel.spacing = ggplot2::unit(1,"lines")
      )
    return(plt)
  }
  
  plt_rgb_rast_itd_crowns(sample_nmbr = 2, rgb_rast = pj_rgb_rast, itd_crowns = itd_tuning_ans2$crowns)
  
  # default lin_fn looks good
  my_ws_fn <- cloud2trees::itd_ws_functions()[["lin_fn"]]
  
  remove(list = ls()[grep("_temp",ls())])
  remove(pj_rgb_rast)
  gc()
##############################################################
# cloud2trees::raster2trees
##############################################################
if(
  !file.exists( file.path(c2t_process_dir, "final_detected_crowns.gpkg") )
  || !file.exists( file.path(c2t_process_dir, "final_detected_tree_tops.gpkg") )
){
  # time it
  st_temp <- Sys.time()
  # run it
  # cloud2trees
  raster2trees_ans <- cloud2trees::raster2trees(
    chm_rast = agg_chm_rast
    , outfolder = c2t_process_dir
    , ws = my_ws_fn
    , min_height = 1.37
    , min_crown_area = 0.5
  )
  # raster2trees_ans
  # timer
  mins_temp <- difftime(Sys.time(),st_temp,units = "mins") %>% as.numeric()
  # save tracking
  readr::read_csv(c2t_tracking_fnm, progress = F, show_col_types = F) %>% 
    dplyr::mutate(
      timer_raster2trees_mins = mins_temp
    ) %>% 
    write.csv(
      file = c2t_tracking_fnm
      , row.names = F, append = F
    )
}else{
  search_dir_final_detected_ans_temp <- cloud2trees:::search_dir_final_detected(dir = c2t_process_dir)
  crowns_flist_temp <- search_dir_final_detected_ans_temp$crowns_flist
  
  # read it to get the full list of tree polygons
    raster2trees_ans <- crowns_flist_temp %>%
      purrr::map(function(x){
        sf::st_read(
          dsn = x
          , quiet = T
        ) %>%
        # throw in hey_xxxxxxxxxx to test it works if we include non-existant columns
        dplyr::select( -dplyr::any_of(c(
          "hey_xxxxxxxxxx"
          , "tree_cbh_m"
          , "is_training_cbh"
        )))
      }) %>%
      dplyr::bind_rows()
}
# raster2trees_ans %>% dplyr::glimpse()
# cloud2trees:::search_dir_final_detected(dir = c2t_process_dir)$crowns_flist
  
remove(list = ls()[grep("_temp",ls())])
remove(f)
gc()

# if we don't already have the data, run it
if(!file.exists( file.path(c2t_process_dir, "cbh_data.csv") )){
  # sample proportion
  sample_prop_temp <- 0.10
  # time it
  st_temp <- Sys.time()
  # run it
  trees_cbh_ans <- cloud2trees::trees_cbh(
    trees_poly = c2t_process_dir
    , norm_las = file.path(c2t_output_dir, "point_cloud_processing_temp", "02_normalize")
    , tree_sample_prop = sample_prop_temp
    , which_cbh = "lowest"
    , estimate_missing_cbh = TRUE
    , min_vhp_n = 3
    , voxel_grain_size_m = 1
    , dist_btwn_bins_m = 1
    , min_fuel_layer_ht_m = 0.5
    , lad_pct_gap = 25
    , lad_pct_base = 25
    , num_jump_steps = 1
    , min_lad_pct = 10
    , frst_layer_min_ht_m = 0.5
    , force_same_crs = T
  )
  # timer
  mins_temp <- difftime(Sys.time(),st_temp,units = "mins") %>% as.numeric()
  # save cbh
  trees_cbh_ans %>% sf::st_drop_geometry() %>% 
    write.csv(file = file.path(c2t_process_dir, "cbh_data.csv"), row.names = F, append = F)
  # save tracking
  readr::read_csv(c2t_tracking_fnm, progress = F, show_col_types = F) %>% 
    dplyr::mutate(
      timer_trees_cbh_mins = mins_temp
      , sttng_cbh_tree_sample_n = as.character(NA)
      , sttng_cbh_tree_sample_prop = sample_prop_temp
    ) %>% 
    write.csv(
      file = c2t_tracking_fnm
      , row.names = F, append = F
    )
  
}else{
  # cbh data
  trees_cbh_ans <- readr::read_csv( file.path(c2t_process_dir, "cbh_data.csv"), progress = F, show_col_types = F)
}
