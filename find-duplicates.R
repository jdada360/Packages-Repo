find_duplicates <- function(df, buffer = NULL, include = TRUE) {
  
  require(tidyverse)
  require(sf)
  require(ggplot2)
  
  
  # Create polygons ============================================================
  
  coords <-
    df %>% 
    dplyr::select(source_id,
                  gpslongitude,
                  gpslatitude,
                  gpsaccuracy) %>%
    filter(!is.na(gpslongitude) & !is.na(gpslatitude)) %>% 
    st_as_sf(coords = c("gpslongitude","gpslatitude"),
             crs = 4326, 
             remove = F) %>% 
    st_transform(21097) %>% 
    st_make_valid() 
  
  if (missing(buffer)) {
    
    polygons <-
      st_buffer(
        coords,
        dist = coords$gpsaccuracy
      ) %>% 
      st_make_valid() 
    
  }
  
  else {
    
    polygons <-
      st_buffer(
        coords,
        dist = buffer
      ) %>% 
      st_set_precision(2) %>%
      st_make_valid() 
    
  }
  
  # Plot to check polygons have been created =====================================
  
  single_poly <- 
    polygons %>% 
    slice(1) %>% 
    st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
    ggplot() +
    geom_sf(color = "pink2",
            linewidth = 2,
            fill = NA) +
    coord_sf(expand = TRUE) +
    theme_bw() +
    labs(x = "longitude",
         y = "latitude",
         title = "Polygon of Random Water Source") +
    theme(plot.title = element_text(size = 12, hjust = 0.5),
          text = element_text(size = 10))
  
  print(single_poly)
  
  
  # Find Polygon Intersections ==================================================
  
  polygons <-
    polygons %>%
    mutate(origin_id = row_number())
  
  
  source_intersection <-
    polygons %>%
    st_intersection()
  
  
  overlapping_sources <-
    source_intersection %>%
    filter(n.overlaps > 1) %>%
    mutate(origins = gsub("\\:", ", ",
                          gsub("\\(|\\)|c", "",
                               as.character(origins)))) %>%
    apply_labels(source_id = "Unique Identifier for Water Sources",
                 origin_id = "Unique Identifier for Water Sources used in computing
                intersections",
                 n.overlaps = "Number of overlapping sources in a given geometry",
                 origins = "List of origin_id with intersecting polyons")
  
  overlapping_sources$origins <- lapply(as.list(overlapping_sources$origins),
                                        function(list){
                                          as.list(as.numeric(str_split(list,
                                                                       ", ")[[1]]))
                                        })
  
  # Plot of Intersections =======================================================
  
  intersections <-
    overlapping_sources %>%
    dplyr::select(-"origins") %>%
    ggplot() +
    labs(
      x = "longitude",
      y = "latitude",
      title = "Intersections of overlapping water sources"
    ) +
    geom_sf(color = "pink2",
            linewidth = 5) +
    coord_sf(expand = TRUE) +
    theme_bw() +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          text = element_text(size = 9),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(intersections)
  
  # Create `match_id` ===========================================================
  
  overlapping_sources <-
    lapply(seq_along(overlapping_sources$origins),
           function(i){
             
             list <- unlist(overlapping_sources$origins[[i]])
             
             polygons %>%
               filter(origin_id %in% list) %>%
               mutate(match_id = i,
                      n_match = n()) %>%
               st_drop_geometry()
             
           })
  
  overlapping_sources <-
    bind_rows(overlapping_sources) %>%
    inner_join(polygons) %>%
    st_as_sf()
  
  print(paste0("Prior to making match_id unique for each water source:"))
  
  describe_intersections <- function(df) {
    print(paste0("Number of intersecting sources = ", n_distinct(df[, "source_id"])))
    print(paste0("Number of sets of intersecting sources = ", n_distinct(df[, "match_id"])))
  }
  
  describe_intersections(overlapping_sources)
  
  one_intersection <-
    overlapping_sources %>%
    filter(match_id == 1) %>%
    ggplot() +
    geom_sf(linewidth = 3,
            aes(color = source_id),
            fill = NA) +
    coord_sf(expand = TRUE) +
    theme_bw() +
    labs(x = "longitude",
         y = "latitude",
         title = "Map of Intersecting Water Sources",
         color = "Water Source ID")  +
    theme(plot.title = element_text(size = 12, hjust = 0.5),
          text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.subtitle = element_text(size = 12, hjust = 0.5))
  
  print(one_intersection)
  
  # Create `match_id_new` for sources that have multiple intersections =========
  
  overlapping_id <-
    unlist(unique(overlapping_sources$source_id))
  
  paired_ids <- list()
  new_id_list <- list()
  
  for (i in 1:length(overlapping_id)){
    
    if (overlapping_id[[i]] %in% paired_ids){
      new_id_list[[i]] <- NULL
    }
    
    else {
      match_id_list <-
        overlapping_sources  %>% 
        filter(source_id == overlapping_id[[i]]) %>% 
        dplyr::select(match_id) %>% 
        unlist()
      
      source_id_list <-
        overlapping_sources %>% 
        filter(match_id %in% match_id_list) %>% 
        distinct(source_id) %>% 
        inner_join(overlapping_sources,
                   by = "source_id") %>% 
        distinct(match_id) %>% 
        inner_join(overlapping_sources,
                   by = "match_id",
                   multiple = "all") %>% 
        distinct(source_id) %>% 
        inner_join(overlapping_sources,
                   by = "source_id",
                   multiple = "all") %>% 
        distinct(match_id) %>% 
        inner_join(overlapping_sources,
                   by = "match_id",
                   multiple = "all") %>% 
        distinct(source_id) %>% 
        inner_join(overlapping_sources,
                   by = "source_id",
                   multiple = "all") %>%
        distinct(match_id) %>% 
        inner_join(overlapping_sources,
                   by = "match_id",
                   multiple = "all") %>% 
        distinct(source_id, match_id, .keep_all = TRUE)
      
      paired_ids <- append(
        paired_ids,
        unlist(source_id_list[, "source_id"]))
      
      new_id_list[[i]] <-
        source_id_list %>%
        mutate(match_id_new = i) 
    }
  }
  
  new_id_df <-
    bind_rows(new_id_list) %>%
    group_by(match_id_new) %>%
    mutate(n_match_new = n_distinct(source_id)) %>%
    ungroup() %>%
    group_by(source_id) %>%
    mutate(n_new_id = n_distinct(match_id_new)) %>%
    ungroup() 
  
  
  # Create new buffers for each group of overlapping polygons ====================
  
  union <-
    new_id_df %>% 
    ungroup() %>% 
    st_as_sf() %>% 
    group_by(match_id_new) %>% 
    mutate(geometry = st_union(geometry))
  
  # Plot of each new buffer and the original polygons ==========================
  
  suppressMessages({
    for (id in unique(union$match_id_new)[1:10]){
      source_id_list <-
        new_id_df %>% 
        ungroup() %>% 
        filter(match_id_new == id) %>% 
        st_drop_geometry() %>% 
        distinct(source_id) %>% 
        unlist()
      
      fig <-
        polygons %>% 
        filter(source_id %in% source_id_list) %>% 
        ggplot() +
        geom_sf(linewidth = 7,
                aes(color = "pink2"),
                fill = NA) +
        coord_sf(expand = TRUE) +
        geom_sf(data = filter(union, match_id_new == id),
                linewidth = 2,
                fill = NA,
                aes(color = "lightblue2")) +
        theme_bw() +
        labs(x = "longitude",
             y = "latitude",
             title = "Map of Individual Water Source Polygons and New Buffers",
             subtitle = paste0("match_id_new == ", id)) +
        theme(plot.title = element_text(size = 12, hjust = 0.5),
              text = element_text(size = 11),
              plot.subtitle = element_text(size = 11, hjust = 0.5)) +
        scale_color_manual(
          name = "Polygon Type",
          values = c("lightblue2", "pink2"),
          labels = c("New Buffer", "Water Source Polygons")
        )
      
      print(fig)
      
    }
  })
  
  # Create master of new water sources =========================================
  
  union_ids <-
    union %>% 
    dplyr::select(source_id) %>% 
    distinct(source_id) %>% 
    unlist()
  
  just_unions <-
    union %>% 
    dplyr::select(-c("match_id",
                     "n_match",
                     "origin_id",
                     "n_new_id",
                     "n_match_new")) %>% 
    rename_at(
      "match_id_new",
      ~ gsub("_new", "", .x)
    ) %>% 
    distinct(source_id, .keep_all = TRUE) %>% 
    ungroup() 
  
  updated_sources <-
    polygons %>% 
    filter(!(source_id %in% union_ids)) %>% 
    distinct(source_id, .keep_all = TRUE) %>% 
    dplyr::select(-"origin_id") %>% 
    bind_rows(just_unions) %>% 
    arrange(match_id) %>% 
    ungroup() %>% 
    mutate(
      match_id = case_when(
        is.na(match_id) ~ row_number(),
        TRUE ~ match_id
      )
    ) %>% 
    group_by(match_id) %>% 
    mutate(n_match = n()) %>% 
    ungroup() 
  
  updated_sources_all <-
    updated_sources %>% 
    dplyr::select(-c("gpslatitude",
                     "gpslongitude",
                     "gpsaccuracy")) %>% 
    right_join(df,
               by = "source_id") %>% 
    mutate(county = gsub("COUNTY", "", county),
           county = factor(trimws(county),
                           levels = c("BUNGOMA","KAKAMEGA",
                                      "VIHIGA", "OTHER"))) %>% 
    st_as_sf()
  
  
  # Updated plot of all water sources ==========================================
  
  updated_sources_all %>% 
    ggplot() +
    geom_sf(aes(color = county),
            linewidth = 3,
            fill = NA) +
    coord_sf(expand = TRUE) +
    theme_bw() +
    labs(x = "longitude",
         y = "latitude",
         title = "Map of Updated Water Sources",
         color = "County") +
    scale_color_viridis(discrete = TRUE, option = "D") +
    theme(plot.title = element_text(size = 12, hjust = 0.5),
          text = element_text(size = 11),
          plot.subtitle = element_text(size = 11, hjust = 0.5),
          legend.title.align = 0.5,
          legend.title = element_text(size = 9),
          legend.text=element_text(size = 9),
          legend.key.size = unit(0.5, 'cm'),
          legend.box.background = element_rect(colour = "black"))
  
  print(paste0("After making match_id unique for each water source:"))
  describe_intersections(just_unions)
  
  paste("From the initial",
        nrow(polygons),
        "water sources with GPS coordinates, after grouping intersecting sources together we have",
        n_distinct(updated_sources$match_id),
        "water sources, so",
        nrow(polygons) -  n_distinct(updated_sources$match_id),
        "less water sources once we combine overlapping sources")
  
  return(updated_sources_all)
}