

if(!is.na(trips$Origin_address[1])){
  query_frame=
    tibble(trip_id = 1:nrow(trips),
           origin_id = trips$LabelID,
           origin = trips$Origin_address,
           dest = trips$Dest_address) %>%
    mutate(
      distance_mi=NA,
      duration_min= NA,
    )
  input_type = 'string'
  
} else {
  query_frame =
    tibble(trip_id = 1:nrow(trips),
           origin_id = trips$LabelID,
           origin_lat = trips$Origin_lat,
           origin_lon = trips$Origin_lon,
           dest_lat = trips$Dest_lat,
           dest_lon = trips$Dest_lon) %>%
    mutate(
      distance_mi=NA,
      duration_min= NA,
    )
  input_type = 'coords'
}

# Run Queries------------------------------------------------------------------------------------------------------------------
for(i in 1:nrow(query_frame)){
  
  if(input_type=='string'){
    oid = query_frame$origin_id[i]
    o_xy = query_frame$origin[i]
    d_xy = query_frame$dest[i]
    
    dirs = google_directions(origin = o_xy,
                             destination = d_xy,
                             mode = mode_sel,
                             departure = as.POSIXct(departure_datetime, tz = timezone))
  } else {
    oid = query_frame$origin_id[i]
    o_xy = as.numeric(c(query_frame$origin_lat[i],query_frame$origin_lon[i]))
    d_xy = as.numeric(c(query_frame$dest_lat[i],query_frame$dest_lon[i]))
    
    dirs = google_directions(origin = o_xy,
                             destination = d_xy,
                             mode = mode_sel,
                             departure = as.POSIXct(departure_datetime,tz = timezone))
  }
  
  # shp= st_as_sf(gDirsToShape(dirs,mode="driving")) %>%
  #   mutate(home_id=oid)
  # shape_list[[i]]=shp 
  
  if(dirs$status=="OK"){
    duration_min <- round(dirs$routes$legs[[1]]$duration$value/60 %>% round(1), 1)
    distance_mi <- round(dirs$routes$legs[[1]]$distance$value*0.000621371,1)
    
    query_frame$duration_min[i] <- duration_min
    query_frame$distance_mi[i] <- distance_mi
  }
  else{
    print("Error: cannot geocode the current input!")
  }
  
  Sys.sleep(0.1)
  currentTime = Sys.time()
  print(paste0(i," of ",nrow(query_frame)," complete, averaging ",
               round(as.numeric(difftime(currentTime,startTime,units="secs")/(i)),2)," seconds per query.",
               " ",round(as.numeric(difftime(currentTime,startTime,units="min")),2)," minutes so far. Estimated ",
               round(as.numeric(difftime(currentTime,startTime,units="min")/(i))*(nrow(query_frame)-i),2)," minutes to go"))
  
}

# When done, write outputs---------------------------------------------------------------------------------------------------------------------
output_name <- file.path("output",paste(mode_sel, gsub(":", "_", departure_datetime), sep = '_'))

saveRDS(query_frame, paste0(output_name,'.rds'))
write.csv(query_frame, paste0(output_name,'.csv'))