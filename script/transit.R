
if(traffic_model_sel == "best_route"){
  traffic_model_sel = NULL
}else{
  traffic_model_sel = traffic_model_sel
}

if(transit_mode == "not_specified"){
  transit_mode = NULL
}else{
  transit_mode = transit_mode
}

if(!is.na(trips$Origin_address[1])){
  query_frame=
    tibble(trip_id = 1:nrow(trips),
           origin_id = trips$LabelID,
           origin = trips$Origin_address,
           dest = trips$Dest_address) %>%
    mutate(
      total_dist_mi= NA,
      total_duration_min = NA,
      transport_time = NA,
      transit_dist_mi=NA,
      transit_duration_min=NA,
      total_walk_dist_mi = NA,
      total_walk_duration_min = NA,
      walk_dist_origin_mi=NA,
      walk_duration_origin_min=NA,
      walk_dist_dest_mi=NA,
      walk_duration_dest_min=NA,
      wait_time_min = NA,
      num_transfers=NA,
      routes_used=NA,
      agency=NA,
      transit_modes=NA,
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
      total_dist_mi= NA,
      total_duration_min = NA,
      transport_time = NA,
      transit_dist_mi=NA,
      transit_duration_min=NA,
      total_walk_dist_mi = NA,
      total_walk_duration_min = NA,
      walk_dist_origin_mi=NA,
      walk_duration_origin_min=NA,
      walk_dist_dest_mi=NA,
      walk_duration_dest_min=NA,
      wait_time_min = NA,
      num_transfers=NA,
      routes_used=NA,
      agency=NA,
      transit_modes=NA,
    )
  input_type = 'coords'
}

startTime = Sys.time()

for(i in 1:nrow(query_frame)){
  
  
  if(input_type =='string'){
    oid = query_frame$origin_id[i]
    o_xy = query_frame$origin[i]
    d_xy = query_frame$dest[i]
    
    dirs = google_directions(origin = o_xy,
                             destination = d_xy,
                             mode = mode_sel,
                             departure = as.POSIXct(departure_datetime, tz = timezone),
                             arrival_time =  as.POSIXct(arrival_datetime, tz = timezone),
                             transit_mode = transit_mode,
                             transit_routing_preference = traffic_model_sel
                             )
    
  }else{
    oid = query_frame$origin_id[i]
    o_xy = as.numeric(c(query_frame$origin_lat[i],query_frame$origin_lon[i]))
    d_xy = as.numeric(c(query_frame$dest_lat[i],query_frame$dest_lon[i]))
    
    dirs = google_directions(origin = o_xy,
                             destination = d_xy,
                             mode = "transit",
                             departure_time =  as.POSIXct(departure_datetime, tz = timezone),
                             arrival_time =  as.POSIXct(arrival_datetime, tz = timezone),
                             transit_mode = transit_mode,
                             transit_routing_preference = traffic_model_sel
                             )
  }


  
  # shp= st_as_sf(gDirsToShape(dirs,mode="driving")) %>%
  #   mutate(home_id=oid)
  # shape_list[[i]]=shp 
  
  if(dirs$status=="OK"){
    total_duration_min <- round(dirs$routes$legs[[1]]$duration$value/60 %>% round(1), 1)
    total_dist_mi <- round(dirs$routes$legs[[1]]$distance$value*0.000621371,1)
    steps <- dirs$routes$legs[[1]]$steps[[1]]
    # first_step <- steps$steps[[1]]
    # final_step <- steps$steps[[length(steps)]]
    
    df <- data.frame(steps$travel_mode,steps$distance$value*0.000621371, steps$duration$value/60)
    df <- setNames(df, c("travel_mode","steps_dist_mi","steps_duration_min"))
    
    if(length(df)==1 & df$travel_mode[[1]]=='WALKING'){
      query_frame$walk_dist_origin_mi[i]=df$steps_dist_mi[[1]]
      query_frame$walk_duration_origin_min[i]=df$steps_duration_min[[1]]
    }else{
      query_frame$num_transfers[i] = nrow(df %>% filter(travel_mode =='TRANSIT'))-1
      
      if(df$travel_mode[[1]]=='WALKING'){
        query_frame$walk_dist_origin_mi[i]=df$steps_dist_mi[[1]]
        query_frame$walk_duration_origin_min[i]=df$steps_duration_min[[1]]
      }
      if(df$travel_mode[[nrow(df)]]=='WALKING'){
        query_frame$walk_dist_dest_mi[i]=df$steps_dist_mi[[nrow(df)]]
        query_frame$walk_duration_dest_min[i]=df$steps_duration_mi[[nrow(df)]]
      }
      if(nrow(df %>% filter(travel_mode=='WALKING'))>=1){
        query_frame$total_walk_dist_mi[i] = sum(df %>% filter(travel_mode=='WALKING') %>% select(steps_dist_mi))
        query_frame$total_walk_duration_min[i] = sum(df %>% filter(travel_mode=='WALKING') %>% select(steps_duration_min))
      }
      
    # transit dist mi
    if("TRANSIT" %in% df$travel_mode){
      query_frame$transit_dist_mi[i]=sum(df %>% filter(travel_mode=='TRANSIT') %>% select(steps_dist_mi))
      # transit duration min
      query_frame$transit_duration_min[i]=sum(df %>% filter(travel_mode=='TRANSIT') %>% select(steps_duration_min))
    }else{
      query_frame$transit_dist_mi[i] <- NA
      query_frame$transit_duration_min[i] <- NA
    }
    # query_frame$transit_dist_mi[i]=sum(df %>% filter(travel_mode=='TRANSIT') %>% select(steps_dist_mi))
    # # transit duration min
    # query_frame$transit_duration_min[i]=sum(df %>% filter(travel_mode=='TRANSIT') %>% select(steps_duration_min))
    # # walk dist mi
    # query_frame$total_walk_dist_mi[i] =sum(df %>% filter(travel_mode=='WALKING') %>% select(steps_dist_mi))
    # # walk duration min
    # query_frame$total_walk_duration_min[i] = sum(df %>% filter(travel_mode=='WALKING') %>% select(steps_duration_min))
    
    routes <- steps$transit_details$line$short_name
    route <- routes[!is.na(routes)]  
    route <- paste(route, collapse = '_')
    query_frame$routes_used[i]<-route
    
    modes <- steps$transit_details$line$vehicle$type
    mode <- modes[!is.na(modes)]  
    mode <- paste(mode, collapse = ',')
    query_frame$transit_modes[i]<-mode
    
    agencies<- steps$transit_details$line$agencies
    agencies <- lapply(agencies, function(x) x$name)
    agency <- do.call(c, agencies)
    agency<- paste(agency, collapse = ',')
    query_frame$agency[i]<-agency
    
    query_frame$total_dist_mi[i]<-total_dist_mi
    query_frame$total_duration_min[i]<-total_duration_min
    
    # fill NA with 0
    # query_frame[is.na(query_frame)] <- 0
    
    # transport time = total duration from origin to the destination without waiting time
    query_frame$transport_time[i] <- sum(df$steps_duration_min)
    
    # wait time = total duration - transport time (i.e., waiting time includes origin/dest walking time)
    query_frame$wait_time_min[i] <-total_duration_min- sum(df$steps_duration_min)
    
    currentTime = Sys.time()
    print(paste0(i," of ",nrow(query_frame)," complete, averaging ",round(as.numeric(difftime(currentTime,startTime,units="secs")/(i)),2),
                 " seconds per query.", " ",round(as.numeric(difftime(currentTime,startTime,units="min")),2)," minutes so far. Estimated ",
                 round(as.numeric(difftime(currentTime,startTime,units="min")/(i)),2)*(nrow(query_frame)-i)," minutes to go"))
    
    }
  }
}

# When done, write outputs---------------------------------------------------------------------------------------------------------------------
output_name <- file.path("output",paste(mode_sel, gsub(":", "_", departure_datetime), sep = '_'))

saveRDS(query_frame, paste0(output_name,'.rds'))
write.csv(query_frame, paste0(output_name,'.csv')) 
 

  

