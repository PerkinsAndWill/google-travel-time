library(tidyverse)
library(readxl)
library(openxlsx)

input = read_excel("input.xlsx")

od_pair <- expand.grid(input$LabelID, input$LabelID) %>% 
  rename(Origin_ID = Var2, Dest_ID = Var1) %>% 
  relocate(Origin_ID, .before = Dest_ID) %>% 
  filter(Origin_ID != Dest_ID) %>% 
  mutate(`OD Pair` = paste(Origin_ID, Dest_ID, sep = "_"))


od_pair_input <- od_pair %>% 
  left_join(input, by=c("Origin_ID" = "LabelID")) %>% 
  rename(Origin_lon = Lon, 
         Origin_lat = Lat, 
         Origin_address = Address) %>% 
  left_join(input, by=c("Dest_ID" = "LabelID")) %>% 
  rename(Dest_lon = Lon, 
         Dest_lat = Lat,
         Dest_address = Address) %>% 
  relocate(Dest_address, .before = Origin_lon)

write.xlsx(od_pair_input,"output/input_for_travel_time.xlsx")

