test <- bind_rows(receiver1, 
                      receiver2, 
                      receiver3, 
                      receiver4, 
                      receiver5,
                      receiver6,
                      receiver7,
                      # receiver8, no data
                      receiver303,
                      receiver9,
                      receiver10,
                      receiver11,
                      receiver12,
                      receiver13,
                      receiver14,
                      receiver15,
                      receiver16,
                      receiver17,
                      receiver18,
                      receiver19,
                      receiver20) %>%
  clean_names %>% 
  mutate(transmitter_id = as.numeric(str_sub(transmitter, -5, -1))) %>% 
  filter(!is.na(transmitter_id)) # checked this. It's fine


test_data <- test %>%
  filter(transmitter_id < 49000 & transmitter_id >48000) %>% 
  mutate(date = ymd(as.Date(date_and_time_utc)),
         detected_by = as.character("stationary_receiver")) %>%  
  select(date, station_name,transmitter_id, detected_by) %>% 
  group_by(date, station_name, transmitter_id, detected_by) %>% 
  count() %>% 
  left_join(all_vemco_receivers)


test_data %>% 
  ggplot(aes(x = date, y = rkm)) + 
  geom_line(aes(group = transmitter_id)) 
