# Making the Player Data Table

data <- baseballr::bref_daily_batter("2023-04-10", '2023-08-01')

top_batter_ids <- data %>%
  select(bbref_id) %>%
  rename(key_bbref = bbref_id)

id_table <- baseballr::chadwick_player_lu() %>%
  filter(!is.na(key_mlbam))

names_ids <- id_table %>%
  mutate(name = paste(name_first, name_last)) %>%
  arrange(name_last) %>%
  select(key_bbref, name, key_mlbam) %>%
  filter(key_bbref != '') %>%
  rename(mlbam_id = key_mlbam) %>%
  inner_join(top_batter_ids) %>%
  select(name, mlbam_id)

write_rds(names_ids, "player_data.rds")

pitch_names <- 
  baseballr::statcast_search_batters(
    start_date = '2023-03-25',
    end_date = '2023-10-01',
    batterid = 660271
  )
  select(pitch_name) %>%
  filter(pitch_name != "") %>%
  unique() %>%
  arrange(pitch_name)

write_rds(pitch_names, "pitch_name_data.rds")
