library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(nflseedR)
library(Cairo)
library(gt)


pbp = read_csv("coding-projects/nfl-fast-r/pbp-df-thru23")[, -1]

games = load_schedules()


# Preliminary


avg_def_epa = pbp |> group_by(season) |> summarise(mean_epa_allowed_season = mean(mean_epa_allowed))
avg_off_epa = pbp |> group_by(season) |> summarise(mean_epa_season = mean(mean_epa))

off_epa = pbp |> select(season, team, mean_epa) |> arrange(season, team)
def_epa = pbp |> select(season, team, mean_epa_allowed) |> arrange(season, team)

seasons_raw = pbp |> select(season) |> pull()
teams_raw = pbp |> select(team) |> pull()



# Offense


off_adj_raw = numeric(797)


for (i in 1:797) { 
  off_adj_raw[i] = games |> 
    filter(season == seasons_raw[i]) |> 
    select(season, home_team, away_team) |> 
    mutate(home_team = clean_team_abbrs(home_team),
           away_team = clean_team_abbrs(away_team)) |> 
    filter(home_team == teams_raw[i] | away_team == teams_raw[i]) |> 
    mutate(opp_team = if_else(home_team == teams_raw[i], away_team, home_team)) |> 
    left_join(def_epa, by = c("opp_team" = "team", "season")) |> 
    summarise(mean(mean_epa_allowed))
}



# Defense


def_adj_raw = numeric(797)


for (i in 1:797) { 
  def_adj_raw[i] = games |> 
    filter(season == seasons_raw[i]) |> 
    select(season, home_team, away_team) |> 
    mutate(home_team = clean_team_abbrs(home_team),
           away_team = clean_team_abbrs(away_team)) |> 
    filter(home_team == teams_raw[i] | away_team == teams_raw[i]) |> 
    mutate(opp_team = if_else(home_team == teams_raw[i], away_team, home_team)) |> 
    left_join(off_epa, by = c("opp_team" = "team", "season")) |>  
    summarise(mean(mean_epa))
}



sos_df = pbp |>
  mutate(off_sos = off_adj_raw |> as_vector() |> as.double(),
         def_sos = def_adj_raw |> as_vector()) |> 
  left_join(avg_off_epa, by = "season") |> 
  left_join(avg_def_epa, by = "season") |> 
  mutate(adj_mean_epa = mean_epa - (off_sos - mean_epa_allowed_season),
         adj_mean_epa_allowed = mean_epa_allowed - (def_sos - mean_epa_season)) |> 
  filter(season == 2023)

sos_df |> arrange(adj_mean_epa_allowed)

ggplot(sos_df, aes(x = off_sos, y = def_sos)) +
  labs(x = "Offesnive SOS",
       y = "Defensive SOS",
       title = "NFL Strength of Schedule (2023)",
       subtitle = "top right = hard  |  bottom left = easy  |  strength of schedule = average opponents' 23' efficiency",
       caption = "By: Sam Burch  |  Data @nflfastR") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 6),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  scale_x_reverse() +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
  nflplotR::geom_mean_lines(aes(x0 = off_sos, y0 = def_sos))

# ggsave("team-sos.png", width = 16, height = 9, units = "cm")



# Offensive Performance

ggplot(sos_df, aes(x = adj_mean_epa, y = success_rate)) +
  labs(x = 'EPA/play',
       y = 'Success Rate',
       title = "NFL Offensive Performances (2023)",
       subtitle = "EPA/play adjusted for SOS",
       caption = 'By: Sam Burch  |  Data @nflfastR') +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
  nflplotR::geom_mean_lines(aes(x0 = adj_mean_epa, y0 = success_rate))

# ggsave("off_performance.png", width = 16, height = 9, units = "cm")



# Defensive Performance

ggplot(sos_df, aes(x = adj_mean_epa_allowed, y = success_rate_allowed)) +
  labs(x = 'EPA/play Allowed',
       y = 'Success Rate Allowed',
       title = "NFL Defensive Performances (2023)",
       subtitle = "EPA/play adjusted for SOS",
       caption = 'By: Sam Burch  |  Data @nflfastR') +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
  nflplotR::geom_mean_lines(aes(x0 = mean_epa_allowed, y0 = success_rate_allowed))

# ggsave("def_performance.png", width = 16, height = 9, units = "cm")




# Old ---------------------------------------------------------------------



# pbp_23 = load_pbp(2023)
# games_23 = load_schedules(2023)
# 
# 
# # Average efficiency
# ## DEF
# avg_def_epa = pbp_23 |> 
#   filter(!is.na(yards_gained), (pass == 1 | rush == 1)) |> 
#   group_by(defteam) |> 
#   summarise(mean_epa = mean(epa),
#             success_rate = mean(success),
#             ypa = mean(yards_gained),
#             plays = n()) |> 
#   select(mean_epa) |> 
#   pull() |> 
#   mean()
# 
# ## OFF
# avg_off_epa = pbp_23 |> 
#   filter(!is.na(yards_gained), (pass == 1 | rush == 1)) |> 
#   group_by(posteam) |> 
#   summarise(mean_epa = mean(epa),
#             success_rate = mean(success),
#             ypa = mean(yards_gained),
#             plays = n()) |> 
#   select(mean_epa) |> 
#   pull() |> 
#   mean()
# 
# 
# # OFF efficiency of each team
# off_23 = pbp_23 |> 
#   filter(!is.na(yards_gained), (pass == 1 | rush == 1)) |> 
#   group_by(posteam) |> 
#   summarise(mean_epa = mean(epa),
#             success_rate = mean(success),
#             ypa = mean(yards_gained),
#             plays = n()) |> 
#   arrange(-mean_epa)
# 
# 
# 
# # DEF efficiency of each team
# def_23 = pbp_23 |> 
#   filter(!is.na(yards_gained), (pass == 1 | rush == 1)) |> 
#   group_by(defteam) |> 
#   summarise(mean_epa = mean(epa),
#             success_rate = mean(success),
#             ypa = mean(yards_gained),
#             plays = n()) |> 
#   arrange(mean_epa)
# 
# 
# 
# 
# # SOS ---------------------------------------------------------------------
# 
# # Offense
# 
# nfl_teams = off_23 |> 
#   select(posteam) |> 
#   arrange(posteam) |> 
#   pull()
# 
# off_adj_raw = numeric(32)
# 
# 
# 
# for (i in 1:32) {
#   opp_epa = games_23 |> 
#     select(home_team, away_team) |> 
#     filter(home_team == nfl_teams[i] | away_team == nfl_teams[i]) |> 
#     mutate(opp_team = if_else(home_team == nfl_teams[i], away_team, home_team)) |> 
#     left_join(def_23 |> select(defteam, mean_epa), by = c("opp_team" = "defteam")) |> 
#     mutate(adj_mean_epa = avg_def_epa - mean_epa) |> 
#     summarise(mean(adj_mean_epa))
#   
#   off_adj_raw[i] = opp_epa
#   
# }
# 
# off_23_2 = off_23 |> 
#   mutate(off_sos = off_adj_raw |> as_vector()) |> 
#   mutate(adj_mean_epa = mean_epa + off_sos) |> 
#   arrange(-adj_mean_epa) |> 
#   rename(team = posteam,
#          off_epa = mean_epa,
#          off_sr = success_rate,
#          adj_off_epa = adj_mean_epa) |> 
#   select(team, off_epa, off_sr, off_sos, adj_off_epa)
# 
# off_23_2 |> arrange(-off_sos)
# 
# # Defense
# 
# 
# def_adj_raw = numeric(32)
# 
# for (i in 1:32) {
#   opp_epa = games_23 |> 
#     select(home_team, away_team) |> 
#     filter(home_team == nfl_teams[i] | away_team == nfl_teams[i]) |> 
#     mutate(opp_team = if_else(home_team == nfl_teams[i], away_team, home_team)) |> 
#     left_join(off_23 |> select(posteam, mean_epa), by = c("opp_team" = "posteam")) |> 
#     mutate(adj_mean_epa = avg_off_epa - mean_epa) |> 
#     summarise(mean(adj_mean_epa))
#   
#   def_adj_raw[i] = opp_epa
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# def_23_2 = def_23 |> 
#   mutate(def_sos = def_adj_raw |> as_vector()) |> 
#   mutate(adj_mean_epa = mean_epa + def_sos) |> 
#   arrange(adj_mean_epa) |> 
#   rename(team = defteam,
#          def_epa = mean_epa,
#          def_sr = success_rate,
#          adj_def_epa = adj_mean_epa) |> 
#   select(team, def_epa, def_sr, def_sos, adj_def_epa)
# 
# 
# 
# # Adjusting Team Strengths ------------------------------------------------
# 
# 
# 
# adj_power_rankings = off_23_2 |> 
#   left_join(def_23_2, by = "team") |> 
#   mutate(almost = .6*scale(adj_off_epa) - .4*scale(adj_def_epa)) |> 
#   mutate(almost_prev = .6*scale(off_epa) - .4*scale(def_epa)) |> 
#   mutate(ranking = scale(almost) * 
#            (20 / (max(scale(almost)) - min(scale(almost))))) |> 
#   mutate(ranking_prev = scale(almost_prev) * 
#            (20 / (max(scale(almost_prev)) - min(scale(almost_prev))))) |>
#   mutate(sos_diff = ranking - ranking_prev) |> 
#   arrange(-ranking) |> 
#   select(team, ranking, sos_diff, everything())
# 
# adj_power_rankings |> 
#   arrange(-sos_diff)
# 
# # Improved Rankings
# ggplot(full_df |> filter(season == 2023), aes(y = reorder(team, ranking), x = ranking)) +
#   labs(
#     title = 'NFL Power Rankings (2023)',
#     subtitle = "based on SOS adjusted EPA/play  |  60-40 split for offense-defense  |  assumes 20-point spread from worst and best team",
#     caption = 'By: Sam Burch  |  Data @nflfastR',
#     x = "Projected Spread against an Average Team"
#   ) +
#   theme(
#     plot.subtitle = element_text(size = 6, hjust = .5),
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.x = element_line(color = "lightgray", size = 0.5, linetype = 2),
#     panel.grid.minor.x = element_blank(),
#     panel.background = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank()) +
#   scale_x_continuous(breaks = seq(-15, 15, 1)) +
#   geom_col(aes(color = team, fill = team), alpha = .8, width = 1) +
#   nflplotR::scale_fill_nfl(alpha = .3) +
#   nflplotR::scale_color_nfl(type = "secondary") +
#   nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .03, alpha = .8)
# 
# 
# # ggsave("power_rankings_23.png", width = 16, height = 12, units = "cm")
# 
# 
# # Offensive Performance
# 
# ggplot(adj_power_rankings, aes(x = adj_off_epa, y = off_sr)) +
#   labs(x = 'EPA/play',
#        y = 'Success Rate',
#        title = "NFL Offensive Performances (2023)",
#        subtitle = "EPA/play adjusted for SOS",
#        caption = 'By: Sam Burch  |  Data @nflfastR') +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5, size = 8),
#     axis.line = element_line(color = "black", size = 0.5),
#     panel.grid = element_blank(),
#     panel.background = element_blank()
#   ) +
#   nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
#   stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
#   nflplotR::geom_mean_lines(aes(x0 = adj_off_epa, y0 = off_sr))
# 
# # ggsave("off_performance.png", width = 16, height = 9, units = "cm")
# 
# 
# 
# # Defensive Performance
# 
# ggplot(adj_power_rankings, aes(x = adj_def_epa, y = def_sr)) +
#   labs(x = 'EPA/play Allowed',
#        y = 'Success Rate Allowed',
#        title = "NFL Defensive Performances (2023)",
#        subtitle = "EPA/play adjusted for SOS",
#        caption = 'By: Sam Burch  |  Data @nflfastR') +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5, size = 8),
#     axis.line = element_line(color = "black", size = 0.5),
#     panel.grid = element_blank(),
#     panel.background = element_blank()
#   ) +
#   scale_x_reverse() +
#   scale_y_reverse() +
#   nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
#   stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
#   nflplotR::geom_mean_lines(aes(x0 = adj_def_epa, y0 = def_sr))
# 
# # ggsave("def_performance.png", width = 16, height = 9, units = "cm")
# 




