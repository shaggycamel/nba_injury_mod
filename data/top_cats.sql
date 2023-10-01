SELECT  
  season_stats.player_id,
  info.display_first_last AS player_name,
  season_stats.min,
  --    season_stats.fga,
  --    season_stats.fgm, 
  --    season_stats.fg_pct,
  season_stats.fga - season_stats.fg3_a AS fg2_a,
  COALESCE((season_stats.fgm - season_stats.fg3_m) / CASE WHEN (season_stats.fga - season_stats.fg3_a) = 0 THEN 1 END, 0) AS fg2_pct,
  season_stats.fg3_a,
  --    season_stats.fg3_m, 
  COALESCE(season_stats.fg3_pct, 0) AS fg3_pct,
  season_stats.fta,
  --    season_stats.ftm,
  COALESCE(season_stats.ft_pct, 0) AS ft_pct,
  --    season_stats.pts,
  season_stats.oreb, 
  season_stats.dreb, 
  --    season_stats.reb, 
  season_stats.ast, 
  season_stats.stl, 
  season_stats.blk

FROM nba.player_season_stats AS season_stats
LEFT JOIN nba.player_info AS info ON season_stats.player_id = info.person_id
WHERE season_stats.season_id = '2022-23'