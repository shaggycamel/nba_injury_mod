SELECT 
    log.player_id, 
    info.display_first_last AS player_name,
    (log.game_date - info.birthdate::DATE)::INT / 365 AS age,
    info.height, -- When cast to numeric 6.10 becomes 6.1. Which is incorrect 
    info.weight, 
    info.position, 
    log.year_season - info.from_year AS years_experience,
    log.year_season,
    log.slug_season,
    log.season_type,
    log.game_date,
    injury_data.date AS injury_date,
    CASE WHEN injury_data.notes IS NOT NULL THEN TRUE ELSE FALSE END AS injury_flag,
    log.game_id,
    injury_data.notes,
    log.matchup,
    log.wl,
    log.min,
    log.pf, 
    log.tov,  
    log.plus_minus,
--    log.fga,
--    log.fgm, 
--    log.fg_pct,
    log.fga - log.fg3_a AS fg2_a,
    (log.fgm - log.fg3_m) / CASE WHEN (log.fga - log.fg3_a) = 0 THEN 1 ELSE (log.fga - log.fg3_a) END AS fg2_pct,
    log.fg3_a,
--    log.fg3_m, 
    COALESCE(log.fg3_pct, 0) AS fg3_pct, 
    log.fta,
--    log.ftm,
    COALESCE(log.ft_pct, 0) AS ft_pct,
--    log.pts,
    log.oreb, 
    log.dreb, 
--    log.reb, 
    log.ast, 
    log.stl, 
    log.blk
    
FROM nba.player_game_log AS log

LEFT JOIN nba.player_info AS info ON log.player_id = info.person_id

-- This join duplicates injuries. An injury date that happens after back-to-back games
-- is being brought in twice. Ideally, find a technique that joins injury data on to
-- the last played game, regardless of game_date-injury_date difference.
LEFT JOIN (
    SELECT player, date, notes
    FROM nba.transaction_log
    WHERE LOWER(transaction_type) LIKE 'injur%'
        AND acc_req = 'Relinquished'
        
-- Need to create matchup_id for transaction data in fty_nba_id_matchup
) AS injury_data ON info.display_first_last = injury_data.player
    AND injury_data.date BETWEEN log.game_date AND log.game_date + 3

WHERE info.display_first_last IS NOT NULL
 --   AND player_id = 203954 -- Embiid
ORDER BY game_date