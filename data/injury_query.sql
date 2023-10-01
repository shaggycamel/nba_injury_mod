SELECT 
    log.player_id, 
    info.display_first_last AS player_name,
    log.year_season,
    log.slug_season,
    log.season_type,
    log.game_date,
    injury_data.date AS injury_date,
    log.game_id,
    injury_data.notes,
    log.matchup,
    log.wl,
    log.min,
    log.tov,  
    log.plus_minus,
    log.pf, 
    log.fga,
    log.fgm, 
    log.fg_pct, 
    log.fg3_a,
    log.fg3_m, 
    log.fg3_pct, 
    log.fta,
    log.ftm,
    log.ft_pct,
    log.pts,
    log.oreb, 
    log.dreb, 
    log.reb, 
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

-- WHERE player_id = 203954 -- Embiid
ORDER BY player_id DESC, game_date