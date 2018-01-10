# 必要データの作成
library(pitchRx2)
library(xml2)
library(tidyverse)
library(DBI)
library(animation)
# ↓30分くらいかかります
# gids <- get_gids(start_year = 2014, end_year = 2017)
# あらかじめ用意してあるgame_idsをお使いください
head(game_ids)
# ヤンキースのデータに絞ります
nya <- game_ids %>% str_subset("201[4-7].*nya.*")
# スクレイピングしたデータの格納先を準備
db <- src_sqlite("masahiro_tanaka.sqlite3", create = T)
# suffix <- c("miniscoreboard.xml", "players.xml", "inning/inning_hit.xml")
scrape(game.ids = nya, connect = db$con, suffix = "inning/inning_all.xml")
# 得られたテーブルを確認
db_list_tables(db$con)
# "action" "atbat" "pitch" "po" "runner"
