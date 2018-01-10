# devtools::install_github("pontsuyu/pitchRx2")
# 必要なパッケージの読み込み
library(pitchRx2)
library(xml2)
library(tidyverse)
library(DBI)
# 2008〜2017年のgameidはあらかじめ用意してあるgame_idsをお使いください
# ヤンキースのデータに絞ります
nya <- game_ids %>% str_subset("201[4-7].*nya.*")
# スクレイピングしたデータの格納先を準備
db <- src_sqlite("nya.sqlite3", create = T)
# スクレイピングの実行
scrape(game.ids = nya, connect = db$con, suffix = "inning/inning_all.xml")
# テーブルを確認
db_list_tables(db$con)
# "action" "atbat" "pitch" "po" "runner"

# 2回目以降はこちらを実行
db <- src_sqlite("R/nya.sqlite3", create = F)
# データの抽出
dat <- dbGetQuery(db$con, "SELECT
                  pitcher_name, -- 投手名
                  p_throws, -- 投手の利き腕
                  batter_name, -- 打者名
                  stand, -- 打者の立ち位置
                  pitch_type, -- 球種
                  start_speed, -- 初速
                  end_speed, -- 終速
                  spin_rate, -- 回転数
                  spin_dir, -- 回転軸
                  px, pz, -- 投球ロケーション
                  x0, y0, z0, -- リリースポイント
                  vx0, vy0, vz0, -- 速度
                  ax, ay, az, -- 加速度
                  b_height, -- バッターの身長(feet-inch)
                  break_angle, -- 変化角
                  break_length, -- 変化量
                  pit.num as event_num, -- 試合ごとのイベント番号
                  pit.des, -- 投球結果
                  type,  -- 簡易投球結果
                  event, -- 打席結果
                  atb.date, -- 日時
                  atb.url
                  FROM
                  atbat atb, -- 打席テーブル
                  pitch pit -- 投球データテーブル
                  WHERE atb.url = pit.url -- スクレイピング先url
                  AND atb.inning = pit.inning -- イニング
                  AND atb.inning_side = pit.inning_side -- 表/裏
                  AND atb.gameday_link = pit.gameday_link -- gameid
                  AND atb.next_ = pit.next_ -- 次打者の有無
                  AND atb.num = pit.num -- イベント番号
                  -- キャンプ/プレーオフのデータは除外する処理
                  AND ((CAST(REPLACE(atb.date, '_', '') as NUMBER)  BETWEEN 20140322 AND 20160928)
                  OR   (CAST(REPLACE(atb.date, '_', '') as NUMBER) BETWEEN 20150406 AND 20151004)
                  OR   (CAST(REPLACE(atb.date, '_', '') as NUMBER) BETWEEN 20160403 AND 20161002)
                  OR   (CAST(REPLACE(atb.date, '_', '') as NUMBER) BETWEEN 20170402 AND 20171001))
                  AND atb.pitcher_name is not null
                  AND pit.pitch_type is not null
                  ;")
sort(unique(dat$pitcher_name))
# 見たいピッチャーの名前を入力
p_name <- "Masahiro Tanaka"
pitch <- dat %>% 
         filter(pitcher_name==p_name) %>% 
         separate(date, c("year", "month", "day"), sep = "_")
# 球種の割合
pt <- pitch %>% 
      group_by(year, stand, pitch_type) %>% 
      summarise(N = n()) %>% 
      group_by(year, stand) %>% 
      mutate(per = N/sum(N)*100) %>% 
      inner_join(pitch_type, by = "pitch_type") %>% 
      arrange(year, pitch_type_name) %>% 
      mutate(row = row_number()) %>% 
      arrange(desc(row)) %>% 
      group_by(year, stand) %>% 
      mutate(cumsum = cumsum(per) - 0.5 * per,
             year_N = paste0(stand, "_", year, "(N=", sum(N), ")")) %>% 
      ungroup %>% 
      as.data.frame
# 年別打者位置ごとの球種割合
ggplot(pt, aes(year_N, per, fill = pitch_type_name)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(per,digits = 2), y = cumsum), size = 3) +
  xlab("stand_year_pitching-N") + ylab("percent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(animation)
x <- list(facet_grid(stand ~ year, labeller = label_both), coord_equal())
# アニメーションにして出力
saveHTML({animateFX(pitch, layers = x, point.alpha = 0.3)}, clean = T,
         interval = 0.1, htmlfile = paste0(p_name, ".html"), 
         verbose = FALSE, ani.height = 600, ani.width = 800)

atbat <- pitch %>% 
         select(year, month, day, stand, event_num, event, url) %>% 
         distinct

res <-   atbat %>% 
         group_by(year, event) %>% 
         summarise(N = n()) %>% 
         spread(year, N, fill = 0)

tmp <- pitch %>% 
       filter(type=="X", event %in% c("Double", "Triple", "Home Run"))
saveHTML({animateFX(tmp, layers = x, point.alpha = 0.5)}, clean = T,
         interval = 0.5, htmlfile = paste0(p_name, "_batting.html"), 
         verbose = FALSE, ani.height = 600, ani.width = 800)




