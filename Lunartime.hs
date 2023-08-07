{-# LANGUAGE OverloadedStrings #-}
module Lunartime where 

import Data.Time.Clock ( getCurrentTime, UTCTime (UTCTime) )
import Data.Time.LocalTime ( getZonedTime, zonedTimeToUTC, ZonedTime (ZonedTime, zonedTimeToLocalTime), LocalTime (localDay) )
import qualified Data.Text as T
import Data.Time
import Control.Applicative
import Fmt
import Data.Char (digitToInt)
import Data.Fixed 
import Data.List

tiangan = ["甲","乙","丙","丁","戊","己","庚","辛","壬","癸"]
ndizhi = ["寅","卯","辰","巳","午","未","申","酉","戌","亥","子","丑"]
ydizhi = ["子","丑","寅","卯","辰","巳","午","未","申","酉","戌","亥"]

mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))

baseDay = mkUTCTime (2020, 1, 1) (0, 0, 0)

getNianGan = do
    (y,_,_,_,_) <- getCurrentTimeInt
    let ng | lsd <=3 = lsd-3+10
           | otherwise = lsd-3
           where lsd = digitToInt $ last $ show y
    return $ tiangan !! (ng-1)

getNianZhi = do
    (y,_,_,_,_) <- getCurrentTimeInt
    let nz | yushu ==0 = 12
           | otherwise = yushu
           where lsd = read $ show y :: Int
                 yushu =  rem (lsd+7) 12
    return $ ndizhi !! (nz-1)

getYueGan = do
    (y,m,d,_,_) <- getCurrentTimeInt
    let yg = rem ((rem (lsyd-4) 5)*12 +m) 10
           where lsyd = read $ show y :: Int
    if d < 15 then return $ tiangan !! (yg-1)
    else return $ tiangan !! yg

getYueZhi = do
    (_,m,d,_,_) <- getCurrentTimeInt
    if d < 15 then return $ ydizhi !! (m-1)
    else return $ ydizhi !! m

getRiGanZhi = do
    zt <- getZonedTime
    let ld = localDay $ zonedTimeToLocalTime zt
    let days = diffDays ld (utctDay baseDay)
    let yushu = read (show (rem (days+40) 60)) :: Int
    let riGan = tiangan !! ((rem yushu 10) -1)
    let riZhi | (yushu - 12) >0 = ydizhi !! (yushu - 12 -1)
              | (yushu - 12) ==0 = ydizhi !! (yushu - 1)
              | otherwise = ydizhi !! (yushu -1)
    return (riGan ++ riZhi)
    

getShiGanZhi :: IO ([Char], [Char], Int)
getShiGanZhi = do
    zt <- getZonedTime
    let ld = localDay $ zonedTimeToLocalTime zt
    let days = diffDays ld (utctDay baseDay)
    let yushu = read (show (rem days 5)) :: Int
    (_,_,_,h,_) <- getCurrentTimeInt
    let shiZhiShu = div (h+3) 2
    let shiZhi = ydizhi !! (shiZhiShu -1)
    let shiGanShu | rem (yushu*12 + 8 + shiZhiShu) 10 ==0 = 10
                  | otherwise = rem (yushu*12 + 8 + shiZhiShu) 10
    let shiGan = tiangan !! (shiGanShu -1)
    return (shiGan, shiZhi, shiZhiShu)

getCurrentTimeInt = do
    zt <- getZonedTime
    let (y,m,d) = toGregorian $ localDay $ zonedTimeToLocalTime zt
    let LocalTime ltd ltdt = zonedTimeToLocalTime zt
    let h = todHour ltdt
    let mn = todMin ltdt
    return (y,m,d,h,mn)

showCurrentTime = do
    (y,m,d,h,mn) <- getCurrentTimeInt
    ngan <- getNianGan
    let ingan = elemIndices ngan tiangan
    nzhi <- getNianZhi
    let inzhi = elemIndices nzhi ndizhi
    ygan <- getYueGan
    let iygan = elemIndices ygan tiangan
    yzhi <- getYueZhi
    let iyzhi = elemIndices yzhi ydizhi
    rgz <- getRiGanZhi
    (sg,sz,_) <- getShiGanZhi
    fmt $ "Your current time is: "+|y|+"年("+|ngan++nzhi|+")"+|m|+"月("+|ygan++yzhi|+")"+|d|+"日("+|rgz|+")"+|h|+"时("+|sg++sz|+")"+|mn|+"分\n"
    --fmt $ "Lunar year in digit is: 年干" +|head ingan|+" 年支"+|head inzhi|+" 月干"+|head iygan|+" 月支"+|head iyzhi|+"\n"

--main = do
--    showCurrentTime
    