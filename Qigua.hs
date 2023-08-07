{-#LANGUAGE OverloadedStrings #-}
import qualified Lunartime as LT
import Fmt
import Data.List (elemIndices)
import Control.Monad

bagua :: [[Char]]
bagua = ["乾","兑","离","震","巽","坎","艮","坤"]

sixty4 :: [[Char]]
sixty4 = ["天","履","同人","无妄","姤","讼","遁","否","夬","泽","革","随","大过","困","咸","萃","大有","睽","火","噬瞌","鼎","未济","旅","晋","大壮","归妹","丰","雷","恒","解","小过","豫","小畜","中孚","家人","益","风","涣","渐","观","需","节","即济","屯","井","水","骞","比","大畜","损","贲","姬","盅","蒙","山","剥","泰","临","明夷","复","升","师","谦","地"]
calculateGua y m d t = do
    let shangguashu | rem (y+m+d) 8 ==0 = 8
                    | otherwise = rem (y+m+d) 8
    let shanggua = bagua !! (shangguashu-1)
    let xiaguashu | rem (y+m+d+t) 8 ==0 = 8
                  | otherwise = rem (y+m+d+t) 8
    let xiagua = bagua !! (xiaguashu-1)
    let dongyaoshu = rem (y+m+d+t) 6
    return (shanggua,xiagua,dongyaoshu)

gen64gua :: [[Char]]
gen64gua = [x++y | x<-bagua, y<-bagua]

print64gua :: [([Char],[Char])] -> IO ()
print64gua xs = forM_ [0..length xs -1] $ \x-> do
                    putStr $ fst (xs !! x) ++ "->" ++ snd (xs !! x) ++ "；"


sixtyGuaXiang = zip sixty4 gen64gua

findGua str = filter (\x-> snd x == str)

main = do
    LT.showCurrentTime
    --print $ length gen64gua
    --print64gua sixtyGuaXiang
    --print "\n"
    nzhi <- LT.getNianZhi
    let inzhi = elemIndices nzhi LT.ndizhi
    let ly = head inzhi +1
    --print ly
    print "Please enter lunar month: "
    m <- getLine
    let lm = read m :: Int
    print "Please enter lunar date: "
    d <- getLine
    let ld = read d :: Int
    --print ld
    (_,sz,lt) <- LT.getShiGanZhi
    --print lt
    putStr "起卦时辰要素为：年支-"
    putStr $ nzhi ++ " 月份数-" ++ show lm ++ " 日期数-" ++ show ld ++ " 时支-"
    putStr $ sz ++ "\n"
    (s,x,d) <- calculateGua ly lm ld lt
    putStr "上卦为："
    putStr $ s ++ ", 下卦为："
    putStr $ x ++ ", 变爻为：" ++ show d ++ "\n"
    putStr "这是 "
    putStr $ fst $ head $ findGua (s++x) sixtyGuaXiang
    putStr "卦\n"
    