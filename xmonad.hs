-- -*- coding: utf-8 -*-

-------------------------------------------------------------------------------
-- XMonad 設定ファイル（Ubuntu向け）
--
-- テンプレート
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Template_xmonad.hs_(0.9)
-------------------------------------------------------------------------------

import XMonad
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.WindowGo
import XMonad.Util.Run         (spawnPipe)
import XMonad.Util.EZConfig    (additionalKeysP)
import System.IO
import Graphics.X11.Xlib       (openDisplay)
import Graphics.X11.Xinerama   (xineramaQueryScreens, xsi_width)

-------------------------------------------------------------------------------
-- データ型
-------------------------------------------------------------------------------

-- | Xinerama スクリーン番号
type ScreenNum = Int

-- | Dzen2ステータスバーの幅。ピクセル数、または、％指定
data DzenWidth = Pixels Int | Percent Double


-------------------------------------------------------------------------------
-- 基本設定
-------------------------------------------------------------------------------

-- | メインスクリーンの番号
myMainScreen :: ScreenNum
myMainScreen  = 0

-- | ステータスバーの幅（％）
myStatusWidth :: DzenWidth
myStatusWidth = Percent 50.0

-- | Conkyバーの幅（％）
myConkyWidth :: DzenWidth
myConkyWidth  = Percent 40.0


-- | XMonadメイン
main :: IO ()
main = do
  -- gnome-settings-daemon を起動しルック＆フィールを設定
  spawn "gnome-settings-daemon"

  -- ステータスバーを表示
  statusWidth <- mkDzenWidth myMainScreen myStatusWidth
  conkyWidth  <- mkDzenWidth myMainScreen myConkyWidth
  status <- spawnPipe (myDzenStatus statusWidth)
  conky  <- spawnPipe (myDzenConky statusWidth conkyWidth)

  -- アプリを自動起動
  spawnMyApps

  -- XMonadoをカスタマイズ
  xmonad $ defaultConfig
        {
          -- 基本設定
          terminal = "gnome-terminal",     -- ターミナルを gnome-terminal に変更（デフォルトはxterm）
          modMask = mod4Mask,              -- Modキーを Windowsキー（Super_Lキー）に変更
          borderWidth = 3,                 -- ウィンドウの枠幅
          normalBorderColor  = "#dddddd",  -- ウィンドウの枠の色
          focusedBorderColor = "#3399ff",  -- ウィンドウの枠の色（アクティブなウィンドウ）

          -- フック関連
          manageHook = manageDocks <+> myManageHook
                                   <+> manageHook defaultConfig,
          layoutHook = avoidStruts $ layoutHook defaultConfig,
          logHook = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn status }

        } `additionalKeysP` myKeys


-------------------------------------------------------------------------------
-- アプリの自動起動
-------------------------------------------------------------------------------

-- | ログイン時に自動起動するアプリを指定します。
spawnMyApps :: IO()
spawnMyApps = do
  spawn ("trayer --edge top --align right --SetDockType true --SetPartialStrut false "
         ++ "--expand true --width 10 --transparent true --tint 0x000000 --height 20")
  spawn "gnome-power-manager"
  spawn "gnome-volume-control-applet"
  spawn "nm-applet --sm-disable"
  -- spawn "bluetooth-applet"
  spawn "nautilus --no-desktop -n"


-------------------------------------------------------------------------------
-- アプリごとのカスタマイズ
-------------------------------------------------------------------------------

-- | アプリごとのウィンドウズ動作をカスタマイズします。
--   アプリ名の調べ方：ターミナルで "xprop | grep WM_CLASS" と入力してから
--   　　　　　　　　　ウィンドウをマウスでクリック。
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Xmessage" --> doFloat,
      className =? "Do" --> doFloat,
      className =? "Skype" --> doFloat,
      className =? "Gnome-system-monitor" --> doFloat
    ]

-------------------------------------------------------------------------------
-- ショートカットキー
-------------------------------------------------------------------------------

-- | ショートカットキーをカスタマイズします。
-- | ・spawn はショートカットキーを押すごとにアプリを起動する。
-- | ・runOrRaise はもしアプリが走ってなかったら起動し、そのアプリのウィンドウにフォーカスする。
myKeys :: [(String, X())]
myKeys = [ ("M-p", spawn "gnome-do"),
           ("M-e", runOrRaise "emacs" (className =? "Emacs")),
           ("M-f", runOrRaise "firefox5" (className =? "Firefox"))
         ]

-------------------------------------------------------------------------------
-- ステータスバー関連の設定
-------------------------------------------------------------------------------

-- | Dzen2の起動コマンド
-- | パラメーターの詳細は http://dzen.geekmode.org/dwiki/doku.php?id=dzen:command-and-option-list
myDzenStatus :: Maybe Int -> String
myDzenStatus (Just width) = "dzen2 -w '" ++ (show width) ++ "' -ta 'l'"  ++ myDzenStyle

-- | Dzen2＋Conkyの起動コマンド
myDzenConky :: Maybe Int -> Maybe Int -> String
myDzenConky (Just position) (Just width) = "conky -c ~/.xmonad/conkyrc | "
                                           ++ "dzen2 -x '" ++ show position
                                           ++ "' -w '" ++ show width ++ "' -ta 'r'" ++ myDzenStyle

-- | Dzen共通のスタイル設定
myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222' -fn '-misc-fixed-medium-r-normal--14-*-*-*-*-*-*'"
-- dzen2がXFTをサポートしているときはアンチエイリアスフォントが使用できます。
-- myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222' -fn 'TakaoPGothic:bold:size=10'"

-- | Dzenに表示する項目のカスタマイズ
myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#3399ff" "" . wrap " " "",
      ppHidden  = dzenColor "#dddddd" "" . wrap " " "",
      ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " "",
      ppUrgent  = dzenColor "#ff0000" "" . wrap " " "",
      ppSep     = " ",
      -- ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space) [" "] ^ca()",
      ppLayout  = dzenColor "#aaaaaa" "" . wrap "[" "]",
      ppTitle   = dzenColor "#ffffff" "" . wrap " " "　" . shorten 80 . dzenEscape
    }


-------------------------------------------------------------------------------
-- ステータスバーで使用する関数（プログラム）
--
-- こちらを参考にしました。
-- https://github.com/pbrisbin/xmonad-config/blob/master/lib/Dzen.hs
-------------------------------------------------------------------------------

-- | ScreenNum s（0からスタート）の幅を返します。
--   スクリーンがない場合は0を返します。
screenWidth :: ScreenNum -> IO Double
screenWidth s = do
    dsp <- openDisplay ""
    mss <- xineramaQueryScreens dsp
    return $ case mss of
        Nothing -> 0
        Just [] -> 0
        Just ss -> if s >= 0 && s < length ss
            then fromIntegral . xsi_width $ ss !! s else 0


-- | DzenWidthを与えると、dzen2 の @-w@ や @-x@ パラメーターに
--   適した数字を返します。
mkDzenWidth :: ScreenNum -> DzenWidth -> IO (Maybe Int)
mkDzenWidth _ (Pixels x)  = return $ Just x
mkDzenWidth s (Percent c) = return . go =<< screenWidth s
    where
        go 0  = Nothing
        go sw = Just . round $ (c/100) * sw

