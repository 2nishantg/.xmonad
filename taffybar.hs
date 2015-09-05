import System.Taffybar

import System.Taffybar.Battery
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather()
import System.Taffybar.MPRIS2
import System.Taffybar.Pager
import System.Taffybar.Widgets.PollingBar()
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingLabel()
import System.Taffybar.NetMonitor
import System.Taffybar.WorkspaceSwitcher()
import System.Information.Memory
import System.Information.CPU

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main :: IO ()
main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(0, 1, 1, 1)]
                                  , graphLabel = Just "<span fgcolor='yellow'> ☳ </span>"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (1, 0, 0, 1)
                                                      , (1, 0, 1, 0.7)
                                                      ]
                                  , graphLabel = Just "<span fgcolor='red'>☣</span>"
                                  }
  let clock = textClockNew Nothing "<span fgcolor='orchid'> ◶ %a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew defaultPagerConfig
                  { activeWindow     = colorize "#4169E1" "" . escape . shorten 50
                  , activeLayout     = colorize "#DB7093" "" . escape
                  , activeWorkspace  = colorize "#8FBC8F" "" . escape
                  , hiddenWorkspace  = colorize "#78898D" "" . escape
                  , emptyWorkspace   = colorize "#34495E" "" . escape
--                , visibleWorkspace = colorize "#a88500" "" . escape
                  , urgentWorkspace  = colorize "red" "yellow" . escape
                  , widgetSep        = " "
                  }
---"#4169E1""#8FBC8F"
      note = notifyAreaNew defaultNotificationConfig
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 1 cpuCallback
      tray = systrayNew
      battery = batteryBarNew  defaultBatteryConfig 25
      myNetFormat = "<span fgcolor='seagreen'>▼ $inKB$kb/s</span><span fgcolor='crimson'> ▲ $outKB$kb/s</span>"
      net_wlan = netMonitorNewWith 1.5 "wlan0" 2 myNetFormat
      net_eth = netMonitorNewWith 1.5 "eth0" 2 myNetFormat
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager, note ]
                                        , endWidgets = [ tray, clock,battery, net_wlan, net_eth, mem, cpu, mpris ]
                                        , monitorNumber = 1
                                        , barPosition = Bottom
                                          }
