import System.Taffybar

import System.Taffybar.Battery
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather()
import System.Taffybar.MPRIS
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
                                  , graphLabel = Just "☡"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (1, 0, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "☣"
                                  }
  let clock = textClockNew Nothing "<span fgcolor='yellow'> ◴ %a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew defaultPagerConfig
                  { activeWindow     = colorize "#0C9D52" "" . escape . shorten 140
                  , activeLayout     = colorize"#F5F5F5" "" . escape
                  , activeWorkspace  = colorize "#4169E1" "" . escape
                  , hiddenWorkspace  = colorize "#78898D" "" . escape
                  , emptyWorkspace   = colorize "#34495E" "" . escape
--                  , visibleWorkspace = colorize "#a88500" "" . escape
                  , urgentWorkspace  = colorize "red" "yellow" . escape
                  , widgetSep        = " "
                  }
      note = notifyAreaNew defaultNotificationConfig
      mpris = mprisNew defaultMPRISConfig
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 1 cpuCallback
      tray = systrayNew
      battery = batteryBarNew  defaultBatteryConfig 25
      net_wlan = netMonitorNew 1.5 "wlan0"
      net_eth = netMonitorNew 1.5 "eth0"
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager, note ]
                                        , endWidgets = [ tray, clock,battery, net_wlan, net_eth, mem, cpu, mpris ]
                                        , monitorNumber = 1
                                        , barPosition = Bottom
                                          }
