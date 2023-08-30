-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Widgets
local volumecfg = require("ui.wibar.scripts.volume-control")({})

-- Autostarting
awful.util.spawn("/home/delta/.config/awesome/autostart.sh")

-- ** Notifications and error handleling ** --
require("ui.notifications")

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.init("/home/delta/.config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "alacritty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor

-- Default mod.
local mod = "Mod4"
local alt = "Mod1"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.floating,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.max,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({
    image = beautiful.awesome_icon,
    menu = mymainmenu
})

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Wibar Config
require("ui.wibar")

-- {{{ Mouse bindings

local rofi = "rofi -modi combi -show combi -display-combi 'Plutus ~\u{f054}' -combi-modi run,drun"
local volume = "/home/delta/.config/dunst/notif_scripts/volume_notif.sh"
local scriptsPath = "/home/delta/.config/scripts/"

root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
    -- awful.button({ }, 4, awful.tag.viewnext),
    -- awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(

    -- Restart and quit, in that order
    awful.key({ mod, "Control" }, "r", awesome.restart),
    awful.key({ mod, "Control"   }, "q", awesome.quit),

    -- Basic stuff
    awful.key({ mod }, "Return", function () awful.spawn(terminal) end),
    awful.key({ mod }, "b", function () awful.spawn("brave") end),
    awful.key({ mod }, "f", function () awful.spawn("pcmanfm") end),
    awful.key({ mod }, "p", function () awful.spawn(rofi) end),
    awful.key({ mod, "Shift" }, "s", function () awful.spawn("flameshot gui") end),
    -- awful.key({ alt }, "Tab", function () awful.spawn("rofi -modi window -show window run") end),

    -- Multimedia keys
    awful.key({ }, "XF86AudioRaiseVolume", function () volumecfg:up() end),
    awful.key({ }, "XF86AudioLowerVolume", function () volumecfg:down() end),

    awful.key({ mod }, "space", function () awful.spawn(scriptsPath .. "keyboardChanger.sh") end),

    -- Switching between windows
    awful.key({ mod }, "j", function () awful.client.focus.byidx( 1) end),
    awful.key({ mod }, "k", function () awful.client.focus.byidx(-1) end),

    -- Layout manipulation
    awful.key({ mod, "Shift" }, "j", function () awful.client.swap.byidx( 1) end),
    awful.key({ mod, "Shift" }, "k", function () awful.client.swap.byidx(-1) end),
    awful.key({ mod }, "u", awful.client.urgent.jumpto),
    awful.key({ alt }, "Tab", function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Window size manipulation
    awful.key({ mod }, "l", function () awful.tag.incmwfact( 0.05) end),
    awful.key({ mod }, "h", function () awful.tag.incmwfact(-0.05) end),

    -- Switching between layouts
    awful.key({ mod }, "Tab", function () awful.layout.inc( 1) end),
    awful.key({ mod, "Shift" }, "Tab", function () awful.layout.inc(-1) end),

    -- Moving between workspaces
    awful.key({ mod }, "Left",   awful.tag.viewprev),
    awful.key({ mod }, "Right",  awful.tag.viewnext),
    awful.key({ mod }, "Escape", awful.tag.history.restore),

    -- Idk
    awful.key({ mod, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              {description = "restore minimized", group = "client"})
)

clientkeys = gears.table.join(
    awful.key({ mod, "Control" }, "m", function (c)
                c.fullscreen = not c.fullscreen
                c:raise()
            end),
    awful.key({ mod, "Shift" }, "c", function (c) c:kill() end),
    awful.key({ mod, "Control" }, "v",  awful.client.floating.toggle),

    -- Focus master
    awful.key({ mod, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),

    -- Make the focused window to stay on top
    awful.key({ mod, "Control" }, "t", function (c) c.ontop = not c.ontop end),

--    awful.key({ mod }, "n",
--        function (c)
--            -- The client currently has the input focus, so it cannot be
--            -- minimized, since minimized clients can't have the focus.
--            c.minimized = true
--        end),

    -- Three ways of maximizing windows. Just XD
    awful.key({ mod }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end),

    --awful.key({ mod, "Control" }, "m",
    --    function (c)
    --        c.maximized_vertical = not c.maximized_vertical
    --        c:raise()
    --    end),

    awful.key({ mod, "Shift" }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ mod }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ mod, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ mod, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ mod, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ mod }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ mod }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
          "pinentry",
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "discord",
          "Spotify",
          "joplin",
          "Joplin",
          "joplin-desktop",
          "Joplin-desktop",
          "veromix",
          "xtightvncviewer"},

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "ConfigManager",  -- Thunderbird's about:config.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },
    { rule_any = { class = { "discord", "joplin", "Spotify" } },
        properties = { placement = awful.placement.centered }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- No border when maximized
client.connect_signal("property::maximized", function (c)
    c.border_width = c.maximized and 0 or beautiful.border_width
end)

-- Arrange signal handler
for s = 1, screen.count() do screen[s]:connect_signal("arrange",
  function ()
    local clients = awful.client.visible(s)
    local layout  = awful.layout.getname(awful.layout.get(s))

    if #clients > 0 then -- Fine grained borders and floaters control
      for _, c in pairs(clients) do -- Floaters always have borders
        if awful.client.floating.get(c) or layout == "floating" then
          c.border_width = beautiful.border_width

        -- No borders with only one [visible] client
        -- Gap single - Maximized
        elseif not beautiful.gap_single_client and #clients == 1 then
          c.border_width = 0
        else
          c.border_width = beautiful.border_width
        end
      end
    end
  end)
end

-- Titlebars
require("ui.titlebar.titlebar")

-- Enable sloppy focus, so that focus follows mouse.
--client.connect_signal("mouse::enter", function(c)
--    c:emit_signal("request::activate", "mouse_enter", {raise = false})
--end)

client.connect_signal("focus", function(c)
    c.border_color = beautiful.border_focus
end)

client.connect_signal("unfocus", function(c)
    c.border_color = beautiful.border_normal
end)
-- }}}

-- ** Garbage Collector ** --
-- Enable for lower memory consumption
-- Taken from https://github.com/rxyhn/yoru

-- collectgarbage("incremental", 110, 1000, 0) -- Awesome no me deja usarlo xd
collectgarbage("setpause", 110)
collectgarbage("setstepmul", 1000)
gears.timer({
    timeout = 120,
    autostart = true,
    call_now = true,
    callback = function ()
        collectgarbage("collect")
    end,
})
