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

local ezconfig = require("scripts/ezconfig")
local scratch = require("scripts/scratch")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Autostarting (the lua guru way)
awful.util.spawn("xwallpaper --zoom /home/delta/Pictures/wallpaper.jpg")
awful.util.spawn("/usr/share/emacs --daemon")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("/home/delta/.config/awesome/mytheme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "kitty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.max,
    awful.layout.suit.floating,
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

mylauncher = awful.widget.launcher({image = beautiful.awesome_icon,
                                    menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      "request::activate",
                                                      "tasklist",
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

--- Widget definitions
----------------------------------------------------------------------

local clock_widget = wibox.widget {
  format = "%A, %d/%m/%Y - %H:%M:%S",
  refresh = 1,
  widget = wibox.widget.textclock,
}

container_clock_widget = require("container_nobackground")(clock_widget, "#fab3ff")

local image_widget = wibox.widget {
	{
		{
			image = "/home/delta/Pictures/yveltal.png",
			resize = true,
			widget = wibox.widget.imagebox,
		},
		left = 1,
		right = 1,
		widget = wibox.container.margin,
	},
	bg = "#5f5f5f",
	shape = gears.shape.octogon,
	widget = wibox.container.background,
}

image_widget:connect_signal("mouse::enter", function(c) c:set_bg("#919191") end)
image_widget:connect_signal("mouse::leave", function(c) c:set_bg("#5f5f5f") end)
image_widget:connect_signal("button::press", function(c) c:set_bg("#1a1a1a") end)
image_widget:connect_signal("button::release", function(c, _, _, button) 
	c:set_bg("#919191")
	if button == 1 then awful.spawn("alacritty") end
end)

----------------------------------------------------------------------

awful.screen.connect_for_each_screen(function(s)
    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "A", "4", "5", "6", "7" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = require("taglist_custom")(s)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        filter   = awful.widget.tasklist.filter.currenttags,
        buttons  = tasklist_buttons,
        screen   = s,
    }

    -- Create the wibox
    s.mywibox = awful.wibar({
        position = "top",
        margins = { top = 6, left = 5, right = 5 },
        height = 24,
        screen = s,
    })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.textbox("  "),
	          image_widget,
            wibox.widget.textbox(" "),
            -- mylauncher,
            s.mytaglist,
            -- s.mypromptbox,
        },

        { -- Middle
            layout = wibox.layout.fixed.horizontal,
           -- s.mytasklist,
        },

        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            container_clock_widget,
            -- wibox.widget.systray(),
            s.mylayoutbox,
            wibox.widget.textbox("  ")
        },
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
--    awful.button({ }, 4, awful.tag.viewnext),
--    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Keys 2.0
globalkeys = ezconfig.keytable.join({

  -- Basic
  ["M-C-q"] = awesome.quit,
  ["M-C-r"] = awesome.restart,

  -- Common stuff
  ["M-<Return>"] = function() awful.spawn(terminal) end,
  ["M-b"]        = function() awful.spawn("brave") end,
  ["M-d"]        = function() awful.spawn("dmenu_run") end,
  ["M-f"]        = function() awful.spawn("thunar") end,
  ["M-S-e"]      = function() awful.spawn("emacsclient -c -a 'emacs'") end,

  -- Windows movement + size modification
  ["M-j"]   = function() awful.client.focus.byidx(1) end,
  ["M-k"]   = function() awful.client.focus.byidx(-1) end,

  ["M-S-j"] = function() awful.client.swap.byidx(1) end,
  ["M-S-k"] = function() awful.client.swap.byidx(-1) end,

  ["M-l"]   = function() awful.tag.incmwfact(0.05) end,
  ["M-h"]   = function() awful.tag.incmwfact(-0.05) end,

  -- Rotate between layout | workspaces
  ["M-<space>"]   = function() awful.layout.inc(1) end,
  ["M-S-<space>"] = function() awful.layout.inc(-1) end,

  ["M-<Left>"]  = awful.tag.viewprev,
  ["M-<Right>"] = awful.tag.viewnext,

  -- Extra stuff
  ["M-<Escape>"] = awful.tag.history.restore,
  ["M-w"]        = function() mymainmenu:show() end,
  ["M-u"]        = awful.client.urgent.jumpto,

  -- Rotates between the last 2 used windows
  ["A-<Tab>"] = function ()
    awful.client.focus.history.previous()
    if client.focus then
      client.focus:raise()
    end
  end,

  -- Idk lol
  ["M-C-n"] = function ()
    local c = awful.client.restore()
    if c then
      c:emit_signal("request::activate", "key.unminimize", {raise = true})
    end
  end,

})

-- Clientkeys 2.0
clientkeys = ezconfig.keytable.join({

  ["M-S-c"] = function(c) c:kill() end,
  ["M-C-<Return>"] = function(c) c:swap(awful.client.getmaster()) end,

  ["M-C-f"] = awful.client.floating.toggle,
  ["M-o"]   = function(c) c:move_to_screen() end,
  ["M-t"]   = function(c) c.ontop = not c.ontop end,
  ["M-n"]   = function(c) c.minimized = true end,

  -- 3 formas de maximizar ventanas. Si
  ["M-m"] = function(c)
    c.maximized = not c.maximized
    c:raise()
  end,

  ["M-C-m"] = function(c)
    c.maximized_vertical = not c.maximized_vertical
    c:raise()
  end,

  ["M-S-m"] = function(c)
    c.maximized_horizontal = not c.maximized_horizontal
    c:raise()
  end,

})

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,

        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end),

        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),

        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end),

        -- Creates another "instance" of the focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end)
    )
end

-- Mouse bindings?
clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
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
    { rule = {

      },

      properties = {
        border_width = beautiful.border_width,
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
          "galculator",
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
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            -- awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Turn titlebars on when client is floating
client.connect_signal("property::floating", function(c)
  if c.floating and not (c.request_no_titlebar or c.fullscreen) then
    awful.titlebar.show(c)
  else
    awful.titlebar.hide(c)
  end
end)

awful.tag.attached_connect_signal(nil, "property::layout", function(t)
  local float = t.layout.name == "floating"
  for _,c in pairs(t:clients()) do
    c.floating = float
  end
end)

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
--     c:emit_signal("request::activate", "mouse_enter", {raise = false})
-- end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
