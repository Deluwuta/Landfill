-- awesome_mode: api-level=4:screen=on
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

-- Declarative object management
local ruled = require("ruled")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Autostarting
awful.util.spawn("/home/delta/.config/awesome/autostart.sh")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
naughty.connect_signal("request::display_error", function(message, startup)
    naughty.notification {
        urgency = "critical",
        title   = "Oops, an error happened"..(startup and " during startup!" or "!"),
        message = message
    }
end)
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.init("/home/delta/.config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "alacritty"
editor = "nvim"
editor_cmd = terminal .. " -e " .. editor

-- Default mod.
local mod = "Mod4"
local alt = "Mod1"

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

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Tag layout
-- Table of layouts to cover with awful.layout.inc, order matters.
tag.connect_signal("request::default_layouts", function()
    awful.layout.append_default_layouts({
        awful.layout.suit.tile,
        awful.layout.suit.floating,
        awful.layout.suit.tile.bottom,
        awful.layout.suit.max,
        -- awful.layout.suit.tile.left,
        -- awful.layout.suit.tile.top,
        -- awful.layout.suit.fair,
        -- awful.layout.suit.fair.horizontal,
        -- awful.layout.suit.spiral,
        -- awful.layout.suit.spiral.dwindle,
        -- awful.layout.suit.max.fullscreen,
        -- awful.layout.suit.magnifier,
        -- awful.layout.suit.corner.nw,
    })
end)
-- }}}

-- {{{ Mouse bindings
awful.mouse.append_global_mousebindings({
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    -- awful.button({ }, 4, awful.tag.viewprev),
    -- awful.button({ }, 5, awful.tag.viewnext),
})
-- }}}

-- {{{ Key bindings

-- General Awesome keys
awful.keyboard.append_global_keybindings({
    -- awful.key({ mod }, "s", hotkeys_popup.show_help, { description="show help", group="awesome" }),
    -- awful.key({ mod }, "w", function () mymainmenu:show() end,{ description = "show main menu", group = "awesome" }),

    -- I wanna quit life (in game)
    awful.key({ mod, "Control" }, "q", awesome.quit),
    awful.key({ mod, "Control" }, "r", awesome.restart),

    --awful.key({ mod }, "x", function ()
    --  awful.prompt.run {
    --    prompt       = "Run Lua code: ",
    --    textbox      = awful.screen.focused().mypromptbox.widget,
    --    exe_callback = awful.util.eval,
    --    history_path = awful.util.get_cache_dir() .. "/history_eval"
    --  }
    --end, { description = "lua execute prompt", group = "awesome"} ),

    awful.key({ mod }, "Return", function () awful.spawn(terminal) end),
    awful.key({ mod }, "p"     , function () awful.spawn("rofi -show drun") end),
    awful.key({ mod }, "b"     , function () awful.spawn("brave") end),

    -- Tags related keybindings
    awful.key({ mod }, "Left"  , awful.tag.viewprev, { description = "view previous", group = "tag"}),
    awful.key({ mod }, "Right" , awful.tag.viewnext, { description = "view next", group = "tag"}),
    awful.key({ mod }, "Escape", awful.tag.history.restore, { description = "go back", group = "tag"}),

    -- Focus related keybindings
    awful.key({ mod }, "j", function () awful.client.focus.byidx( 1) end),
    awful.key({ mod }, "k", function () awful.client.focus.byidx(-1) end),

    awful.key({ alt }, "Tab", function ()
     awful.client.focus.history.previous()
        if client.focus then
            client.focus:raise()
        end
    end),

    -- Multimonitor bindings? Idk
    awful.key({ mod, "Control" }, "j", function () awful.screen.focus_relative( 1) end, { description = "focus the next screen", group = "screen" }),
    awful.key({ mod, "Control" }, "k", function () awful.screen.focus_relative(-1) end, { description = "focus the previous screen", group = "screen" }),

    awful.key({ mod, "Control" }, "n", function ()
      local c = awful.client.restore()
        -- Focus restored client
        if c then
          c:activate { raise = true, context = "key.unminimize" }
        end
      end,
    {description = "restore minimized", group = "client"}),

    -- Layout related keybindings
    awful.key({ mod, "Shift" }, "j", function () awful.client.swap.byidx( 1) end),
    awful.key({ mod, "Shift" }, "k", function () awful.client.swap.byidx(-1) end),

    awful.key({ mod, "Shift" }, "u", awful.client.urgent.jumpto, { description = "jump to urgent client", group = "client" }),

    awful.key({ mod }, "l", function () awful.tag.incmwfact( 0.05) end),
    awful.key({ mod }, "h", function () awful.tag.incmwfact(-0.05) end),

    awful.key({ mod }         , "Tab", function () awful.layout.inc( 1) end),
    awful.key({ mod, "Shift" }, "Tab", function () awful.layout.inc(-1) end),
})

awful.keyboard.append_global_keybindings({
    awful.key {
        modifiers   = { mod },
        keygroup    = "numrow",
        description = "only view tag",
        group       = "tag",
        on_press    = function (index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                tag:view_only()
            end
        end,
    },
    awful.key {
        modifiers   = { mod, "Control" },
        keygroup    = "numrow",
        description = "toggle tag",
        group       = "tag",
        on_press    = function (index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                awful.tag.viewtoggle(tag)
            end
        end,
    },
    awful.key {
        modifiers = { mod, "Shift" },
        keygroup    = "numrow",
        description = "move focused client to tag",
        group       = "tag",
        on_press    = function (index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:move_to_tag(tag)
                end
            end
        end,
    },
    awful.key {
        modifiers   = { mod, "Control", "Shift" },
        keygroup    = "numrow",
        description = "toggle focused client on tag",
        group       = "tag",
        on_press    = function (index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:toggle_tag(tag)
                end
            end
        end,
    },
    awful.key {
        modifiers   = { mod },
        keygroup    = "numpad",
        description = "select layout directly",
        group       = "layout",
        on_press    = function (index)
            local t = awful.screen.focused().selected_tag
            if t then
                t.layout = t.layouts[index] or t.layout
            end
        end,
    }
})

client.connect_signal("request::default_mousebindings", function()
    awful.mouse.append_client_mousebindings({
        awful.button({ }, 1, function (c)
            c:activate { context = "mouse_click" }
        end),
        awful.button({ mod }, 1, function (c)
            c:activate { context = "mouse_click", action = "mouse_move"  }
        end),
        awful.button({ mod }, 3, function (c)
            c:activate { context = "mouse_click", action = "mouse_resize"}
        end),
    })
end)

client.connect_signal("request::default_keybindings", function()
    awful.keyboard.append_client_keybindings({
        awful.key({ mod, "Control" }, "m", function (c)
                c.fullscreen = not c.fullscreen
                c:raise()
        end),

        -- Killin
        awful.key({ mod, "Shift" }  , "c", function (c) c:kill() end),

        -- Toggle float | Switch with master | On top
        awful.key({ mod, "Control" }, "v",  awful.client.floating.toggle),
        awful.key({ mod, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
        awful.key({ mod, "Control" }, "t", function (c) c.ontop = not c.ontop end, {description = "toggle keep on top", group = "client"}),

        -- awful.key({ mod }, "o", function (c) c:move_to_screen() end, {description = "move to screen", group = "client"}),

       -- awful.key({ mod }, "n",
       --     function (c)
       --         -- The client currently has the input focus, so it cannot be
       --         -- minimized, since minimized clients can't have the focus.
       --         c.minimized = true
       --     end, {description = "minimize", group = "client"}),

        -- Maximizing Windows
        awful.key({ mod }, "m", function (c)
                c.maximized = not c.maximized
                c:raise()
        end),

       -- awful.key({ mod, "Control" }, "m", function (c)
       --         c.maximized_vertical = not c.maximized_vertical
       --         c:raise()
       -- end),

        awful.key({ mod, "Shift" }, "m", function (c)
                c.maximized_horizontal = not c.maximized_horizontal
                c:raise()
        end),
    })
end)

-- }}}

-- {{{ Rules
-- Rules to apply to new clients.
ruled.client.connect_signal("request::rules", function()
    -- All clients will match this rule.
    ruled.client.append_rule {
        id         = "global",
        rule       = { },
        properties = {
            focus     = awful.client.focus.filter,
            raise     = true,
            screen    = awful.screen.preferred,
            placement = awful.placement.no_overlap+awful.placement.no_offscreen
        }
    }

    -- Floating clients.
    ruled.client.append_rule {
        id       = "floating",
        rule_any = {
            instance = { "copyq", "pinentry" },
            class    = {
                "Arandr",
                "Blueman-manager",
                "Gpick",
                "Kruler",
                "Sxiv",
                "Tor Browser",
                "Wpa_gui",
                "veromix",
                "xtightvncviewer"
            },
            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name    = {
                "Event Tester",  -- xev.
            },
            role    = {
                "AlarmWindow",    -- Thunderbird's calendar.
                "ConfigManager",  -- Thunderbird's about:config.
                "pop-up",         -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = { floating = true }
    }

    -- Add titlebars to normal clients and dialogs
    ruled.client.append_rule {
        id         = "titlebars",
        rule_any   = { type = { "normal", "dialog" } },
        properties = { titlebars_enabled = false }
    }

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- ruled.client.append_rule {
    --     rule       = { class = "Firefox"     },
    --     properties = { screen = 1, tag = "2" }
    -- }
end)
-- }}}

-- {{{ Notifications

ruled.notification.connect_signal('request::rules', function()
    -- All notifications will match this rule.
    ruled.notification.append_rule {
        rule       = { },
        properties = {
            screen           = awful.screen.preferred,
            implicit_timeout = 5,
        }
    }
end)

naughty.connect_signal("request::display", function(n)
    naughty.layout.box { notification = n }
end)

-- }}}

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:activate { context = "mouse_enter", raise = false }
end)

-- ** UI ** --
require("ui")

 -- ** Garbage Collector ** --
-- Enable for lower memory consumption
-- Taken from https://github.com/rxyhn/yoru

collectgarbage("incremental", 110, 1000, 0) -- Awesome no me deja usarlo xd
-- collectgarbage("setpause", 110)
-- collectgarbage("setstepmul", 1000)
gears.timer({
    timeout = 120,
    autostart = true,
    call_now = true,
    callback = function ()
        collectgarbage("collect")
    end,
})
