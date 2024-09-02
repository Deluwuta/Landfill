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

-- Commonly used paths
local HOME = os.getenv("HOME")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Error handling
require("modules.error_handling")

local _aux = require("widgets.pruebas")
local volume_osd, update_volume = table.unpack(_aux)

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.init(HOME .. "/.config/awesome/themes/theme.lua")

-- This is used later as the default terminal and editor to run.
local terminal = "kitty"
local launcher = "dmenu_run"
local editor = os.getenv("EDITOR") or "nvim"
local editor_cmd = terminal .. " -e " .. editor

-- Modkeys. Alt = Mod1; WinKey = Mod4
local mod = "Mod4"
local alt = "Mod1"
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
local myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

local mymainmenu = awful.menu({
    items = {
        { "awesome", myawesomemenu, beautiful.awesome_icon },
        { "open terminal", terminal }
    }
})

local mylauncher = awful.widget.launcher({
    image = beautiful.awesome_icon,
    menu = mymainmenu,
})

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Layouts
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.floating,
    awful.layout.suit.max,
}

-- {{{ Wallpaper
screen.connect_signal("request::wallpaper", function(s)
    awful.spawn("xwallpaper --zoom " .. HOME .. "/Pictures/nicole_demara_anby_demara_amillion_and_billy_kid_officialArt.png")
    -- awful.wallpaper {
    --     screen = s,
    --     widget = {
    --         {
    --             image     = beautiful.wallpaper,
    --             upscale   = true,
    --             downscale = true,
    --             widget    = wibox.widget.imagebox,
    --         },
    --         valign = "center",
    --         halign = "center",
    --         tiled  = false,
    --         widget = wibox.container.tile,
    --     }
    -- }
end)
-- }}}

-- UI
require("modules.wibar")

-- {{{ Mouse bindings
awful.mouse.append_global_mousebindings({
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    -- awful.button({ }, 4, awful.tag.viewprev),
    -- awful.button({ }, 5, awful.tag.viewnext),
})
-- }}}

-- local volume_timer = gears.timer {
--     timeout = 2,
--     autostart = true,
--     callback = function()
--         volume_osd.visible = false
--     end
-- }
--
-- volume_osd:connect_signal("mouse::enter", function()
--     volume_timer:stop()
--     volume_osd.visible = true
-- end)
--
-- volume_osd:connect_signal("mouse::leave", function()
--     volume_timer:again()
-- end)

-- {{{ Key bindings
require("modules.global_keys")

-- Awesome keys :sunglasses:
awful.keyboard.append_global_keybindings({
    -- awful.key({ mod }, "s", hotkeys_popup.show_help,{description="show help", group="awesome"}),
    awful.key({ mod }, "w", function () mymainmenu:show() end, {description = "show main menu", group = "awesome"}),

    awful.key({ mod }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),

    -- Quit and restarting, in that order 
    -- awful.key({ mod, "Control" }, "q", awesome.quit, {description = "quit awesome", group = "awesome"}),
    -- awful.key({ mod, "Control" }, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
    --
    -- awful.key({ mod }         , "Return", function () awful.spawn(terminal) end),
    -- awful.key({ mod }         , "p"     , function () awful.spawn(launcher) end),
    -- awful.key({ mod }         , "b"     , function () awful.spawn("firefox-nightly") end),
    -- awful.key({ mod }         , "f"     , function () awful.spawn("pcmanfm") end),
    -- awful.key({ mod, "Shift" }, "s"     , function () awful.spawn("flameshot gui") end),
    --
    -- -- Multimedia keys
    -- awful.key({ }, "XF86AudioRaiseVolume" , function () awful.spawn("amixer -D pulse set Master 5%+ unmute > /dev/null") end),
    -- awful.key({ }, "XF86AudioLowerVolume" , function () awful.spawn("amixer -D pulse set Master 5%- unmute > /dev/null") end),
    --
    -- awful.key {
    --     modifiers = { alt },
    --     key = "n",
    --     on_press = function ()
    --         awful.spawn("amixer set Master 5%+ unmute")
    --         update_volume()
    --         volume_timer:again()
    --         volume_osd.visible = true
    --     end,
    -- },
    --
    -- awful.key {
    --     modifiers = { alt },
    --     key = "m",
    --     on_press = function ()
    --         awful.spawn("amixer set Master 5%- unmute")
    --         update_volume()
    --         volume_timer:again()
    --         volume_osd.visible = true
    --     end,
    -- },

    awful.key({ }, "XF86MonBrightnessUp"  , function () awful.spawn("brightnessctl s 5%+") end),
    awful.key({ }, "XF86MonBrightnessDown", function () awful.spawn("brightnessctl s 5%-") end),

    awful.key({ }, "XF86KbdBrightnessUp"  , function () awful.spawn("/home/delta/.config/scripts/kbdbacklight.sh up") end),
    awful.key({ }, "XF86KbdBrightnessDown", function () awful.spawn("/home/delta/.config/scripts/kbdbacklight.sh down") end),

    -- Keyboard language changer
    awful.key({ mod }, "space", function () awful.spawn("/home/delta/.config/scripts/keyboardChanger.sh") end),

    awful.key({ mod }, "r", function () awful.screen.focused().mypromptbox:run() end, {description = "run prompt", group = "launcher"}),
    -- awful.key({ mod }, "p", function() menubar.show() end, {description = "show the menubar", group = "launcher"}),

    -- Tags related keybindings
    awful.key({ mod }, "Left"  , awful.tag.viewprev, {description = "view previous", group = "tag"}),
    awful.key({ mod }, "Right" , awful.tag.viewnext, {description = "view next", group = "tag"}),
    awful.key({ mod }, "Escape", awful.tag.history.restore, {description = "go back", group = "tag"}),

    -- Focus related keybindings
    awful.key({ mod }, "j", function () awful.client.focus.byidx( 1) end, {description = "focus next by index", group = "client"}),
    awful.key({ mod }, "k", function () awful.client.focus.byidx(-1) end, {description = "focus previous by index", group = "client"}),

    awful.key({ alt }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end, {description = "go back", group = "client"}),

    -- Multimonitor bindings?
    awful.key({ mod, "Control" }, "j", function () awful.screen.focus_relative(1) end, {description = "focus the next screen", group = "screen"}),
    awful.key({ mod, "Control" }, "k", function () awful.screen.focus_relative(-1) end, {description = "focus the previous screen", group = "screen"}),

    awful.key({ mod, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:activate { raise = true, context = "key.unminimize" }
                  end
              end,
              {description = "restore minimized", group = "client"}),
    -- Layout related bindings
    awful.key({ mod, "Shift" }, "j", function () awful.client.swap.byidx( 1) end, {description = "swap with next client by index", group = "client"}),
    awful.key({ mod, "Shift" }, "k", function () awful.client.swap.byidx(-1) end, {description = "swap with previous client by index", group = "client"}),

    awful.key({ mod }, "u", awful.client.urgent.jumpto, {description = "jump to urgent client", group = "client"}),
    awful.key({ mod }, "l", function () awful.tag.incmwfact( 0.05) end, {description = "increase master width factor", group = "layout"}),
    awful.key({ mod }, "h", function () awful.tag.incmwfact(-0.05) end, {description = "decrease master width factor", group = "layout"}),

    awful.key({ mod }         , "Tab", function () awful.layout.inc(1) end, {description = "select next", group = "layout"}),
    awful.key({ mod, "Shift" }, "Tab", function () awful.layout.inc(-1) end, {description = "select previous", group = "layout"}),
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

-- Client bindings
client.connect_signal("request::default_mousebindings", function()
    awful.mouse.append_client_mousebindings({
        awful.button({ }    , 1, function (c) c:activate { context = "mouse_click" } end),
        awful.button({ mod }, 1, function (c) c:activate { context = "mouse_click", action = "mouse_move" } end),
        awful.button({ mod }, 3, function (c) c:activate { context = "mouse_click", action = "mouse_resize"} end),
    })
end)

client.connect_signal("request::default_keybindings", function()
    awful.keyboard.append_client_keybindings({
        awful.key({ mod, "Control" }, "m", function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end, {description = "toggle fullscreen", group = "client"}),

        awful.key({ mod, "Shift" }, "c", function (c) c:kill() end, {description = "close", group = "client"}),
        awful.key({ mod, "Control" }, "space", awful.client.floating.toggle, {description = "toggle floating", group = "client"}),
        awful.key({ mod, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end, {description = "move to master", group = "client"}),
        awful.key({ mod }, "o", function (c) c:move_to_screen() end, {description = "move to screen", group = "client"}),
        awful.key({ mod }, "t", function (c) c.ontop = not c.ontop end, {description = "toggle keep on top", group = "client"}),

        awful.key({ mod }, "n", function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end, {description = "minimize", group = "client"}),

        awful.key({ mod }, "m", function (c)
            c.maximized = not c.maximized
            c:raise()
        end, {description = "(un)maximize", group = "client"}),

        --awful.key({ mod, "Control" }, "m", function (c)
        --    c.maximized_vertical = not c.maximized_vertical
        --    c:raise()
        --end, {description = "(un)maximize vertically", group = "client"}),

        awful.key({ mod, "Shift" }, "m", function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end, {description = "(un)maximize horizontally", group = "client"}),
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
                "Arandr", "Blueman-manager", "Gpick", "Kruler", "Sxiv",
                "Tor Browser", "Wpa_gui", "veromix", "xtightvncviewer"
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

-- {{{ Titlebars
-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = {
        awful.button({ }, 1, function()
            c:activate { context = "titlebar", action = "mouse_move"  }
        end),
        awful.button({ }, 3, function()
            c:activate { context = "titlebar", action = "mouse_resize"}
        end),
    }

    awful.titlebar(c).widget = {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                halign = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)
-- }}}

-- {{{ Notifications

ruled.notification.connect_signal('request::rules', function()
    -- All notifications will match this rule.
    ruled.notification.append_rule {
        rule       = { },
        properties = {
            screen           = awful.screen.preferred,
            implicit_timeout = 3,
        }
    }
end)

-- }}}

-- Signals
require("modules.signals")

-- Initial spawns
-- awful.spawn.once("redshift -P -O 3000")
-- awful.spawn.once("/usr/bin/emacs --daemon")
awful.spawn.once("nm-applet")

-- Virtual machine specific
awful.spawn.once("xrandr -s 1920x1080")
awful.spawn.once("setxkbmap us intl altGr dead keys")

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
