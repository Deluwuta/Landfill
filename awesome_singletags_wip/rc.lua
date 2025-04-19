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

-- Paths
local HOME = os.getenv("HOME")
local vars = require("utils.user_variables")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Error handling
require("modules.error_handling")

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(HOME .. "/.config/awesome/themes/theme.lua")

-- This is used later as the default terminal and editor to run.
local terminal = vars.terminal
local editor = os.getenv("EDITOR") or vars.editor
local editor_cmd = terminal .. " -e " .. editor
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
    menu = mymainmenu
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
    -- $HOME/Pictures/wallpapers/camellya_red_wuwa_twitter_kyktsu0908.jpg
    awful.spawn("feh --bg-fill " .. HOME .. "/Pictures/wallpapers/camellya_red_wuwa_twitter_kyktsu0908.jpg")

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

local sharedtags = require("wibar.single_tags")
local mod = require("utils.user_variables").mod
local tags = sharedtags({
    { name = "a", screen = 1, layout = awful.layout.layouts[1] },
    { name = "b", screen = 2, layout = awful.layout.layouts[1] },
    { name = "c", layout = awful.layout.layouts[1] },
    { name = "d", layout = awful.layout.layouts[1] },
    { name = "e", layout = awful.layout.layouts[1] },
    { name = "f", layout = awful.layout.layouts[1] },
    { name = "g", layout = awful.layout.layouts[1] },
    { layout = awful.layout.layouts[1] },
    { screen = 2, layout = awful.layout.layouts[1] }
})

-- Wibar
require("wibar.wibar")

-- Awesome bindings
require("modules.global_binds")

-- Tag related bindings. Modifier(s)+number
awful.keyboard.append_global_keybindings({
    awful.key {
        modifiers = { mod },
        keygroup = "numrow",
        description = "only view tag",
        group = "tag",
        on_press = function (index)
            local screen = awful.screen.focused()
            local tag = tags[index]
            if tag then
                sharedtags.viewonly(tag, screen)
            end
        end,
    },

    awful.key {
        modifiers = { mod, "Control" },
        keygroup = "numrow",
        description = "toggle tag",
        group = "tag",
        on_press = function (index)
            local screen = awful.screen.focused()
            local tag = tags[index]
            if tag then
                sharedtags.viewtoggle(tag, screen)
            end
        end,
    },

    awful.key {
        modifiers = { mod, "Shift" },
        keygroup = "numrow",
        description = "move focused client to tag",
        group = "tag",
        on_press = function (index)
            if client.focus then
                local tag = tags[index]
                if tag then
                    client.focus:move_to_tag(tag)
                end
            end
        end,
    },

    awful.key {
        modifiers = { mod, "Control", "Shift" },
        keygroup = "numrow",
        description = "toggle focused client on tag",
        group = "tag",
        on_press = function (index)
            if client.focus then
                local tag = tags[index]
                if tag then
                    client.focus:toggle_tag(tag)
                end
            end
        end,
    },
})

-- CLient bindings
require("modules.client_binds")

-- {{{ Rules
-- Rules to apply to new clients.
ruled.client.connect_signal("request::rules", function()
    -- All clients will match this rule.
    ruled.client.append_rule {
        id = "global",
        rule = { },
        properties = {
            focus = awful.client.focus.filter,
            raise = true,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap+awful.placement.no_offscreen
        }
    }

    -- Floating clients.
    ruled.client.append_rule {
        id = "floating",
        rule_any = {
            instance = {
                "copyq",
                "pinentry"
            },

            class = {
                "Arandr",
                "Blueman-manager",
                "Gpick",
                "Kruler",
                "Sxiv",
                "Tor Browser",
                "Wpa_gui",
                "veromix",
                "xtightvncviewer",
                vars.file_manager:gsub("^%l", string.upper),
                "qemu",
                "virt-manager",
                "Steam", "steam",
            },

            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                "Event Tester",  -- xev.
            },

            role = {
                "AlarmWindow",    -- Thunderbird's calendar.
                "ConfigManager",  -- Thunderbird's about:config.
                "pop-up",         -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = { floating = true }
    }

    -- Add titlebars to normal clients and dialogs
    ruled.client.append_rule {
        id = "titlebars",
        rule_any = { type = { "normal", "dialog" } },
        properties = { titlebars_enabled = false }
    }

    -- Set some application to always open on a given tag.
    ruled.client.append_rule {
        rule = { class = { "Steam", "steam" } },
        properties = {
            screen = 1,
            tag = "5"
        }
    }

    ruled.client.append_rule {
        rule = { class = "virt-manager" },
        properties = {
            screen = 1,
            tag = "6"
        }
    }
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

-- Notifications
require("modules.notifications")

-- Signals
require("modules.signals")

-- Autostart
awful.spawn.once("redshift -P -O 3000")
awful.spawn.once("nm-applet")
awful.spawn.once("systemctl --user start openrazer-daemon.service")
-- awful.spawn.once("$HOME/.config/scripts/desktop-mon-setup.sh")

awful.spawn.with_shell(HOME .. "/.config/awesome/autostart.sh")

-- awful.spawn.once("/usr/bin/emacs --daemon")

--- Virtual machine specifics
-- awful.spawn.once("xrandr -s 1920x1080")
-- awful.spawn.once("setxkbmap us intl altGr dead keys")

-- Garbage collector
-- Enable for lower memory consumption
-- Taken from https://github.com/rxyhn/yoru

collectgarbage("incremental", 110, 1000, 0)
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
