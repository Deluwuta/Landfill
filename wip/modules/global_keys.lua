local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")

local HOME = os.getenv("HOME")

local vars = require("utils.user_variables")
local osds = require("utils.osd_helpers")

-- Volume change
local _a = require("widgets.pruebas")
local volume_osd, update_volume = table.unpack(_a)

-- {{ Template
-- modifiers = { },
-- key = "",
-- description = "",
-- group = "",
-- on_press = function(),
-- }}

local mod = vars.mod
local alt = vars.alt

awful.keyboard.append_global_keybindings({
    -- Quit and restarting
    awful.key({
        modifiers = { mod, "Control" },
        key = "q",
        description = "quit awesome",
        group = "awesome",
        on_press = awesome.quit,
    }),

    awful.key({
        modifiers = { mod, "Control" },
        key = "r",
        description = "reload awesome",
        group = "awesome",
        on_press = awesome.restart,
    }),

    -- Common apps
    awful.key({
        modifiers = { mod },
        key = "Return",
        description = "launch terminal",
        group = "apps",
        on_press = function()
            awful.spawn(vars.terminal)
        end,
    }),

    awful.key({
        modifiers = { mod },
        key = "p",
        description = "app launcher",
        group = "launcher",
        on_press = function()
            awful.spawn(vars.launcher)
        end,
    }),

    awful.key({
        modifiers = { mod },
        key = "b",
        description = "internet browser",
        group = "apps",
        on_press = function()
            awful.spawn(vars.browser)
        end,
    }),

    awful.key({
        modifiers = { mod },
        key = "f",
        description = "file manager",
        group = "apps",
        on_press = function()
            awful.spawn(vars.file_manager)
        end,
    }),

    awful.key({
        modifiers = { mod, "Shift" },
        key = "s",
        description = "screenshoter",
        group = "apps",
        on_press = function()
            awful.spawn("flameshot gui")
        end,
    }),

    -- Multimedia
    awful.key({
        modifiers = { },
        key = "XF86AudioRaiseVolume",
        description = "Raise volume",
        group = "multimedia",
        on_press = function ()
            awful.spawn("amixer set Master 5%+ unmute")
            update_volume()
            osds.volume:again()
            volume_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86AudioLowerVolume",
        description = "Lower volume",
        group = "multimedia",
        on_press = function ()
            awful.spawn("amixer set Master 5%- unmute")
            update_volume()
            osds.volume:again()
            volume_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { alt },
        key = "m",
        description = "Raise volume",
        group = "multimedia",
        on_press = function ()
            awful.spawn("amixer set Master 5%+ unmute")
            update_volume()
            osds.volume:again()
            volume_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { alt },
        key = "n",
        description = "Raise volume",
        group = "multimedia",
        on_press = function ()
            awful.spawn("amixer set Master 5%- unmute")
            update_volume()
            osds.volume:again()
            volume_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86MonBrightnessUp",
        description = "Raise brightness",
        group = "multimedia",
        on_pres = function ()
            awful.spawn("brightnessctl s 5%+")
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86MonBrightnessDown",
        description = "Lower brightness",
        group = "multimedia",
        on_pres = function ()
            awful.spawn("brightnessctl s 5%-")
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86KbdBrightnessUp",
        description = "Raise keyboard brightness",
        group = "multimedia",
        on_pres = function ()
            awful.spawn(HOME .. "./config/scripts/kbdbacklight.sh up")
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86KbdBrightnessDown",
        description = "Lower keyboard brightness",
        group = "multimedia",
        on_pres = function ()
            awful.spawn(HOME .. "./config/scripts/kbdbacklight.sh down")
        end,
    }),

    awful.key({
        modifiers = { mod },
        key = "space",
        description = "Change keyboard layout",
        group = "utilities",
        on_pres = function ()
            awful.spawn(HOME .. "./config/scripts/keyboardChanger.sh")
        end,
    }),


    -- Tags related keybindings
    awful.key({
        modifiers = { mod },
        key = "Left",
        description = "move to previous tag",
        group = "tag",
        on_press = awful.tag.viewprev,
    }),

    awful.key({
        modifiers = { mod },
        key = "Right",
        description = "move to next tag",
        group = "tag",
        on_press = awful.tag.viewnext,
    }),

    awful.key({
        modifiers = { mod },
        key = "Escape",
        description = "move to the last used tag",
        group = "tag",
        on_press = awful.tag.history.restore,
    }),

    -- Focus related keybindings
    awful.key({
        modifiers = { mod },
        key = "k",
        description = "focus next by index",
        group = "client",
        on_press = function ()
            awful.client.focus.byidx(1)
        end,
    }),

    awful.key({
        modifiers = { mod },
        key = "j",
        description = "focus previous by index",
        group = "client",
        on_press = function ()
            awful.client.focus.byidx(-1)
        end,
    }),

    awful.key({
        modifiers = { alt },
        key = "Tab",
        description = "Rotate backwards between clients",
        group = "client",
        on_press = function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
    }),

    -- Restore minimized clients (mod+n to minimize)
    awful.key({
        modifiers = { mod, "Control" },
        key = "n",
        description = "restore minimized",
        group = "client",
        on_press = function ()
            local c = awful.client.restore()
            if c then
                c:activate {
                    raise = true,
                    context = "key.unminimize",
                }
            end
        end
    }),

    -- Multimonitor bindings (?)
    awful.key({
        modifiers = { mod, "Control" },
        key = "k",
        description = "focus the next screen",
        group = "screen",
        on_press = function ()
            awful.screen.focus_relative(1)
        end,
    }),

    awful.key({
        modifiers = { mod, "Control" },
        key = "j",
        description = "focus the previous screen",
        group = "screen",
        on_press = function ()
            awful.screen.focus_relative(-1)
        end,
    }),

    -- Layout related bindings
    awful.key({
        modifiers = { mod, "Shift" },
        key = "k",
        description = "swap the next client by index",
        group = "client",
        on_press = function ()
            awful.screen.swap.byidx(1)
        end,
    }),

    awful.key({
        modifiers = { mod, "Shift" },
        key = "j",
        description = "swap the previous client by index",
        group = "client",
        on_press = function ()
            awful.screen.swap.byidx(-1)
        end,
    }),

    awful.key({
        modifiers = { mod },
        key = "u",
        description = "jump to urgent client",
        group = "client",
        on_press = awful.client.urgent.jumpto
    }),

    awful.key({
        modifiers = { mod },
        key = "l",
        description = "increase master width factor",
        group = "layout",
        on_press = function ()
            awful.tag.incmwfact(0.05)
        end
    }),

    awful.key({
        modifiers = { mod },
        key = "h",
        description = "decrease master width factor",
        group = "layout",
        on_press = function ()
            awful.tag.incmwfact(-0.05)
        end
    }),

    -- Layout switching
    awful.key({
        modifiers = { mod },
        key = "Tab",
        description = "select next layout",
        group = "layout",
        on_press = function ()
            awful.layout.inc(1)
        end
    }),

    awful.key({
        modifiers = { mod, "Shift" },
        key = "Tab",
        description = "select previous layout",
        group = "layout",
        on_press = function ()
            awful.layout.inc(-1)
        end
    }),

    -- Awesome specific utilities
    awful.key({
        modifiers = { mod, "Control" },
        key = "s",
        description = "show help",
        group = "awesome",
        on_press = hotkeys_popup.show_help,
    }),

    -- awful.key({
    --     modifiers = { mod },
    --     key = "w",
    --     description = "show main menu",
    --     group = "awesome",
    --     on_press = function()
    --         mymainmenu:show()
    --     end
    -- }),

    -- awful.key({
    --     modifiers = { mod },
    --     key = "x",
    --     description = "lua execute prompt",
    --     group = "awesome",
    --     on_press = function ()
    --         awful.prompt.run {
    --             prompt = "Run Lua code: ",
    --             textbox = awful.screen.focused().mypromptbox.widget,
    --             exe_callback = awful.util.eval,
    --             history_path = awful.util.get_cache_dir() .. "/history_eval",
    --         }
    --     end,
    -- })
})
