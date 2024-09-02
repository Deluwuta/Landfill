local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")

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
