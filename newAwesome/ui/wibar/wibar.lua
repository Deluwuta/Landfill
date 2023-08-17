local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

-- Widgets
local volume_control = require("ui.wibar.volume-control")

-- Keyboard map indicator and switcher
local mykeyboardlayout = awful.widget.keyboardlayout()

-- Battery?
local myBat = wibox.widget({
        id = "bat_role",
        -- markup = require("ui.wibar.prueba")(),
        refresh = 60,
        widget = wibox.widget.textbox,
})

-- Internet watcher
-- Volume
volumecfg = volume_control({})


-- Create a textclock widget
local mytextclock = wibox.widget({
    {
        {
            id = "clock_role",
            format = "<b>" .. " %a, %d/%m ~ %H:%M " .. "</b>",
            refresh = 30,
            -- font = "Fantasque Nerd Font Mono 11",
            widget = wibox.widget.textclock(),
        },
        fg = "#f5e0dc",
        widget = wibox.container.background,
    },
    margins = 2,
    spacing = 2,
    widget = wibox.container.margin,
})

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ mod }, 1, function(t)
        if client.focus then
            client.focus:move_to_tag(t)
        end
    end),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ mod }, 3, function(t)
                              if client.focus then
                                  client.focus:toggle_tag(t)
                              end
                          end),
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
    awful.button({ }, 1, function (c)
        if c == client.focus then
            c.minimized = true
        else
            c:emit_signal(
                "request::activate",
                "tasklist",
                {raise = true})
        end
    end),
    awful.button({ }, 3, function()
        awful.menu.client_list({ theme = { width = 250 }})
    end),
    awful.button({ }, 4, function () awful.client.focus.byidx(1) end),
    awful.button({ }, 5, function () awful.client.focus.byidx(-1) end)
)

-- Function to update tags
local update_tags = function (self, c3)
    local tagicon = self:get_children_by_id('icon_role')[1]
    if c3.selected then
        tagicon.text = "\u{f03c3}"
        self.fg = "#eeeeee"
    elseif #c3:clients() == 0 then
        tagicon.text = '\u{f09de}'
        self.fg = "#0077ff"
    else
        tagicon.text = '\u{f14fb}'
        self.fg = "#ff7700"
    end
end

-- Taglist widget_template definitions. Just to cleanup a bit
local taglist_template = {
    {
        {
            id = 'icon_role',
            font = "FantasqueSansM Nerd Font Propo 16",
            widget = wibox.widget.textbox
        },
        id = 'margin_role',
        margins = 2,
        widget = wibox.container.margin
    },
    id = 'background_role',
    widget = wibox.container.background,
    create_callback = function (self, c3, index, objects)
        update_tags(self, c3)
    end,
    update_callback = function(self, c3, index, objects)
        update_tags(self, c3)
    end
}

local taglist_simple = {
    {
        id = 'index_rule',
        widget = wibox.widget.textbox,
    },
    margins = 4,
    widget = wibox.container.margin,
}

local taglist_custom = {
    {
        {
            {
                id     = 'index_role',
                widget = wibox.widget.textbox,
            },
            fg = '#ff0000',
            widget = wibox.container.background,
        },
        layout = wibox.layout.fixed.horizontal,
    },
    id     = 'background_role',
    widget = wibox.container.background,
    -- Add support for hover colors and an index label
    create_callback = function(self, c3, index, objects) --luacheck: no unused args
        self:get_children_by_id('index_role')[1].markup = '<b> '..index..' </b>'
    end,
    update_callback = function(self, c3, index, objects) --luacheck: no unused args
        self:get_children_by_id('index_role')[1].markup = '<b> '..index..' </b>'
    end,
}

awful.screen.connect_for_each_screen(function(s)
    -- Each screen has its own tag table.
    -- awful.tag({ "\u{e24f}" }, s, awful.layout.layouts[1])
    awful.tag({"1", "2", "3", "4", "5", "6", "7"}, s, awful.layout.layouts[1])

    -- Custom tags config
    -- local tag1 = awful.tag.add(" FLOAT ", {
        -- layout = awful.layout.suit.floating,
        -- selected = false
    -- })

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
            awful.button({ }, 1, function () awful.layout.inc( 1) end),
            awful.button({ }, 3, function () awful.layout.inc(-1) end),
            awful.button({ }, 4, function () awful.layout.inc( 1) end),
            awful.button({ }, 5, function () awful.layout.inc(-1) end)
        )
    )

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        style = {
            font = "FantasqueSansM Nerd Font Propo 14",
            spacing = 2,
            bg_focus = "#313244",
            fg_focus = "#89b4fa",

            fg_occupied = "#fab387",

            bg_urgent = "#f38ba8",
            fg_urgent = "#181825",

        },
        --widget_template = {
        --    margins =  4,
        --    widget = wibox.container.margin,
        --},
        buttons = taglist_buttons
    }


    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({
        screen = s,
        position = "top",
        height = 20
    })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mypromptbox,
            -- mylauncher,
            s.mytaglist,
            s.mylayoutbox,
        },
        { -- Middle widgets
            layout = wibox.layout.fixed.horizontal,
            -- s.mytasklist,
        },
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            volumecfg.widget,
            mykeyboardlayout,
            myBat,
            mytextclock,
            wibox.widget.systray(),
        },
    }
end)
