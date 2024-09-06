-- Lets see how it goes
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi =  beautiful.xresources.apply_dpi

local separator = wibox.widget.textbox("   ")
separator.forced_height = dpi(50)
separator.forced_width = dpi(50)

-- Signals
local osd_text = wibox.widget({
    markup = '<span color="' .. "#d3c6aa" .. '" font="Ubuntu Nerd Font bold 14">' .. 'bottom text' .. '</span>'
    ,
    widget = wibox.widget.textbox,
    fg = "#FFFFFF",
})

local _maker = {}

function _maker.make_image(filepath)
    return wibox.widget {
        image = filepath,
        widget = wibox.widget.imagebox,
        resize = true,
        forced_height = dpi(24),
        forced_width = dpi(24),
        halign = 'center',
    }
end

function _maker.make_slider(max_value)
    return wibox.widget({
        widget = wibox.widget.slider,
        bar_shape = function (cr, width, height)
            gears.shape.rounded_rect(cr, width, height, 20)
        end,
        bar_height = dpi(10),
        bar_color = "#232a2a",
        bar_active_color = "#d3c6aa",

        handle_width = 0,
        handle_shape = nil,
        -- handle_color = "#FFF",
        -- handle_border_width = 1,
        -- handle_border_color = "#FFF",

        minimum = 0,
        maximum = max_value,
        value = 0,
    })
end

function _maker.make_percentage(slider)
    return wibox.widget {
        markup = '<span color="' ..
            "#d3c6aa" .. '" font="Ubuntu Nerd Font bold 14">' .. slider.value .. '</span>'
        ,
        widget = wibox.widget.textbox,
        fg = "#FFFFFF",
    }
end

-- Scheduler
local update_scheduled = false
function _maker.make_scheduler(update_slider)
    return function ()
        if not update_scheduled then
            update_scheduled = true
            gears.timer.start_new(0.1, function ()
                update_slider()
                update_scheduled = false
                return false
            end)
        end
    end
end

-- Function that takes all the widgets and puts them in the osd
-- It needs an image widget, the slider and the value (percentage)
function _maker.make_osd(image, slider, percentage)
    local osd_box = awful.popup {
        screen = s,
        widget = wibox.container.background,
        ontop = true,
        bg = "#00000000",
        visible = false,
        placement = function(c)
            awful.placement.bottom(c,
                { margins = {
                    top = dpi(0),
                    bottom = dpi(120),
                    left = 0,
                    right = dpi(0),
                }})
        end,
        opacity = 1
    }

    osd_box:setup {
        {
            {
                {
                    image,
                    widget = wibox.container.margin,
                    -- top = dpi(15),
                    bottom = dpi(0),
                    right = dpi(6),
                    left = dpi(6),
                },
                {
                    slider,
                    layout = wibox.layout.align.horizontal,
                    -- bottom = dpi(15),
                    -- top = dpi(0),
                    -- left = dpi(25),
                    -- right = dpi(25),
                    forced_height = dpi(10),
                    forced_width = dpi(160),
                    widget = wibox.container.margin,
                },
                {
                    {
                        -- osd_text,
                        -- nil,
                        percentage,
                        layout = wibox.layout.align.horizontal
                    },
                    widget = wibox.container.margin,
                left = dpi(6),
                    right = dpi(6),
                    -- top = dpi(0),
                    -- bottom = dpi(15)
                },
                widget = wibox.container.place,
                halign = 'center',
                layout = wibox.layout.fixed.horizontal
            },
            -- separator,
            widget = wibox.container.margin,
            top = dpi(6),
            bottom = dpi(6),
            left = dpi(3),
            right = dpi(3),
        },
        widget = wibox.container.background,
        bg = "#3d484d",
        shape = function(cr, width, height)
            gears.shape.rounded_rect(cr, width, height, 30)
        end,
    }

    return osd_box
end

return _maker