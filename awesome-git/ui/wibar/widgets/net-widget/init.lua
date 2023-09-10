local module_path = "ui.wibar.widgets."

package.loaded.net_widgets = nil

local net_widgets = {
    indicator   = require(module_path .. "net-widget.indicator"),
    wireless    = require(module_path .. "net-widget.wireless"),
    internet    = require(module_path .. "net-widget.internet")
}

return net_widgets
