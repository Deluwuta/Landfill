dofile(vim.g.base46_cache .. "mason")

local M = {}

M.opts = {
    PATH = "skip", -- skip / prepend
    ui = {
        icons = {
            package_pending = " ",
            package_installed = " ",
            package_uninstalled = " ",
        },
    },
    max_concurrent_installers = 10,
}

return M.opts
