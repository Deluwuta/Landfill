-- dofile(vim.g.base46_cache .. "mason")
local servers = require("configs.lsp_servers")

local options = {
    PATH = "prepend", -- skip / prepend
    ui = {
        icons = {
            package_pending = " ",
            package_installed = " ",
            package_uninstalled = " ",
        },
    },
    max_concurrent_installers = 10,
}

local mason = {
    "williamboman/mason.nvim",
    lazy = false,
    config = function ()
        require("mason").setup(options)
    end
}

local mason_lsp = {
    "williamboman/mason-lspconfig.nvim",
    lazy = false,
    config = function ()
        require("mason-lspconfig").setup({
            ensure_installed = servers,
            automatic_installation = true,
        })
    end
}

return { mason, mason_lsp }
