pcall(function ()
    dofile(vim.g.base46_cache .. "syntax")
    dofile(vim.g.base46_cache .. "treesitter")
end)

local M = {}

M.opts = {
    indent = { enable = true },
    -- autotag = { enable = true },
    ensure_installed = {
        "bash",
        "c",
        "cpp",
        "css",
        "html",
        "json",
        "lua",
        "markdown",
        "python",
        "rust",
        "vim",
    },
    sync_install = false,
    auto_install = false,
    highlight = {
        enable = true,
        use_languagetree = true,
        -- additional_vim_regex_highlighting = true,
    },
}

return M.opts
