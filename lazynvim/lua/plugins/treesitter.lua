local config = function ()
    require("nvim-treesitter.configs").setup({
        indent = {
            enable = true,
        },
        autotag = {
            enable = true,
        },
        ensure_installed = {
            "bash",
            "c",
            "cpp",
            "css",
            "haskell",
            "html",
            "json",
            "lua",
            "markdown",
            "python",
            "rust",
            "vim",
            "yaml",
        },
        sync_install = false,
        auto_install = true,
        highlight = {
            enable = true,
            additional_vim_regex_highlighting = true,
        },
    })
end

local nvim_treesitter_table = {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    config = config,
}

local extensions = {
    nvim_ts_autotags = {
        "windwp/nvim-ts-autotag",
        lazy = false,
        config = {},
        dependencies = {
            "nvim-treesitter/nvim-treesitter",
        },
    },
    hola = "Hello mum!",
}

return {
    nvim_treesitter_table,
    extensions.nvim_ts_autotags,
}
