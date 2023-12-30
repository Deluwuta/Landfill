require 'nvim-treesitter.configs'.setup {
    ensure_installed = {
        "bash",
        "c",
        "cpp",
        "haskell",
        "html",
        "lua",
        "python",
        "rust",
        "vim",
        "yaml",
    },
    sync_install = false,
    auto_install = true,
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
    },
}
