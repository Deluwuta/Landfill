local keymap = vim.keymap

local nvim_comment = {
    "terrortylor/nvim-comment",
    lazy = false,
    config = function()
        require("nvim_comment").setup({
            comment_emtpy = false,
            comment_empty_trim_whitespaces = false,
        })
    end,
    keys = {
        keymap.set({"n", "v"}, "<leader>l", ":CommentToggle<CR>")
    },
}

local nvim_autopairs = {
    "windwp/nvim-autopairs",
    lazy = false,
    config = function()
        require("nvim-autopairs").setup({
            enable_check_bracket_line = true,
            ignored_next_char = "[%w%.]",
        })
    end,
}

local nvim_highlight_colors = {
    "brenoprata10/nvim-highlight-colors",
    lazy = false,
    config = function ()
        require("nvim-highlight-colors").setup({
            render = "background", -- background | foreground | first_column
            enable_named_colors = true,
            enable_tailwind = false,
        })
    end,
}

local vim_illuminate = {
    "RRethy/vim-illuminate",
    lazy = false,
    config = function()
        require("illuminate").configure({})
    end,
}

local tmux_navigator = {
    "christoomey/vim-tmux-navigator",
    lazy = false,
}

return {
    nvim_comment,
    nvim_autopairs,
    nvim_highlight_colors,
    tmux_navigator,
}
