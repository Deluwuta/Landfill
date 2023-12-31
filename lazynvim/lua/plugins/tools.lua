local keymap = vim.keymap

local comment_config = function ()
    require("nvim_comment").setup({
        comment_emtpy = false,
        comment_empty_trim_whitespaces = false,
    })
end

local comment_table = {
    "terrortylor/nvim-comment",
    lazy = false,
    config = comment_config,
    keys = {
        -- Commenting keys
        keymap.set({"n", "v"}, "<leader>l", ":CommentToggle<CR>")
    },
}

local autopairs = {
    "windwp/nvim-autopairs",
    lazy = false,
    config = function ()
        require("nvim-autopairs").setup({
            enable_check_bracket_line = true,
            ignored_next_char = "[%w%.]",
        })
    end,

}

local illuminate = {
    "RRethy/vim-illuminate",
    lazy = false,
    config = function ()
        require("illuminate").configure({})
    end,
}

local tmux_navigator = {
    "christoomey/vim-tmux-navigator",
    lazy = false,
}

local whichkey = {
    "folke/which-key.nvim",
    lazy = true,
}

return {
    comment_table,
    autopairs,
    illuminate,
    tmux_navigator,
    whichkey,
}
