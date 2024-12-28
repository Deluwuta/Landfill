dofile(vim.g.base46_cache .. "nvimtree")

local options = {
    filters = { dotfiles = false },
    -- disable_netrw = true,
    hijack_cursor = true,
    sync_root_with_cwd = true,
    update_focused_file = {
        enable = true,
        update_root = false,
    },
    view = {
        width = 30,
        preserve_window_proportions = true,
    },
    renderer = {
        root_folder_label = false,
        highlight_git = true,
        indent_markers = { enable = true },
        icons = {
            glyphs = {
                default = "󰈚",
                folder = {
                    default = "",
                    empty = "",
                    empty_open = "",
                    open = "",
                    symlink = "",
                },
                git = { unmerged = "" },
            },
        },
    },
}

local nvim_tree = {
    "nvim-tree/nvim-tree.lua",
    lazy = false,
    cmd = { "NvimTreeToggle", "NvimTreeFocus" },
    opts = options,
    config = function ()
        require("nvim-tree").setup({
            -- Keybinds
            vim.keymap.set(
                "n", "<leader>tt", ":NvimTreeToggle<CR>",
                { desc = "nvimtree toggle window" }
            ),

            vim.keymap.set(
                "n", "<leader>e", ":NvimTreeFocus<CR>",
                { desc = "nvimtree focus window" }
            )
        })
    end
}

return { nvim_tree }
