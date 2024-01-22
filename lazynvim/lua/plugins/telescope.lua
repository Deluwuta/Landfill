-- Telescope :D
local kmap = vim.keymap

local config = function()
    require("telescope").setup {
        defaults = {
            vimgrep_arguments = {
                "rg",
                "-L",
                "--color=never",
                "--no-heading",
                "--with-filename",
                "--line-number",
                "--column",
                "--smart-case",
                -- "--trim",
            },
            -- path_display = "absolute"
        },

        pickers = {
            find_files = {
                theme = "ivy",
                initial_mode = "normal",
                hidden = true,
            },
            buffers = {
                initial_mode = "normal",
            },
        },

        extensions = {
            file_browser = {
                theme = "ivy",
                initial_mode = "insert",
                hidden = true,
                prompt_path = true,
                dir_icon = "",
                dir_icon_hl = "Default",
            },
        },
    }
    require("telescope").load_extension("file_browser")
end

local telescope_table = {
    "nvim-telescope/telescope.nvim",
    -- tag = "0.1.5",
    lazy = false,
    dependencies = {
        "nvim-lua/plenary.nvim",
    },
    config = config,
    keys = {
        -- Finding files
        kmap.set("n", "<leader>ff", ":Telescope find_files<CR>"),
        kmap.set("n", "<leader>fr", ":Telescope oldfiles<CR>"),
        kmap.set("n", "<leader>." , ":Telescope file_browser hidden=true<CR>"),
        kmap.set("n", "<leader>fw", ":Telescope live_grep<CR>"),

        -- Buffers
        kmap.set("n", "<leader>fb", ":Telescope buffers<CR>"),

        -- Choose colorscheme
        kmap.set("n", "<leader>ht", ":Telescope colorscheme<CR>"),
    },
}

local extensions = {
    file_browser = {
        "nvim-telescope/telescope-file-browser.nvim",
        lazy = false,
        dependencies = { 
            "nvim-telescope/telescope.nvim",
            "nvim-lua/plenary.nvim"
        },
    },
}

return {
    telescope_table,
    extensions.file_browser,
}
