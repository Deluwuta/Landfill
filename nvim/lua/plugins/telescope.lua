dofile(vim.g.base46_cache .. "telescope")
local kmap = vim.keymap

local options = {
    defaults = {
        -- path_display = "absolute",
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
        selection_caret = " ",
        entry_prefix = " ",
        sorting_strategy = "ascending",
        layout_config = {
            horizontal = {
                prompt_position = "top",
                preview_width = 0.55,
            },
            width = 0.87,
            height = 0.80,
        },

        mappings = {
            n = { ["q"] = require("telescope.actions").close },
        },
    },

    pickers = {
        find_files = {
            -- theme = "ivy",
            initial_mode = "insert",
            hidden = true,
        },
        buffers = {
            initial_mode = "normal",
        },
    },

    extensions = {
        file_browser = {
            -- Open the filebrowser in the current buffer path
            path = vim.fn.expand('%:p'),
            select_buffer = true,

            hidden = true,
            grouped = true,
            previewer = true,

            theme = "ivy",
            prompt_path = true,
            initial_mode = "insert",

            dir_icon = "",
            dir_icon_hl = "Default",

            layout_config = {
                horizontal = { prompt_position = "top" },
                height = 0.40,
            },
        },
    },
}

local telescope_table = {
    "nvim-telescope/telescope.nvim",
    -- tag = "0.1.5",
    lazy = false,
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
        require("telescope").setup(options)
        require("telescope").load_extension("file_browser")
    end,

    keys = {
        -- Finding files
        kmap.set("n", "<leader>ff", ":Telescope find_files<CR>"),
        kmap.set("n", "<leader>fr", ":Telescope oldfiles<CR>"),
        kmap.set("n", "<leader>." , ":Telescope file_browser<CR>"),
        -- kmap.set("n", "<leader>." , ":Telescope file_browser hidden=true path=%:p:h select_buffer=true<CR>"),
        kmap.set("n", "<leader>fw", ":Telescope live_grep<CR>"),

        -- Buffers
        kmap.set("n", "<leader>fb", ":Telescope buffers<CR>"),

        -- Choose colorscheme
        -- kmap.set("n", "<leader>ht", ":Telescope colorscheme<CR>"),
        kmap.set("n", "<leader>th", ":lua require('nvchad.themes').open()<CR>"),
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
