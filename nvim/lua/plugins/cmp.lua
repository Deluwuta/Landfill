dofile(vim.g.base46_cache .. "cmp")

local options = function()
    local cmp = require("cmp")

    cmp.setup({
        completion = { completeopt = "menu,menuone" },

        snippet = {
            expand = function (args)
                require("luasnip").lsp_expand(args.body)
            end,
        },

        mapping = {
            ["<C-Space>"] = cmp.mapping.complete(),
            ["C-e"] = cmp.mapping.close(),

            ["<CR>"] = cmp.mapping.confirm {
                behavior = cmp.ConfirmBehavior.Insert,
                select = true,
            },

            ["<Tab>"] = cmp.mapping(function(fallback)
                if cmp.visible() then
                    cmp.select_next_item()
                elseif require("luasnip").expand_or_jumpable() then
                    require("luasnip").expand_or_jump()
                else
                    fallback()
                end
            end, { "i", "s" }),

            ["<S-Tab>"] = cmp.mapping(function(fallback)
                if cmp.visible() then
                    cmp.select_prev_item()
                elseif require("luasnip").jumpable(-1) then
                    require("luasnip").jump(-1)
                else
                    fallback()
                end
            end, { "i", "s" }),
        },

        sources = {
            { name = "nvim_lsp" },
            { name = "luasnip" },
            { name = "buffer" },
            { name = "nvim_lua" },
            { name = "path" },
        },
    })
end

local completions = {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
        {
            "L3MON4D3/LuaSnip",
            dependencies = { "rafamadriz/friendly-snippets" },
            opts = { history = true, updateevents = "TextChanged,TextChangedI" },
            config = function(_, opts)
                require("luasnip").config.set_config(opts)
                require "configs.luasnip_config"
            end,
        },

        -- Autopairs
        {
            "windwp/nvim-autopairs",
            opts = {
                fast_wrap = {},
                disable_filetype = { "TelescopePrompt", "vim" },
                enable_check_bracket_line = true,
                ignored_next_char = "[%w%.]",
            },
            config = function(_, opts)
                require("nvim-autopairs").setup(opts)

                -- setup cmp for autopairs
                local cmp_autopairs = require("nvim-autopairs.completion.cmp")
                require("cmp").event:on("confirm_done", cmp_autopairs.on_confirm_done())
            end,
        },

        -- cmp sources plugins
        {
            "saadparwaiz1/cmp_luasnip",
            "hrsh7th/cmp-nvim-lua",
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
        },
    },
    opts = options,
}

return completions
