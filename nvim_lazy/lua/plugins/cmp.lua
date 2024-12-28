local cmp_config = function ()
    local cmp = require("cmp")
    require("luasnip.loaders.from_vscode").lazy_load()

    cmp.setup({
        mapping = cmp.mapping.preset.insert({
            ["<C-Space>"] = cmp.mapping.complete(),
            ["<Tab>"]     = cmp.mapping.select_next_item(),
            ["<S-Tab>"]   = cmp.mapping.select_prev_item(),
            ["<A-CR>"]    = cmp.mapping.abort(),
            ["<CR>"]      = cmp.mapping.confirm({ select = true }),
        }),
        snippet = {
            expand = function(args)
                require("luasnip").lsp_expand(args.body)
            end,
        },
        sources = cmp.config.sources({
            { name = "nvim_lsp" },
            { name = "luasnip" },
        },
        {
            { name = "buffer" },
            { name = "path" },
        }),
    })
end

local cmp_luasnip = {
    "saadparwaiz1/cmp_luasnip",
    lazy = false,
    config = cmp_config,
    dependencies = {
        "L3MON4D3/LuaSnip",
    },
}

local friendly_snippets = {
    "rafamadriz/friendly-snippets",
    lazy = false,
}

return {
    cmp_luasnip,
    friendly_snippets,
}
