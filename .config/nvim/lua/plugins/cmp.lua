return {
  {
    'hrsh7th/nvim-cmp',
    version = false,
    event = 'InsertEnter',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lua',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'onsails/lspkind-nvim',
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',
    },
    opts = function ()
      local cmp = require('cmp')
      local lspkind = require('lspkind')

      return {
        mapping = {
          ['<C-d>'] = cmp.mapping.scroll_docs(5),
          ['<C-u>'] = cmp.mapping.scroll_docs(-5),
          ['<Tab>'] = cmp.mapping(
            cmp.mapping.confirm(
              {
                behaviour = cmp.ConfirmBehavior.Insert,
                select = true,
              },
              { 'i', 'c' }
            )
          ),
          ['<C-n>'] = function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            else
              fallback()
            end
          end,
          ['<C-p>'] = function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            else
              fallback()
            end
          end,
          ['<C-space>'] = cmp.mapping.complete(),
        },
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'nvim_lua' },
          { name = 'path' },
          { name = 'buffer', keyword_length = 3 },
        }),
        snippet = {
          expand = function(args)
            require('luasnip').lsp_expand(args.body)
          end,
        },
        formatting = {
          format = lspkind.cmp_format({
            with_text = true,
            menu = {
              nvim_lsp = '[LSP]',
              nvim_lua = '[Lua]',
              path = '[path]',
              buffer = '[buf]',
            },
          })
        },
        experimental = {
          native_menu = false,
          ghost_text = true,
        },
      }
    end,
    config = function (_, opts)
      require('cmp').setup(opts)
    end,
  }
}
