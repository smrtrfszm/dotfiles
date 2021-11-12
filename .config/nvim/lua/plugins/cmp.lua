local cmp = require('cmp')
local lspkind = require('lspkind')

cmp.setup({
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(5),
    ['<C-u>'] = cmp.mapping.scroll_docs(-5),
    ['<CR>'] = cmp.mapping(
      cmp.mapping.confirm(
        {
          behaviour = cmp.ConfirmBehavior.Insert,
          select = true,
        },
        { 'i', 'c' }
      )
    ),
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'path' },
    { name = 'buffer', keyword_length = 5 },
  },
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
        path = '[path]',
        buffer = '[buf]',
      },
    })
  },
  experimental = {
    native_menu = false,
    ghost_text = true,
  },
})
