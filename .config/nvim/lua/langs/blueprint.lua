local settings = require('settings2')

settings.lang({
  ['blueprint'] = {
    parsers = { 'blueprint' },
    indent = { width = 2, style = 'space', ruler = 121 },
  }
})

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'blueprint' })
    end,
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        blueprint_ls = {
        },
      },
    },
  },
}
