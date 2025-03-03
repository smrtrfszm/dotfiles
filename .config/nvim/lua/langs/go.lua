local settings = require('settings2')

settings.set({
  ['go'] = { width = 4, style = 'tab', ruler = 121 },
  ['proto'] = { width = 2, style = 'space', ruler = 121 },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'go', 'gomod', 'gosum', 'gowork', 'proto' })
    end
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        gopls = {
        }
      }
    }
  },
}
