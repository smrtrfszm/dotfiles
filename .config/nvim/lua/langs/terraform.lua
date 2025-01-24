local settings = require('settings2')

settings.set({
  ['terraform'] = { width = 2, style = 'space' },
  ['terraform-vars'] = { width = 2, style = 'space' },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'terraform' })
    end
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        terraformls = {}
      }
    }
  },
}
