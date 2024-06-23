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

  {
    'settings',
    opts = {
      ['terraform'] = { width = 2, style = 'space' },
      ['terraform-vars'] = { width = 2, style = 'space' },
    },
  },
}
