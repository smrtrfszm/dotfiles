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
