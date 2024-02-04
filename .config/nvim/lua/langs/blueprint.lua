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
  }
}
