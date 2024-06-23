return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'json', 'jsonc' })
    end,
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        jsonls = {
        },
      },
    },
  },
}
