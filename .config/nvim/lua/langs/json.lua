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

  {
    'settings',
    opts = {
      ['json'] = { width = 2, style = 'space', ruler = 121 },
      ['jsonc'] = { width = 2, style = 'space', ruler = 121 },
    },
  },
}
