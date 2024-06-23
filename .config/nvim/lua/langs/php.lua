return {
  {
    "nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "php" })
    end
  },

  {
    "nvim-lspconfig",
    opts = {
      servers = {
        phpactor = {
        }
      }
    }
  },

  {
    'settings',
    opts = {
      ['php'] = { width = 2, style = 'space', ruler = 121 },
    },
  },
}

