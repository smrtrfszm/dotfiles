return {
  {
    'nvim-treesitter',
    opts = function (_, opts)
      vim.list_extend(opts.ensure_installed, { 'typescript', 'tsx', 'javascript' })
    end,
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        tsserver = {
        },
        cssmodules_ls = {
        },
        cssls = {
        },
      }
    }
  },

  {
    'norcalli/nvim-colorizer.lua',
    opts = {
      'css';
      'scss';
    },
    config = function (_, opts)
      require('colorizer').setup(opts)
    end,
  }
}

