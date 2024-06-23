return {
  {
    'nvim-treesitter',
    opts = function (_, opts)
      vim.list_extend(opts.ensure_installed, { 'typescript', 'tsx', 'javascript', 'css', 'scss', 'graphql', 'html' })
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
        graphql = {
        },
        emmet_ls = {
        },
      }
    }
  },

  {
    'norcalli/nvim-colorizer.lua',
    ft = { 'css', 'scss', 'svelte' },
    opts = {
      'css';
      'scss';
    },
    config = function (_, opts)
      require('colorizer').setup(opts)
    end,
  }
}

