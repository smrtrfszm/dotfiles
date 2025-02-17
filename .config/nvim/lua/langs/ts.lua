local settings = require('settings2')

settings.set({
  ['javascript'] = { width = 2, style = 'space', ruler = 121 },
  ['typescript'] = { width = 2, style = 'space', ruler = 121 },
  ['typescriptreact'] = { width = 2, style = 'space', ruler = 121 },
  ['svelte'] = { width = 2, style = 'space', ruler = 121 },
  ['html'] = { width = 2, style = 'space', ruler = 121 },
  ['css'] = { width = 2, style = 'space', ruler = 121 },
  ['scss'] = { width = 2, style = 'space', ruler = 121 },
});

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
        ts_ls = {
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
  },
}

