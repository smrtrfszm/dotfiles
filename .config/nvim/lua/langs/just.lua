return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'just' })
    end,
  },

  {
    'settings',
    opts = {
      ['just'] = { width = 2, style = 'space' },
    },
  },
}

