return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'java' })
    end,
  },

  {
    'settings',
    opts = {
      ['java'] = { width = 2, style = 'space', ruler = 121 },
    },
  },
}
