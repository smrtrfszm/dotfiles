return {
  {
    "nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "haskell" })
    end,
  },

  {
    'settings',
    opts = {
      ['haskell'] = { width = 4, style = 'space' },
    },
  },
}
