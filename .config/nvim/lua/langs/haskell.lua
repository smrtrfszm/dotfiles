local settings = require('settings2')

settings.set({
  ['haskell'] = { width = 4, style = 'space' },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'haskell' })
    end,
  },
}
