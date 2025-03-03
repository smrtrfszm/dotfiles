local settings = require('settings2')

settings.set({
  ['markdown'] = { width = 4, style = 'space', ruler = 80 },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'markdown' })
    end,
  },
}
