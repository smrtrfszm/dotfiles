local settings = require('settings2')

settings.set({
  ['sql'] = { width = 2, style = 'space', ruler = 121 },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'sql' })
    end,
  },
}
