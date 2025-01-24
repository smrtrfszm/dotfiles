local settings = require('settings2')

settings.set({
  ['java'] = { width = 2, style = 'space', ruler = 121 },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'java' })
    end,
  },
}
