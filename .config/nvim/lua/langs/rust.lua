return {
  {
    'Saecki/crates.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    event = { 'BufRead Cargo.toml' },
    config = true,
  },

  {
    'nvim-cmp',
    dependencies = {
      'Saecki/crates.nvim',
    },
    opts = function(_, opts)
      local cmp = require('cmp')
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, {
        { name = 'crates' },
      }))
    end,
  },
}
