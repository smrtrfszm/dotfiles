return {
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

