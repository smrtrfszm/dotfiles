return {
  {
    'numToStr/Comment.nvim',
    event = 'VeryLazy',
    dependencies = {
      {'JoosepAlviste/nvim-ts-context-commentstring', branch = 'main'},
      'nvim-treesitter/nvim-treesitter',
    },
    opts = {
      opleader = {
        line = 'gc',
        block = 'gb',
      },
      mappings = {
        basic = true,
        extra = true,
      },
      pre_hook = function (ctx)
        require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook()(ctx)
      end,
      post_hook = nil,
    },
    config = function (_, opts)
      require('Comment').setup(opts)
    end,
  }
}
