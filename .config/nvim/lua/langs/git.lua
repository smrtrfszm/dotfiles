return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'diff', 'gitcommit', 'gitattributes', 'gitignore', 'git_config', 'git_rebase' })
    end,
  },
}
