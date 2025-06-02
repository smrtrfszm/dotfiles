return {
  {
    'mason-org/mason-lspconfig.nvim',
    opts = {
      automatic_enable = true,
      ensure_installed = {
        'ansiblels',
        'clangd',
        'cmake',
        -- 'codelldb',
        'cssls',
        'cssmodules_ls',
        'emmet_ls',
        'gopls',
        'graphql',
        'hyprls',
        'jsonls',
        'lua_ls',
        'phpactor',
        'rust_analyzer',
        'terraformls',
        'ts_ls',
        'yamlls',
      },
    },
    dependencies = {
      { 'mason-org/mason.nvim', opts = {} },
      'neovim/nvim-lspconfig',
    },
  }
}
