local lsp = require('lspconfig')

vim.fn.sign_define('LspDiagnosticsSignError', {
  texthl = 'LspDiagnosticsSignError',
  numhl = 'LspDiagnosticsSignError',
  text = '',
})

vim.fn.sign_define('LspDiagnosticsSignWarning', {
  texthl = 'LspDiagnosticsSignWarning',
  numhl = 'LspDiagnosticsSignWarning',
  text = '',
})

vim.fn.sign_define('LspDiagnosticsSignInformation', {
  texthl = 'LspDiagnosticsSignInformation',
  numhl = 'LspDiagnosticsSignInformation',
  text = '',
})

vim.fn.sign_define('LspDiagnosticsSignHint', {
  texthl = 'LspDiagnosticsSignHint',
  numhl = 'LspDiagnosticsSignHint',
  text = '',
})

local servers = {
  'rust_analyzer',
  'tsserver', 
  'clangd',
  'cssls',
  'html',
  'jsonls',
  'yamlls',
}

for _, l in ipairs(servers) do
  lsp[l].setup({})
end
