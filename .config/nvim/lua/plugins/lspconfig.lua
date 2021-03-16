local lsp = require('lspconfig')

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
