local utils = require('utils')
utils.set_opts({ width = 4, style = 'space', ruler = 81 })

vim.treesitter.language.register('bash', 'zsh')
vim.treesitter.start()
