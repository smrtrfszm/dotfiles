luafile $HOME/.config/nvim/init2.lua

" Plugins
source $HOME/.config/nvim/plugins/init.vim

" open/close coc-explorer
nmap <leader>e :CocCommand explorer<cr>

" Syntax
syntax on

autocmd Filetype javascript setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype typescript setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype html setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype css setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype scss setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype haskell setlocal tabstop=4 shiftwidth=4 expandtab softtabstop
autocmd Filetype java setlocal colorcolumn=121
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 expandtab softtabstop colorcolumn=81
autocmd Filetype cpp setlocal tabstop=4 shiftwidth=4 expandtab softtabstop colorcolumn=121
autocmd Filetype rust setlocal tabstop=4 shiftwidth=4 expandtab softtabstop colorcolumn=121
autocmd Filetype yaml setlocal tabstop=2 shiftwidth=2 expandtab softtabstop
