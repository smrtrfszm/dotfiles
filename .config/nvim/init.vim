set number relativenumber
set nu rnu

set nowrap
set laststatus=2
set noshowmode

let mapleader = "\<Space>"

" Plugins
source $HOME/.config/nvim/plugins.vim

" Plugin configs
source $HOME/.config/nvim/plugin-configs/coc.vim
source $HOME/.config/nvim/plugin-configs/base16.vim
source $HOME/.config/nvim/plugin-configs/lightline.vim

" Unbind arrow keys
noremap <Up> <Nop>
noremap! <Up> <Nop>
noremap <Down> <Nop>
noremap! <Down> <Nop>
noremap <Left> <Nop>
noremap! <Left> <Nop>
noremap <Right> <Nop>
noremap! <Right> <Nop>

" Bind esc to ctrl + j
inoremap <C-j> <Esc>

" Open nerdtree on ctrl + n
map <C-n> :NERDTreeToggle<cr>

map <C-f> :Files <CR>
nmap <leader>b :Buffers<CR>

nmap <leader>w :w<CR>

" Close vim when only nerdtree is open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Undo after reopen vim
set undodir=~/.vimdid
set undofile

" Syntax
syntax on

set colorcolumn=81

autocmd Filetype javascript setlocal colorcolumn=121 
autocmd Filetype javascript setlocal tabstop=2 shiftwidth=2 expandtab softtabstop


autocmd Filetype typescript setlocal colorcolumn=121 
autocmd Filetype typescript setlocal tabstop=2 shiftwidth=2 expandtab softtabstop

autocmd Filetype css setlocal colorcolumn=121 
autocmd Filetype css setlocal tabstop=2 shiftwidth=2 expandtab softtabstop

autocmd Filetype scss setlocal colorcolumn=121 
autocmd Filetype scss setlocal tabstop=2 shiftwidth=2 expandtab softtabstop

autocmd Filetype haskell setlocal tabstop=4 shiftwidth=4 expandtab softtabstop

autocmd Filetype java setlocal colorcolumn=121

