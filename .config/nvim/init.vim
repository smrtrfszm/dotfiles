set number relativenumber
set nu rnu

set nowrap
set laststatus=2
set noshowmode
set cursorline
set scrolloff=5
set mouse=a

let mapleader = "\<Space>"

" Plugins
source $HOME/.config/nvim/plugins.vim


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

map <C-f> :GFiles <CR>
nmap <leader>b :Buffers<CR>
nnoremap <leader><leader> <c-^>

nmap <leader>w :w<CR>

" Close vim when only nerdtree is open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Undo after reopen vim
set undodir=~/.vimdid
set undofile

" Syntax
syntax on

if has('nvim')
  aug fzf_setup
    au!
    au TermOpen term://*FZF tnoremap <silent> <buffer><nowait> <esc> <c-c>
  aug END
end

autocmd Filetype javascript setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype typescript setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype html setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype css setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype scss setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121
autocmd Filetype haskell setlocal tabstop=4 shiftwidth=4 expandtab softtabstop
autocmd Filetype java setlocal colorcolumn=121
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 expandtab softtabstop colorcolumn=81
autocmd Filetype cpp setlocal tabstop=4 shiftwidth=4 expandtab softtabstop colorcolumn=121

