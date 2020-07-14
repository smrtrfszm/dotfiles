set number relativenumber
set nu rnu

set nowrap
set laststatus=2
set noshowmode

call plug#begin()

" Visual
Plug 'chriskempson/base16-vim'
Plug 'itchyny/lightline.vim'
Plug 'w0rp/ale'
Plug 'sheerun/vim-polyglot'

" Fuzzy finder
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf.vim'

" Autocomplete
" Plug 'ncm2/ncm2'
" Plug 'roxma/nvim-yarp'

call plug#end()

" Set base16 color scheme
colorscheme base16-default-dark
let base16colorspace=256

" Unbind arrow keys
noremap <Up> <Nop>
noremap! <Up> <Nop>
noremap <Down> <Nop>
noremap! <Down> <Nop>
noremap <Left> <Nop>
noremap! <Left> <Nop>
noremap <Right> <Nop>
noremap! <Right> <Nop>

inoremap <C-j> <Esc>

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

autocmd Filetype java setlocal colorcolumn=121

