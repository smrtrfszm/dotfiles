set number relativenumber
set nu rnu

set nowrap
set laststatus=2
set noshowmode

let mapleader = "\<Space>"

call plug#begin()

" Visual
Plug 'chriskempson/base16-vim'
Plug 'itchyny/lightline.vim'
Plug 'w0rp/ale'

" Nerdtree
Plug 'preservim/nerdtree'

" Fuzzy finder
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Autocomplete
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'

Plug 'tpope/vim-fugitive'

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

autocmd Filetype haskell setlocal tabstop=4 shiftwidth=4 expandtab softtabstop

autocmd Filetype java setlocal colorcolumn=121

" Configure ncm2
autocmd BufEnter * call ncm2#enable_for_buffer()

set completeopt=noinsert,menuone,noselect
