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
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Git
Plug 'tpope/vim-fugitive'

" I don't know what this is
Plug 'eagletmt/ghcmod-vim'

call plug#end()

" Plugin configs
source $HOME/.config/nvim/plugin-configs/coc.vim
source $HOME/.config/nvim/plugin-configs/base16.vim
source $HOME/.config/nvim/plugin-configs/lightline.vim
source $HOME/.config/nvim/plugin-configs/ale.vim
