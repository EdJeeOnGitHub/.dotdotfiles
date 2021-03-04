inoremap jj <Esc>
nnoremap <C-t> :NERDTreeToggle<CR>
"Plugins"
call plug#begin(stdpath('data') . '/plugged')

Plug 'ayu-theme/ayu-vim' " or other package manager
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'preservim/nerdtree'
call plug#end()

"...
"set termguicolors     " enable true colors support
"let ayucolor="light"  " for light version of theme
"let ayucolor="mirage" " for mirage version of theme
"let ayucolor="dark"   " for dark version of theme
"colorscheme ayu
autocmd vimenter * ++nested colorscheme gruvbox
