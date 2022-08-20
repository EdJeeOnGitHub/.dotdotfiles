inoremap jj <Esc>

autocmd FileType r inoremap <buffer> <C-,> <Esc>:normal! a%>%<CR>a 
autocmd FileType rnoweb inoremap <buffer> <C-,> <Esc>:normal! a%>%<CR>a 
autocmd FileType rmd inoremap <buffer> <C-,> <Esc>:normal! a%>%<CR>a 

call plug#begin()
Plug 'jpalardy/vim-slime'
Plug 'airblade/vim-gitgutter'
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-fugitive'
call plug#end()

" NERDTree
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

" vim-slime
let g:slime_target = "kitty"
" remove default mappings
let g:slime_no_mappings = 1 
" add back default mappings
xmap <c-c><c-c> <Plug>SlimeRegionSend 
nmap <c-c><c-c> <Plug>SlimeParagraphSend
nmap <c-c>v     <Plug>SlimeConfig
"xmap <c-Enter> <Plug>SlimeRegionSend 
nmap <c-Enter> <Plug>SlimeParagraphSend
xmap <c-Enter> <Plug>SlimeRegionSend gv<esc>

"function! _EscapeText_r(text)
"  call system("cat > ~/.slime_r", a:text)
"  return ["source('~/.slime_r', echo = TRUE, max.deparse.length = 4095)\r"]
"endfunction

set number                     " Show current line number
set relativenumber             " Show relative line numbers

syntax on " Syntax highlighting
set ruler " Always shows location in file (line#)
set smarttab " Autotabs for certain code
set shiftwidth=4
set tabstop=4
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <expr> <tab> InsertTabWrapper()
inoremap <s-tab> <c-n>


set background=dark
colorscheme gruvbox
