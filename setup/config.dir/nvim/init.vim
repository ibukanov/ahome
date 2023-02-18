
" set wildmenu
set colorcolumn=80

" Keep the cursor at the center.
set scrolloff=999

" Buffer options
set hidden

:cabbrev h vert h

map <space> <C-D>
map , <C-U>


autocmd BufNewFile,BufRead ~/p/bergenrabbit/*.php set expandtab tabstop=4 shiftwidth=4 autoindent
