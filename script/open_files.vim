"
" open_files.vim
" ==============
"
" use:
"
" :source script/open_files.vim
"
" to load the function into scope, where you can call with:
"
" :call OpenAoC(day)
"
" If you use the :source command in a buffer where the filename has a number
" in it (like Day16.hs), this will automatically open all the files associated
" with that day.
"
" Change s:year below to open test data for a different year
"

function! OpenAoC(year,day)
    let l:daystr  = printf("%02d",a:day)
    let l:yearstr = printf("%04d",a:year)
    let l:files = [l:yearstr . "/AOC" . l:yearstr . "/Day" . l:daystr . ".hs",
                  \"data/" . l:yearstr . "/" . l:daystr . ".txt",
                  \"prompt/" . l:yearstr . "/" . l:daystr . "a.md",
                  \"prompt/" . l:yearstr . "/" . l:daystr . "b.md",
                  \"data/" . l:yearstr . "/code-blocks/" . l:daystr . "a.txt",
                  \"data/" . l:yearstr . "/code-blocks/" . l:daystr . "b.txt",
                  \"test-data/" . l:yearstr . "/" . l:daystr . "a.txt",
                  \"test-data/" . l:yearstr . "/" . l:daystr . "b.txt",
                  \"reflections/" . l:yearstr . "/day" . l:daystr . ".md",
                  \"bench-results/" . l:yearstr . "/day" . l:daystr . ".txt"
                  \]

    for fn in reverse(l:files)
        execute "e " . fnameescape(fn)
    endfor
endfunction

let s:buffyear = str2nr(matchstr(expand('%'), '\d\d\d\d'))
let s:buffday = str2nr(matchstr(expand('%:t:r'), '\d\d'))

if (s:buffday == 0)
    echo "no valid file found in buffer; use :call OpenAoC(day) to open a day"
else
    echo "found day " . string(s:buffyear) . " " . string(s:buffday)
    call OpenAoC(s:buffyear, s:buffday)
endif
