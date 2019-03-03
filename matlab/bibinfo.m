function [keys,bibs] = bibinfo(f)
% 此处假设每一个 bib 的格式都是
% @xxx{yyy,
%     ...
% }
% 即头尾是顶格的
bib_reg='(?s)@\w+\{(\w+),(\s|.)*?\n\}';

% 用于提取一个 bib 元素的 label
key_reg='(?<=@\w+\{)(\w+)';
get_key=@(s)regexp(s,key_reg,'match');

% 读取信息
s=readtext(f,'utf8');
bibs=regexp(s,bib_reg,'match');
keys=cellfun(get_key,bibs);
end