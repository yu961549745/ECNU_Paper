function s = read_tex(p,f,encoding)
if isempty(regexp(f,'.*?\.tex','once'))
    f=[f,'.tex'];
end
fn=fullfile(p,f);
s=readtext(fn,encoding);
reg='\\input\{(.*?)\}';
ks=regexp(s,reg,'match');
n=length(ks);
if n>0
    fs=cellfun(@(s){strtrim(regexprep(s,reg,'$1'))},ks);
    cs=cellfun(@(s){read_tex(p,s,encoding)},fs);
    for k=1:n
        s=strrep(s,ks{k},cs{k});
    end
end
end