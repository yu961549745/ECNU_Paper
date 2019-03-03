clc,clear,close all;
% 关键正则

[keys,bibs] = bibinfo('../paper/refs.bib');

% 输出某些键值
if ~exist('../bibinfo','dir')
    mkdir('../bibinfo');
end

js=get_attrs(bibs,'journal');
js=unique(js);
writetext('../bibinfo/js.csv',sprintf('%s\n',js{:}),'utf8');

bs=get_attrs(bibs,'booktitle');
bs=unique(bs);
writetext('../bibinfo/bs.csv',sprintf('%s\n',bs{:}),'utf8');

ps=get_attrs(bibs,'publisher');
ps=unique(ps);
writetext('../bibinfo/ps.csv',sprintf('%s\n',ps{:}),'utf8');

ts=get_attrs(bibs,'title');
uts=unique(ts);
if length(uts)~=length(ts)
    fprintf('发现重复 title\n');
end
ts=sort(ts);
writetext('../bibinfo/ts.csv',sprintf('%s\n',ts{:}),'utf8');

as=get_attrs(bibs,'author');
ns=cell(size(as));
p=1;
for k=1:length(as)
    aa=strsplit(as{k},' and ');
    for j=1:length(aa)
        ns{p}=strtrim(aa{j});
        p=p+1;
    end
end
ns=unique(ns);
writetext('../bibinfo/ns.csv',sprintf('%s\n',ns{:}),'utf8');


function vs = get_attrs(bibs,key)
% 用于提取一个属性的值, 假设每个属性只占一行, 且形如 xxx = {yyy}
% 使用时将 __key__ 替换为具体的关键字
attr_reg='(?<=\s+__key__\s*=\s*\{)(.*?)(?=\}\s*(,|\n))';
attr_reg=strrep(attr_reg,'__key__',key);
vs=cellfun(@(s){regexp(s,attr_reg,'match')},bibs);
vs=cat(1,vs{:});
end