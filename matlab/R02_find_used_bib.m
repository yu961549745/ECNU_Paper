clc,clear,close all;
s=read_tex('../paper/','paper.tex','utf8');
cite_reg='(?<=\\cite(tt)*\{)(.*?)(?=\})';
cs=regexp(s,cite_reg,'match');
[keys,bibs] = bibinfo('../paper/refs.bib');
% 提取引用的参考文献
ucs=cell(size(cs));
p=1;
for k=1:length(cs)
    scs=strsplit(cs{k},',');
    for j=1:length(scs)
        ucs{p}=scs{j};
        p=p+1;
    end
end
ucs=unique(ucs,'stable');
% 删除自定义命令的引用
[~,ind]=ismember('#1',ucs);
ucs(ind)=[];
% 按照代码中出现的顺序输出, 但是会受到自定义命令的影响
m=containers.Map(keys,bibs);
scs=cellfun(@(k){m(k)},ucs);
writetext('../bibinfo/used.txt',sprintf('%s\n\n',scs{:}),'utf8');

rest_keys=setdiff(keys,ucs);
rest_bibs=cellfun(@(k){m(k)},rest_keys);
writetext('../bibinfo/not_used.txt',sprintf('%s\n\n',rest_bibs{:}),'utf8');
