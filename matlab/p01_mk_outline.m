clc,clear,close all;
fs=getFiles('../outline','.*?\.gv');
for k=1:length(fs)
    f=fs{k};
    [p,n,~]=fileparts(f);
    svg=fullfile(p,[n,'.svg']);
    pdf=fullfile(p,[n,'.pdf']);
    system(sprintf('dot -Tsvg -o %s %s',svg,f));
    system(sprintf('inkscape --file %s --export-pdf %s',svg,pdf));
end