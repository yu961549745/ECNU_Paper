clc,clear,close all;
fs=getFiles('../_new_ppt/fig','.*?\.pdf');
cellfun(@gv2pdf,fs);

function gv2pdf(f)
name=getname(f);
fprintf('%s\n',name);
system(sprintf('pdfcrop %s.pdf %s.pdf',name,name));
end
function s = getname(f)
[path,name,~]=fileparts(f);
s=fullfile(path,name);
end