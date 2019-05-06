clc,clear,close all;
fs=getFiles('../_new_ppt/fig','.*?\d+');
for k=1:length(fs)
    fprintf('%s\n',fs{k});
    r=folder_cut_range(fs{k});
    disp(r);
    if r(1)==1 && r(3)==1
        continue;
    end
    fss=getFiles(fs{k},'.*?\.png');
    cellfun(@(x)cut_img(x,r),fss);
end

function cut_img(f,r)
fprintf('\t%s\n',f);
im=imread(f);
imwrite(im(r(1):r(2),r(3):r(4),:),f);
end
function range = folder_cut_range(f)
fs=getFiles(f,'.*?\.png');
rs=cellfun(@(x){cut_range(x)},fs);
rs=cell2mat(rs);
range=[min(rs(:,1)),max(rs(:,2)),min(rs(:,3)),max(rs(:,4))];
end
function range = cut_range(f)
im=imread(f);
msk=all(im~=255,3);
r=any(msk,2);
c=any(msk,1);
range=[rg(r),rg(c)];
end
function ab = rg(x)
a=find(x,1,'first');
b=find(x,1,'last');
ab=[a,b];
end