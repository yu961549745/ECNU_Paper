# timestamp: 2019-05-06 14:18:57
# git  hash: e8bd9e6
eqs:=table();
get_eq:=proc(n::string)
    if assigned(eqs[n]) then
        return eqs[n];
    else
        error("%1 not defined.",n);
    end if;
end proc:
# 1+1
alias(u=u(x,t));
eqs["(1+1)Burgers"]:=diff(u,t)+u*diff(u,x)-alpha*diff(u,x,x);
eqs["(1+1)KdV"]:=diff(u,t)+alpha*u*diff(u,x)+diff(u,x,x,x);
eqs["(1+1)Boussinesq"]:=diff(u,t,t)-alpha*diff(u,x,x)-beta*diff(u^2,x,x)-gamma*diff(u,x$4);
eqs["(1+1)Vakhnenko"]:=diff(u,t,x)+diff(u,x)^2+diff(u,x)*u+u;
eqs["(1+1)Fisher"]:=diff(u,t)-alpha*diff(u,x,x)-beta*u*(1-u);
eqs["(1+1)mKdV"]:=diff(u,t)+alpha*u^2*diff(u,x)+diff(u,x,x,x);
eqs["(1+1)KdV-mKdV"]:=diff(u,t)+alpha*diff(u,x)*u+beta*diff(u,x)*u^2+gamma*diff(u,x$3);
# 以下摘自柳老师硕士论文
alias(d=diff):
eqs["(1+1)BBM"]:=d(u,t)+d(u,x)+u*d(u,x)+p*d(u,x,x,t);
eqs["(1+1)JE"]:=d(u,t)+d(u,x)+u*d(u,x)+p*d(u,x,t,t);
eqs["(1+1)MBBM"]:=d(u,t)+d(u,x)+u^2*d(u,x)+p*d(u,x,x,t);
eqs["(1+1)KdVB"]:=d(u,t)+d(u,x)+u*d(u,x)-p*d(u,x,x)-q*d(u,x,x,x);
eqs["(1+1)BBMB"]:=d(u,t)+d(u,x)+u*d(u,x)-p*d(u,x,x)-q*d(u,x,x,t);
eqs["(1+1)STO"]:=d(u,t)+3*p*(d(u,x)^2+u^2*d(u,x)+u*d(u,x,x)+d(u,x$3)/3);
eqs["(1+1)SRLW"]:=d(u,t,t)+p*d(u,x,x)+q*d(u^2,x,t)+r*d(u,x,x,t,t);
eqs["(1+1)SOBO"]:=d(u,t,t)+q*d(u^2,x,x)+r*d(u,x$4);
eqs["(1+1)Hirota"]:=d(u,t)-d(u,x,x,t)+p*d(u,x)*(1-d(u,t));
eqs["(1+1)BH"]:=d(u,t)+p*u*d(u,x)-d(u,x,x)-q*(1-u)*(u-r);
eqs["(1+1)KdVBK"]:=d(u,t)+u*d(u,x)+p*d(u,x,x)+q*d(u,x$3)+r*d(u,x$4);
eqs["(1+1)7D"]:=d(u,t)+u*d(u,x)+p*d(u,x$3)+q*d(u,x$5)+r*d(u,x$7);
eqs["(1+1)SK"]:=d(u,t)+45*u^2*d(u,x)+15*d(u,x)*d(u,x,x)+15*u*d(u,x$3)+d(u,x$5);
eqs["(1+1)KK"]:=2*d(u,t)-90*u^2*d(u,x)-75*d(u,x)*d(u,x,x)+30*u*d(u,x,x)+2*d(u,x$5);
eqs["(1+1)CH"]:=d(u,t)+d(u,x,x,t)+2*alpha*d(u,x)+3*u*d(u,x)-2*d(u,x)*d(u,x,x)-u*d(u,x$3);
eqs["(1+1)EMM"]:=d(u,t)+d(u,x)+alpha*d(u,x$3)+beta*u*d(u,x)+gamma*u*d(u,x$3)+delta*d(u,x)*d(u,x,x);
eqs["(1+1)5D-G"]:=diff(u, t)+Diff(alpha*(diff(u, x))^2+beta*u*(diff(u, x$2))+mu*(diff(u, x$2))+diff(u, x$4)+p*u+q*u^2+r*u^3, x);
alias(d=d):
alias(u=u);
# 2+1
alias(u=u(x,y,t));
eqs["(2+1)KP"]:=diff(diff(u,t)+6*u*diff(u,x)+diff(u,x,x,x),x)+alpha*diff(u,y,y);
eqs["(2+1)SK"]:=diff(u,x,x,x,x,x,x)+5*diff(u,x,x)*diff(u,x,x,x)+5*diff(u,x)*diff(u,x,x,x,x)+5*diff(u,x)^2*diff(u,x,x)+5*(diff(u,x,x,x,y)-diff(u,y,y)+diff(u,x)*diff(u,x,y)+diff(u,x,x)*diff(u,y))-diff(u,x,t);
eqs["(2+1)BKP"]:=diff(u,t)+diff(u,x,x,x,x,x)-5*(diff(u,x,x,y)+int(diff(u,y,y),x))+15*(diff(u,x)*diff(u,x,x)+u*diff(u,x,x,x)-u*diff(u,y)-diff(u,x)*int(diff(u,y),x))+45*u^2*diff(u,x);
eqs["(2+1)BKP-T"]:=subs(u=diff(v(x,y,t),x),eqs["(2+1)BKP"]);
eqs["(2+1)CBS"]:=diff(u,x,t)+diff(u,x$3,y)+4*diff(u,x)*diff(u,x,y)+2*diff(u,x,x)*diff(u,y);
eqs["(2+1)CBS-G"]:=diff(u,x,t)+diff(u,x$3,y)+3*diff(u,x)*diff(u,x,y)+3*diff(u,x,x)*diff(u,y)+alpha*diff(u,x,y)+beta*diff(u,y,y);
alias(u=u);
# 3+1
alias(u=u(x,y,z,t));
eqs["(3+1)KP"]:=diff(diff(u,t)-6*u*diff(u,x)+diff(u,x,x,x),x)+3*diff(u,y,y)+3*diff(u,z,z);
eqs["(3+1)YTSF"]:=-4*diff(u,x,t)+diff(u,x,x,x,z)+4*diff(u,x)*diff(u,x,z)+2*diff(u,x,x)*diff(u,z)+3*alpha*diff(u,y,y);
eqs["(3+1)JM"]:=diff(u,x,x,x,y)+3*diff(u,y)*diff(u,x,x)+3*diff(u,x)*diff(u,x,y)+2*diff(u,y,t)-3*diff(u,x,z);
eqs["(3+1)BKP"]:=diff(u,y,t)-diff(u,x$3,y)-3*diff(diff(u,x)*diff(u,y),x)+3*diff(u,x$2)+3*diff(u,z$2);
eqs["(3+1)NEE"]:=3*diff(u,x,z)-diff(2*diff(u,t)+diff(u,x,x,x)-2*u*diff(u,x),y)+2*diff(diff(u,x)*int(diff(u,y),x),x);
eqs["(3+1)NEE-T"]:=subs(u=diff(v(x,y,z,t),x),eqs["(3+1)NEE"]);
eqs["(3+1)CBS"]:=diff(u,x,t)+4*diff(u,x)*diff(u,x,y)+2*diff(u,x,x)*diff(u,y)+4*diff(u,x)*diff(u,x,z)+2*diff(u,x,x)*diff(u,z)+diff(u,x,x,x,y)+diff(u,x,x,x,z);
eqs["(3+1)KPB"]:=diff(u,x$3,y)+3*diff(diff(u,x)*diff(u,y),x)+diff(u,t,y)+diff(u,t,x)+diff(u,t,t)-diff(u,z,z);
alias(u=u);
# 4+1
alias(u=u(x,y,z,w,t)):
eqs["(4+1)Fokas"]:=diff(u,x,t)-1/4*diff(u,x,x,x,y)+1/4*diff(u,x,y,y,y)+3/2*diff(u^2,x,y)-3/2*diff(u,z,w);
alias(u=u);
# 行波变换统一实现 
twtransform:=proc(_eq,tr,fs::set(name):={})
    local eq,s,funcs,vars,trvars,varsubs;
    # 保序的自变量替换
    varsubs:=proc(f,vs)
        local svs,k;
        # 行波变量的位置是第一个替换变量的位置
        svs:=seq(vs[k]=NULL,k=2..numelems(vs)),vs[1]=s(vs[]);
        return f=op(0,f)(subs(svs,[op(f)])[]);
    end proc:
    eq:=_eq;
    funcs:=remove(t->type(op(0,t),procedure),indets(eq,function)); # 所有函数 
    if fs<>{} then # 只对指定的函数进行变换
        funcs:=indets(funcs,specfunc(fs));
    end if;
    vars:=map(op,funcs);# 所有函数变量的并集
    trvars:=indets(rhs(tr),name) intersect vars;# 需要替换的变量
    funcs:=select(t->trvars subset {op(t)}, funcs);# 自变量含需要替换的变量的函数
    eq:=eval(eq,map(varsubs,funcs,trvars));# 行波变量换成抽象函数
    eq:=convert(eq,D);# 统一替换为 D 形式, 保证行波的抽象函数只作为自变量出现
    eq:=subs(s(trvars[])=lhs(tr),eq);# 抽象函数换成新变量名
    eq:=convert(eq,diff);# 转化成正常表达式
    eq:=eval(subs(s(trvars[])=rhs(tr),eq));# 抽象函数换成具体表达式并自动求导
    return eq;
end proc:
eqs["(4+1)Fokas-T"]:=twtransform(eqs["(4+1)Fokas"],xi=m[1]*x+m[2]*y);
eqs["(4+1)Fokas-T-2"]:=twtransform(eqs["(4+1)Fokas-T"],eta=m[3]*z+m[4]*w);
