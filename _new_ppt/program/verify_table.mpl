# timestamp: 2019-05-31 15:55:35
# git  hash: 148e982
all_subset:=proc(n::nonnegint)
    local tmp,t;
    if n=0 then
        return {};
    end if;
    tmp:=thisproc(n-1);
    return tmp,map(t->t union {n},[tmp])[];
end proc:

# 对 (n+1) 维方程返回 n
eqn_dim:=proc(eq)
    local _,x,eq_process;
# 公用方程预处理函数
# 返回: 方程, 未知函数, 空间变量的向量x, 时间变量t
eq_process:=proc(_eq)
    local eq,u,n,x,t,vars,assert;
assert:=proc(cond)
    if not cond then
        error(_rest);
    end if;
end proc:
    # 原方程处理
    assert(indets(_eq,specfunc({int,Int}))={},"equation cannot contain integral. please do transform first.");
    eq:=_eq;
    if type(eq,equation) then 
        eq:=lhs(eq)-rhs(eq);
    end if;
    eq:=numer(eq);
    # 未知函数提取
    u:=indets(eq,function);
    u:=remove(e->type(op(0,e),procedure),u);
    assert(numelems(u)=1,"only one unkown function admited, but receieved %1",u);
    u:=u[];
    # 变量绑定
    n:=nops(u)-1;
    vars:=[op(u)];
    x:=vars[1..n];
    t:=vars[n+1];
    return eq,u,x,t;
end proc:
    _,_,x,_:=eq_process(eq);
    return numelems(x);
end proc:

# 使用局部变量防止与方程系数冲突
get_pl:=proc(n)
    local delta,k,p,q,r,c,ps;
    ps:=[k,p,q,r];
    return [delta,ps[1..n][],c];
end proc:

set_order:=[all_subset(3)][1..-1];
eqn_order:=[
    "(1+1)KdV",
    "(2+1)BKP-T",
    "(2+1)KP",
    "(2+1)SK",
    "(4+1)Fokas-T-2",
    "(2+1)CBS",
    "(2+1)CBS-G",
    "(2+1)BLMP",
    "(3+1)BKP",
    "(3+1)KP",
    "(3+1)JM",
    "(3+1)NEE-T",
    "(3+1)SW",
    "(4+1)Fokas-T",
    "(3+1)YTSF",
    "(3+1)CBS",
    "(3+1)KP-Bq"
];

# 去根号, 更快
eqs["(3+1)KP-Bq"]:=subs(u(x,y,z,t)=u(x,z,t,y),eqs["(3+1)KP-Bq"]);

verify_eqns:=proc(eqn_order,set_order)
    local n_eq,n_dv,TL,TS,TE,err_code,NE,i,eq,n,j,sh,st;
    n_eq:=numelems(eqn_order);
    n_dv:=numelems(set_order);
    TS:=Array(1..n_eq,1..n_dv,fill=FAIL);
    TL:=Array(1..n_eq,1..n_dv,fill=FAIL);
    TE:=table();NE:=0;
    err_code:=proc(msg::string)
        if not assigned(TE[msg]) then
            NE:=NE+1;
            TE[msg]:=NE;
        end if;
        return TE[msg];
    end proc:
    for i from 1 to n_eq do
        eq:=get_eq(eqn_order[i]);
        n:=eqn_dim(eq);
        for j from 1 to 2^n do
            st:=time[real]();
            printf("%s | ",eqn_order[i]);
            printf("%a | ",set_order[j]);
            sh:=hirota_ex(eq,set_order[j],get_pl(n),'quiet');
            try
                TS[i,j]:=sh:-verify_sol(0,0,3,'rnd_assign','quiet');
            catch:
                TS[i,j]:=err_code(sh["err_msg"]);
            end try;
            printf("%a | ",TS[i,j]);
            try
                TL[i,j]:=sh:-verify_sol(2,0,0,'rnd_assign','quiet');
            catch:
                TL[i,j]:=err_code(sh["err_msg"]);
            end try;
            printf("%a | ",TL[i,j]);
            printf("%.3f \n",time[real]()-st);
            print();
        end do;
    end do;
    return TS,TL,TE;
end proc:

print_elem:=proc(e)
    if e=true then
        return "\\V";
    elif e=false then
        return "\\X";
    elif type(e,set(nonnegint)) then
        return cat("",e[]);
    elif type(e,posint) then
        return sprintf("\\mb{%d}",e);
    elif e=FAIL then
        return "";
    else
        error("unkown input %1 .",e);
    end if;
end proc:

print_table:=proc(eqn_order,set_order,TS,TL,TE)
    local n_eq,n_dv,i,j,RTE,k;
    n_eq:=numelems(eqn_order);
    n_dv:=numelems(set_order);
    printf("\\multicolumn{1}{c}{Eqn}");
    for j from 1 to n_dv do
        printf(" & %s",print_elem(set_order[j]));
    end do;
    printf(" \\\\ \n\n");

    for i from 1 to n_eq do
        printf("%s",eqn_order[i]);
        for j from 1 to n_dv do
            printf(
                " & %s%s",
                print_elem(TS[i,j]),
                print_elem(TL[i,j])
            );
        end do;
        printf(" \\\\\n");
    end do;

    printf("\n\n\n");
    RTE:=table(map(e->rhs(e)=lhs(e),[indices(TE,'pairs')]));
    for k from 1 to numelems(RTE) do 
        printf("$E_%d$: %s\n",k,RTE[k]);
    end do;
end proc:
