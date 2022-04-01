clc;clear;close all;format longeng;format compact;rand('state',sum(100*clock));

id=4;
Max_Runs=1;
upper_limit=100;
lower_limit=-100;
members=20;
dim=30;

Max_FES=300000;
Function_error_values=zeros(11,Max_Runs);
Data_record=zeros(1,Max_Runs);
Results_to_record=zeros(1,5);
if id<15
    error_value=100*(id-1)-1400;
else
    error_value=100*(id-1)-1300;
end

for Runs=1:Max_Runs
    lv=zeros(members,dim);
    lp=(upper_limit-lower_limit).*rand(members,dim)+lower_limit;
    lgp_id=1;
    lp_fv=zeros(1,members);
    for i=1:members
        lp_fv(i)=Benchmark_30D(id,dim,lp(i,:)');
        if lp_fv(i)<lp_fv(lgp_id)
            lgp_id=i;
        end
    end
    lpp=lp;
    lpp_fv=lp_fv;
    
    
    
    wc=zeros(1,members);
    cc=zeros(1,members);
    sm=zeros(1,members);
    parameter=xlsread('Parameter.xlsx');
    for i=1:members
        wc(i)=parameter(ceil(30000*rand),1);
        cc(i)=parameter(ceil(30000*rand),2);
        sm(i)=parameter(ceil(30000*rand),3);
    end
    
    temp_id=ceil(members*rand(1,members));
    Pm=7;
    m=zeros(1,members);
    
    
    for FES=1:Max_FES
        for i=1:members
            if m(i)<Pm
                temp_id(i)=ceil(members*rand);
                while temp_id(i)==lgp_id
                    temp_id(i)=ceil(members*rand);
                end
            else
                m(i)=0;
                temp_id(i)=lgp_id;
            end
            lv(i,:)=wc(i)*rand(1,dim).*lv(i,:)+cc(i)*rand(1,dim).*(lpp(temp_id(i),:)-lp(i,:));
            if rand<sm(i)
                lp(i,ceil(dim*rand))=(upper_limit-lower_limit)*rand+lower_limit;
            end
            lp(i,:)=lp(i,:)+lv(i,:);
            update_lpp=1;
            for j=1:dim
                if lp(i,j)<lower_limit || lp(i,j)>upper_limit
                    update_lpp=0;
                    break;
                end
            end
            if update_lpp==1
                lp_fv(i)=Benchmark_30D(id,dim,lp(i,:)');
                if lp_fv(i)<lpp_fv(temp_id(i))
                    if lp_fv(i)<lpp_fv(lgp_id)
                        lgp_id=temp_id(i);
                    end
                    lpp_fv(temp_id(i))=lp_fv(i);
                    lpp(temp_id(i),:)=lp(i,:);
                elseif lp_fv(i)<lpp_fv(i)
                    lpp_fv(i)=lp_fv(i);
                    lpp(i,:)=lp(i,:);
                else
                    m(i)=m(i)+1;
                end
            end
        end
        gbest_position_fitness_value=lpp_fv(lgp_id);
        gbest_position=lpp(lgp_id,:)';
        
        if FES==round(0.01*Max_FES)
            if gbest_position_fitness_value-error_value<10^-8
                Function_error_values(1,Runs)=0;
            else
                Function_error_values(1,Runs)=gbest_position_fitness_value-error_value;
            end
        end
        for i=1:10
            if FES==round(0.1*i*Max_FES)
                if gbest_position_fitness_value-error_value<10^-8
                    Function_error_values(i+1,Runs)=0;
                else
                    Function_error_values(i+1,Runs)=gbest_position_fitness_value-error_value;
                end
            end
        end
        if mod(FES,100)==0
            gbest_position
            lpp_fv'
            gbest_position_fitness_value-error_value
            FES
            Runs
            id
        end
        if gbest_position_fitness_value-error_value<10^-8
            break;
        end
    end
    if gbest_position_fitness_value-error_value<10^-8
        Data_record(Runs)=0;
    else
        Data_record(Runs)=gbest_position_fitness_value-error_value;
    end
end
Data_record=sort(Data_record);
Results_to_record(1)=Data_record(1);
Results_to_record(2)=Data_record(Max_Runs);
Results_to_record(3)=Data_record(ceil(Max_Runs/2));
Results_to_record(4)=sum(Data_record)/Max_Runs;
Results_to_record(5)=sqrt(sum(Data_record.^2)/Max_Runs);

xlswrite('v8612.xls',Function_error_values,['f' int2str(id)]);
fid = fopen(['f' int2str(id) '.csv'], 'a');
fprintf(fid, '%f,%f,%f,%f,%f\n', Results_to_record(1), Results_to_record(2), Results_to_record(3), Results_to_record(4), Results_to_record(5));
fclose(fid);







