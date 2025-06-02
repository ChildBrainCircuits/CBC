function [ varargout ] = wfpt_wrapper(t,v,a,z,err)


for tt = 1:size(t,1)
    for tw = 1:size(t,2)
       [p(tt,tw),k(tt,tw),s(tt,tw)] = wfpt_k(t(tt,tw), v(tt,tw),a(tt,tw),z(tt,tw),err);
    end
end

varargout{1} = p;
if nargout == 3
    varargout{2} = k;
    varargout{3} = s;
end

end

