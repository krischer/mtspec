function [freq,spec,ds,dds] = mtspec(dt,x,tbp,kspec,method)

%
% Function mtspec to calculate the multitaper spectrum of signal x.
%
% Based on the F90 Multitaper library by Prieto et al. (2008)
%
% [freq,spec,ds,dds] = mtspec(dt,x,tbp,kspec,method)
% 
% INPUTS
%
% dt		sampling rate
% x		time series (single vector only allowed)
% tbp		the time-bandwidth product
% kspec		number of tapers to use
%
% method	optional (default - 0)
% 		0 - For standard adaptive multitaper
% 		1 - Quadratic multitaper, with additional
% 		    derivative estimates (ds, dds)
%
% OUTPUTS
%
% freq		Frequency vector
% spec		spectral estimate
% ds		estimate of first derivative (QI method)
% dds		estimate of second derivative (QI method)
%
%
% German A. Prieto
% June 10, 2008
%

if (nargin == 4)
   method = 0;
elseif (nargin > 5)
   error('Too many input arguments')
end

if (nargout == 2)
   ds = 0;
   dds = 0;
elseif (nargout == 3)
   dds = 0;
elseif (nargout > 4)
   error('Too many output arguments')
end

if (method==0)
   ds = 0;
   dds = 0;
elseif (method == 1)
   disp('Quadratic method requested')
else
   error('method has to be 0 or 1')
end

npts = length(x);

nfft = npts;

if (nfft < 2) 
   error(1,'Number of points in x is too short')
end

if (isreal(x))
   if (mod(npts,2)==0)
      nf = nfft/2 + 1;
   else
      nf = (nfft+1)/2;
   end
   fnyq	= 0.5/dt;
   df 	= fnyq/(nf-1);

   freq = [0:df:fnyq]';

else
   nf = nfft;
   fnyq	= 1./dt;
   df 	= fnyq/(nf);

   if (mod(npts,2)==0)
      freq = [0:df:fnyq]';
   else
      freq = [0:df:fnyq-df]';
   end
 
   freq = [0:df:fnyq-df]';

end

% Define frequency bins 
% !!!!!!! TO DO !!!!!!!


xmean = mean(x);
xvar  = var(x);

x2 = detrend(x,'constant');

% Get DPSS

[vn,lambda] = dpss(nfft,tbp,kspec);

% Get eigenspectra and Sbar

sbar = zeros(nf,1);
for i = 1:kspec

   xtaper = x2.*vn(:,i);
   yk(:,i) = fft(xtaper);
   sk(:,i) = abs(yk(1:nf,i)).^2;

   sbar = sbar + (1./lambda(i)) * sk(:,i);

end

% Two times power for one sided spectra

sbar = (2.d0/kspec)*sbar;

% The F-test and reshaping
%
% !!!!!!! TO DO !!!!!!!
%
%

% Adaptive weighted spectrum

[spec,se,wt] = adaptspec(nfft,nf,kspec,lambda,yk);

% Jackknife spectrum
%
% !!!!!!!!! TO DO !!!!!!!!!
%
%

% The Quadratic Inverse problem

if (method==1)

   [spec,ds,dds] = qiinv(nfft,nf,tbp,kspec,lambda,vn,yk,wt,spec);

end

%  double power in positive frequencies

if (isreal(x))

   spec(2:nf-1) = 2.d0 * spec(2:nf-1);

end

sscal = (spec(1) + spec(nf));
for i=2: nf-1
   sscal=sscal + spec(i);
end
sscal = xvar/(sscal*df);

spec = sscal * spec;

return

%--------------------------------------------------------------------
%
%--------------------------------------------------------------------

%
% Additional functions
%

%--------------------------------------------------------------------
% Adaptspec
%--------------------------------------------------------------------

function [spec,se,wt] = adaptspec(nfft,nf,kspec,lambda,yk);

%
% Function to calculate the adaptive multitaper spectrum. 
%

% Initialize some variables

df = 1./(nfft-1);	% Assume unit sampling

for k = 1:kspec
   sk(:,k) = abs(yk(1:nf,k)).^2;
end


if (nfft == nf) 	% Complex multitaper
   varsk = sum(sk,1)*df;
else
   varsk = (sk(1,:) + sk(nf,:) + 2.*sum(sk(2:nf-1,:),1) ) * df;
end

dvar   = sum(varsk)/(kspec);

evalu  = lambda;
evalu1 = 1.-lambda;
bk     = dvar  * evalu1;
sqev   = sqrt(evalu);

% Iterative procedure

rerr = 9.5e-7;

sbar = (sk(:,1)+sk(:,2))/2;

for j = 1:1000

   slast = sbar;

   for k = 1:kspec
      wt(:,k) = sqev(k)*sbar ./( evalu(k)*sbar + bk(k));
      wt(:,k) = min(wt(:,k),1.0);
      skw(:,k) = wt(:,k).^2 .* sk(:,k);   
   end

   sbar = sum(skw,2) ./ sum(wt.^2, 2);
	    
   if (j==1000) then
      spec = sbar;
      disp(['adaptspec did not converge, rerr = ',  ...
	      num2str(max(abs((sbar-slast)/(sbar+slast)))),num2str(rerr)])
      continue
   end
   if (max(abs((sbar-slast)/(sbar+slast))) > rerr)
      continue
   end
            
   spec = sbar;
   break
	 
end 

for i = 1:nf
   wt_dofs(i,:) = wt(i,:)./sqrt(sum(wt(i,:).^2)/kspec);
end

wt_dofs = min(wt_dofs,1.0);

se = 2.0 * sum(wt_dofs.^2, 2); 

return

% End function adaptspec

%--------------------------------------------------------------------
% QIINV The quadratic Inverse spectrum
%--------------------------------------------------------------------

function [qispec,ds,dds] = qiinv(nfft,nf,tbp,kspec,lambda,vn,yk,wt,spec)

%
% Function to calculate the Quadratic Spectrum using the method 
% developed by Prieto et al. (2007). 
% The first 2 derivatives of the spectrum are estimated and the 
% bias associated with curvature (2nd derivative) is reduced. 
%

if (min(lambda) < 0.9) 
      disp(['Careful, Poor leakage of eigenvalue ', ...
		 num2str(min(lambda))]);
      disp('Value of kspec is too large, revise? *****') 
end

% New frequency sampling in inner bandwidth (-W,W)

%  New inner bandwidth frequency

nxi = 79;

bp = tbp/nfft;		% W bandwidth

dxi = (2.0*bp)/(nxi-1);	% QI freq. sampling

xi = [-bp:dxi:bp];

nfft = nfft + 10*nfft;

if (mod(nfft,2)==0)
   fsamp = [-nfft/2:nfft/2-1]'/(nfft);
else
   fsamp = [-(nfft-1)/2:(nfft-1)/2]'/(nfft-1);
end

for k = 1:kspec
   xk(:,k) = wt(1:nf,k).*yk(1:nf,k);
   Vk(:,k) = fftshift(fft(vn(:,k),nfft));
end

for i = 1:kspec

   Vj1 = interp1(fsamp,real(Vk(:,i)),xi,'cubic');  
   Vj2 = interp1(fsamp,imag(Vk(:,i)),xi,'cubic');  

   Vj(:,i) = 1.0/sqrt(lambda(i)) * complex(Vj1,Vj2);

end

% 
% Create the vectorized Cjk matrix and Pjk matrix { Vj Vk* }
%

L = kspec*kspec;

m = 0;
for j = 1:kspec
   for k = 1:kspec

      m = m + 1;
      C(m,:) = ( conj(xk(:,j)) .* (xk(:,k)) );

      Pk(m,1:nxi) = conj(Vj(:,j)) .* (Vj(:,k));

   end
end

Pk(1:m,1)         = 0.5 * Pk(1:m,1);
Pk(1:m,nxi)       = 0.5 * Pk(1:m,nxi);

%  I use the Chebyshev Polynomial as the expansion basis.

hcte(1:nxi)  = 1.0; 
hk(:,1) = Pk*hcte' * dxi;

hslope(1:nxi) = xi/bp; 
hk(:,2) = Pk*hslope' * dxi; 

hquad(1:nxi) = (2.*((xi/bp).^2) - 1.0);
hk(:,3) = Pk*hquad' * dxi;

hm1 = reshape(hk(:,1),kspec,kspec); 
hm2 = reshape(hk(:,2),kspec,kspec); 
hm3 = reshape(hk(:,3),kspec,kspec); 

if (m ~= L) then 
      error('Error in matrix sizes, stopped ')
end 
n = nxi;
nh = 3;

%
% Begin Least squares solution (QR factorization)
%

[Q,R] = qr(hk);

% Covariance estimate                                                  
ri = R\eye(L);                                                  
covb = real(ri*ri');                                                
   
for i = 1:nf

   btilde = Q' * C(:,i);

   hmodel = R \ btilde;

   cte(i)   = real(hmodel(1));
   slope(i) = -real(hmodel(2));
   quad(i)  = real(hmodel(3));

   sigma2(i) = sum(abs( C(:,i) - hk*real(hmodel) ).^2)/(L-nh) ;

   cte_var(i)   = sigma2(i)*covb(1,1);
   slope_var(i) = sigma2(i)*covb(2,2);
   quad_var(i)  = sigma2(i)*covb(3,3);

end

slope = slope / (bp);
quad  = quad  / (bp.^2);

slope_var = slope_var / (bp.^2);
quad_var = quad_var / (bp.^4);

%  Compute the Quadratic Multitaper
%  Eq. 33 and 34 of Prieto et. al. (2007)

for i = 1:nf

   qicorr = (quad(i).^2)/((quad(i).^2) + quad_var(i) ); 
   qicorr = qicorr * (1/6)*(bp.^2)*quad(i);

   qispec(i) = spec(i) - qicorr;

end

ds  = slope';
dds = quad';

%figure(2)
%ip = [1 4 6];
%subplot(2,1,1)
%plot(xi,real(Vj(:,ip)),'k')
%xlim([-bp bp])
%subplot(2,1,2)
%plot(xi,imag(Vj(:,ip)),'k')
%xlim([-bp bp])

%figure(3)
%plot(xi,hcte)
%hold on
%plot(xi,hslope)
%plot(xi,hquad)
%hold off

%figure(4)
%colormap gray
%subplot(2,3,1)
%pcolor(-abs(real(hm1)))
%caxis([-1 0])
%subplot(2,3,2)
%pcolor(-abs(real(hm2)))
%caxis([-1 0])
%subplot(2,3,3)
%pcolor(-abs(real(hm3)))
%caxis([-1 0])
%subplot(2,3,4)
%pcolor(-abs(imag(hm1)))
%caxis([-1 0])
%subplot(2,3,5)
%pcolor(-abs(imag(hm2)))
%caxis([-1 0])
%subplot(2,3,6)
%pcolor(-abs(imag(hm3)))
%caxis([-1 0])

%figure(5)
%subplot(3,1,1)
%plot(real(cte))
%subplot(3,1,2)
%plot(real(slope))
%subplot(3,1,3)
%plot(real(quad))

return

