function [tfun] = mt_deconv(dt,x,y,tbp,kspec)

% 
% Multitaper deconvolution
%

nfft = length(x);

% Get DPSS

[vn,lambda] = dpss(nfft,tbp,kspec);

% Demean

%x = x - mean(x);
%y = y - mean(y);

% Complex eigenspectra 

for i = 1:kspec

   junk1 = [x.*vn(:,i) ; zeros(nfft-1,1)];
   junk2 = [y.*vn(:,i) ; zeros(nfft-1,1)];
   yk_i(:,i) = fft(junk1);
   yk_j(:,i) = fft(junk2);

end

nfft2 = length(yk_i);

% For now, assume unit weights
% *********
% TO DO
% *********

dyk_i = yk_i;
dyk_j = yk_j;

% Force zero mean process
%
%for i = 1:kspec
%   dyk_i(:,i) = dyk_i(:,i) - sum(real(dyk_i(:,i)))/(nfft);
%   dyk_j(:,i) = dyk_j(:,i) - sum(real(dyk_j(:,i)))/(nfft);
%end

% Calculate power spectra

si = sum(abs(dyk_i).^2, 2); 
sj = sum(abs(dyk_j).^2, 2);

% eps = 0.0001 * sum(sj)/real(nfft)

% Initialize xspec

xspec(1:nfft2) = 0;

for i = 1:nfft2
   
   % Deconvolution
   
   xspec(i) = sum ( dyk_i(i,:) .* conj(dyk_j(i,:)) );  
        
   xspec(i) = xspec(i) / (sj(i));
%      xspec(i) = xspec(i) / (sj(i)+eps)
 
end

% Zero mean process
%
%xspec = xspec - sum(real(xspec))/nfft;
%

% Sym fft
% *******
% TO DO
% *******

% Return to time domain

tfun = ifft(xspec);

return






