%
% Matlab routine to retrieve the results from the multitaper
% analysis and plot figures, as presented in the literature
%

close all
clear

load d18O_1.dat
load d18O_2.dat

figure(1)
semilogy(d18O_1(:,1),d18O_1(:,2));
hold on
semilogy(d18O_1(:,1),d18O_1(:,3),'--');
semilogy(d18O_1(:,1),d18O_1(:,4),'--');
hold off

figure(2)
semilogy(d18O_2(:,1),d18O_2(:,2),'r');

figure(3)
plot(d18O_1(:,1),d18O_2(:,5));
axis tight

clear

load noise_st.dat
load noise_mt.dat
load noise_qi.dat
load noise_sin.dat

figure(4)
subplot(2,2,1)
loglog(noise_st(:,1),noise_st(:,2))
subplot(2,2,2)
loglog(noise_mt(:,1),noise_mt(:,2))
subplot(2,2,3)
loglog(noise_qi(:,1),noise_qi(:,2))
subplot(2,2,4)
loglog(noise_sin(:,1),noise_sin(:,2))

clear
load mt_cohe.dat
load sin_cohe.dat
load mt_trfun.dat

nsmooth = 160;

freq = mt_trfun(2:end,1)';
rtrf = mt_trfun(2:end,3)';
itrf = mt_trfun(2:end,4)';
cohe = mt_trfun(2:end,2)';

tfunction = rtrf + i*itrf;

nf = length(freq);

% Band averaging around values of interest
% Similar frequencies to Constable and Constable (2004) 

per = 1./freq;

lper = log10(per);

avper(1)  = 21330.;
avper(2)  = 41410.;
avper(3)  = 74400.; 
avper(4)  = 185100.; 
avper(5)  = 348000.;
avper(6)  = 697800.;
avper(7)  = 1428000.;
avper(8)  = 2674000.;
avper(9)  = 4593000.; 
avper(10) = 11810000.;

avper = log10(avper);

for i = 1:nf
   if (cohe(i) >= 0.6)
      wt(i)   = 1./sqrt(1. - cohe(i));	% Coherence weighting
   else
      wt(i) = 0.;
   end
end

for i = 1:length(avper)

   iloc = find(lper<=avper(i)+0.1 & lper>=avper(i)-0.1);


   tfavg(i) = sum(tfunction(iloc).*wt(iloc));
   wt_avg    = sum(wt(iloc));

   tfavg(i) = tfavg(i)/wt_avg;

end

c = 6378. * (1. - 2.*tfavg) ./ (2.*(1.+tfavg)); 

figure(5)
subplot(2,2,1)
plot(mt_cohe(:,1),mt_cohe(:,4))
subplot(2,2,2)
plot(mt_cohe(:,1),mt_cohe(:,5))
subplot(2,2,3)
plot(sin_cohe(:,1),sin_cohe(:,4))
subplot(2,2,4)
plot(sin_cohe(:,1),sin_cohe(:,5))

figure(6)
plot(avper,real(c));
hold on
plot(avper,imag(c),'r');
hold off
xlim([4 7.5])
ylim([-750 1500])

clear

load noise_deconv.dat
figure(7)
plot(noise_deconv(:,1),noise_deconv(end:-1:1,2))
xlim([-86400 -85900])



